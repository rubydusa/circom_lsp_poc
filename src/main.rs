use ropey::Rope;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use codespan_reporting::diagnostic::{Severity, LabelStyle};
use circom_structure::error_definition::Report;
use circom_structure::file_definition::FileLibrary;

use std::fmt;
use std::sync::Mutex;
use std::cell::RefCell;
use std::collections::HashMap;

enum FileLibrarySource {
    ProgramArchive(ProgramArchive),
    FileLibrary(FileLibrary)
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: Option<i32>,
}

// circom's program archive doesn't implement debug
struct ProgramArchive {
    inner: circom_structure::program_archive::ProgramArchive
}
impl ProgramArchive {
    pub fn new(inner: circom_structure::program_archive::ProgramArchive) -> ProgramArchive {
        ProgramArchive {
            inner
        }
    }
}

impl fmt::Debug for ProgramArchive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ProgramArchive")
         .finish()
    }
}

#[derive(Debug)]
struct DocumentData {
    content: Rope,
    archive: Option<ProgramArchive>,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: Mutex<RefCell<HashMap<Url, DocumentData>>>,
}

impl Backend {
    fn new(client: Client) -> Backend { 
        Backend { 
            client,
            document_map: Mutex::new(RefCell::new(HashMap::new())),
        }
    }

    async fn on_change(&self, params: TextDocumentItem, publish_diagnostics: bool) {
        let path = uri_to_string(&params.uri);
        let rope = Rope::from_str(&params.text);

        // do not compute archive if publish_diagnostics flag is not set, this
        // is done to prevent from tons of calls to the circom compiler
        //
        // also, as of now circom's parser function doesn't seperate the file library creation
        // logic from the parseing, so it's impossible to run the parser on an intermediate buffer
        let archive = if publish_diagnostics {
            let (reports, file_library_source) = match circom_parser::run_parser(
                path, 
                "2.1.5",  // TODO: figure what this version number actually does
                vec![],  // TODO: add linked library support
            ) {
                Ok((mut archive, mut reports)) => {
                    let mut type_reports = match circom_type_checker::check_types::check_types(&mut archive) {
                        Ok(type_reports) => type_reports,
                        Err(type_reports) => type_reports
                    };
                    reports.append(&mut type_reports);
                    (reports, FileLibrarySource::ProgramArchive(ProgramArchive::new(archive)))
                },
                Err((file_library, reports)) => (reports, FileLibrarySource::FileLibrary(file_library))
            };

            let file_library = match &file_library_source {
                FileLibrarySource::ProgramArchive(archive) => &archive.inner.file_library,
                FileLibrarySource::FileLibrary(file_library) => &file_library
            };

            let diagnostics: Vec<_> = reports
                .into_iter()
                .map(|x| Self::report_to_diagnostic(x, &file_library, &params.uri))
                .collect();

            let mut main_file_diags = Vec::new();
            let mut other_files_diags: HashMap<_, Vec<_>> = HashMap::new();

            for (diagnostic, uri) in diagnostics {
                if uri == params.uri {
                    main_file_diags.push(diagnostic);
                } else if other_files_diags.contains_key(&uri) {
                    other_files_diags.get_mut(&uri).expect("other_files_diag should contain uri").push(diagnostic);
                } else {
                    other_files_diags.insert(uri, vec![diagnostic]);
                }
            }

            if let Some(other_files_error) = Self::other_files_diagnostic(&other_files_diags) {
                main_file_diags.push(other_files_error);
            };

            // publish main errors
            self.client
                .publish_diagnostics(params.uri.clone(), main_file_diags, params.version)
                .await;

            // publish errors in other files
            for (uri, diags) in other_files_diags {
                self.client
                    .publish_diagnostics(uri, diags, params.version)
                    .await;
            }

            match file_library_source {
                FileLibrarySource::ProgramArchive(x) => Some(x),
                _ => None
            }
        } else {
            None
        };

        let document_map = self.document_map.lock().unwrap();
        // if new archive computed succesfully, insert it.
        // otherwise, use old archive
        let archive = match archive {
            Some(new_archive) => Some(new_archive),
            None => match document_map.borrow_mut().remove(&params.uri).map(|x| x.archive) {
                Some(existing_archive) => existing_archive,
                None => None
            }
        };

        let document = DocumentData {
            content: rope,
            archive
        };

        document_map.borrow_mut().insert(params.uri, document);
    }

    // file_library is needed to decide in what file does the report occurs
    fn report_to_diagnostic(report: Report, file_library: &FileLibrary, main_uri: &Url) -> (Diagnostic, Url) {
        let diagnostic = report.to_diagnostic();

        let label = diagnostic.labels.into_iter().reduce(|cur, a| {
            match (cur.style, a.style) {
                (LabelStyle::Primary, LabelStyle::Secondary) => cur,
                (LabelStyle::Secondary, LabelStyle::Primary) => a,
                _ => if cur.range.start < a.range.start {
                    cur
                } else {
                    a
                }
            }
        });

        let (url, range) = match label {
            Some(label) => { 
                let simple_file = file_library
                    .to_storage()
                    .get(label.file_id)
                    .expect("invalid file_id from label");

                let uri = string_to_uri(simple_file.name());
                let rope = Rope::from_str(simple_file.source());

                (uri, Range {
                    start: char_to_position(&rope, label.range.start).expect("valid label range start"),
                    end: char_to_position(&rope, label.range.end).expect("valid label range end")
                })
            },
            None => (main_uri.clone(), Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 0 }
            })
        };

        let severity = match diagnostic.severity {
            Severity::Bug | Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            Severity::Help => DiagnosticSeverity::HINT,
            Severity::Note => DiagnosticSeverity::INFORMATION
        };

        let message = diagnostic.message;

        (Diagnostic {
            range,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: Some(String::from("circom_lsp")),
            message,
            related_information: None,
            tags: None,
            data: None
        }, url)
    }

    fn other_files_diagnostic(other_files_diags: &HashMap<Url, Vec<Diagnostic>>) -> Option<Diagnostic> {
        let locations: String = other_files_diags
            .keys()
            .map(|uri| {
                format!("\n{}", uri_to_string(uri))
            })
            .collect();

        if locations.is_empty() {
            None
        } else {
            Some(Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0
                    },
                    end: Position {
                        line: 0,
                        character: 0
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some(String::from("circom_lsp")),
                message: format!("errors found in the following files:{}", locations),
                related_information: None,
                tags: None,
                data: None
            })
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "Opened!")
            .await;

        self.on_change(
            TextDocumentItem { 
                uri: params.text_document.uri,
                text: params.text_document.text,
                version: Some(params.text_document.version)
            },
            true
        ).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(
            TextDocumentItem {
                uri: params.text_document.uri,
                text: params.content_changes[0].text.clone(),
                version: Some(params.text_document.version)
            },
            false
        ).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let result = {
            let document_map = self.document_map.lock().unwrap();
            let document_map = document_map.borrow();
            document_map.get(&params.text_document.uri).map(|x| x.content.to_string())
        };

        match result {
            Some(document) => {
                self.on_change(
                    TextDocumentItem {
                        uri: params.text_document.uri,
                        text: document,
                        version: None
                    },
                    true
                ).await;
            },
            None => ()
        };
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let document_map = self.document_map.lock().unwrap();
        let document_map = document_map.borrow();
        let document_data = document_map.get(&uri).expect("document map should have uri on hover");

        let Ok(Some((start, word))) = find_word(&document_data.content, params.text_document_position_params.position) else {
            // return Ok(Some(simple_hover(String::from("could not find word"))))
            return Ok(None);
        };

        let Some(archive) = &document_data.archive else {
            return Ok(Some(simple_hover(String::from("Could not find information (are there any compilation errors?)"))))
        };

        if let Some(_template_data) = archive.inner.templates.keys().find(|x| x == &&word) {
            Ok(Some(simple_hover(String::from("template"))))
        } else if let Some(_function_data) = archive.inner.functions.keys().find(|x| x == &&word) {
            Ok(Some(simple_hover(String::from("function"))))
        } else {
            Ok(Some(simple_hover(String::from(word))))
        }
    }
}

fn find_word(rope: &Rope, position: Position) -> ropey::Result<Option<(usize, String)>> {
    let char_idx = position_to_char(rope, position)?;
    let char = rope.get_char(char_idx)
        .expect("char_idx should not be out of range since position_to_char guarantees");

    if char.is_alphanumeric() {
        let start = 'start: loop {
            let mut i = char_idx;
            for c in rope.chars_at(char_idx).reversed() {
                if !c.is_alphanumeric() {
                    break 'start i;
                }
                i -= 1;
            }
            break i;
        };
        let end = 'end: loop {
            let mut i = char_idx;
            for c in rope.chars_at(char_idx) {
                if !c.is_alphanumeric() {
                    break 'end i;
                }
                i += 1;
            }
            break i;
        };

        Ok(Some((start, rope.slice(start..end).to_string())))
    } else {
        Ok(None)
    }
}

fn position_to_char(rope: &Rope, position: Position) -> ropey::Result<usize> {
    let line_start = rope.try_line_to_char(usize::try_from(position.line).unwrap())?;
    let char = line_start + usize::try_from(position.character).unwrap();

    // ensure resulting character is in bounds
    rope.try_char_to_byte(char)?;
    Ok(char)
}

fn char_to_position(rope: &Rope, idx: usize) -> ropey::Result<Position> {
    let line = rope.try_char_to_line(idx)?;
    let line_start = rope.line_to_char(line);
    let character = idx - line_start;

    let line = u32::try_from(line).unwrap();
    let character = u32::try_from(character).unwrap();

    Ok(Position {
        line,
        character
    })
}

fn uri_to_string(uri: &Url) -> String {
    uri
        .to_file_path()
        .expect("Invalid text document URI")
        .into_os_string()
        .into_string()
        .expect("Invalid text document URI")
}

fn string_to_uri(s: &str) -> Url {
    // strip first and last chars because circom is stupid
    let fixed = {
        let mut chars = s.chars();
        chars.next();
        chars.next_back();

        chars.as_str()
    };

    Url::from_file_path(fixed).expect("string is valid uri")
}

fn simple_hover(message: String) -> Hover {
    return Hover {
        contents: HoverContents::Scalar(MarkedString::String(message)),
        range: None
    };
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    
    let (service, socket) = LspService::new(|client| Backend::new(client));

    Server::new(stdin, stdout, socket).serve(service).await;
}
