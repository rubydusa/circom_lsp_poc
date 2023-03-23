use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use codespan_reporting::diagnostic::{Severity, LabelStyle};
use circom_structure::error_definition::Report;

use std::path::PathBuf;
use std::collections::HashMap;
use std::cell::RefCell;
use std::sync::Mutex;

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: Option<i32>,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: Mutex<RefCell<HashMap<Url, Rope>>>,
}

impl Backend {
    fn new(client: Client) -> Backend { 
        Backend { 
            client,
            document_map: Mutex::new(RefCell::new(HashMap::new()))
        }
    }

    async fn debug(&self, message: &str) {
        self.client
            .log_message(MessageType::ERROR, message)
            .await;
    }

    async fn on_change(&self, params: TextDocumentItem, publish_diagnostics: bool) {
        self.debug("on_change").await;
        let path = url_to_string(&params.uri);
        let rope = Rope::from_str(&params.text);

        if publish_diagnostics {
            let diagnostics: Vec<_> = {
                self.debug("computing cache").await;
                let cache = {
                    let document_map = self.document_map.lock().unwrap();
                    let document_map = document_map.borrow();
                    document_map_to_cache(&document_map)
                };
                // let cache = HashMap::new();

                self.debug("computing reports").await;
                let reports = match circom_parser::run_parser_cached(
                    path, 
                    "2.1.4", 
                    vec![], 
                    cache, 
                    true
                ) {
                    Ok((mut archive, mut reports)) => {
                        let mut type_reports = match circom_type_checker::check_types::check_types(&mut archive) {
                            Ok(type_reports) => type_reports,
                            Err(type_reports) => type_reports
                        };
                        reports.append(&mut type_reports);
                        reports
                    },
                    Err((_, reports)) => reports
                };

                self.debug("convert reports to diagnostics").await;
                reports
                    .into_iter()
                    .map(|x| Self::report_to_diagnostic(&rope, x))
                    .collect()
            };

            self.debug(&format!("{:?}", diagnostics)).await;

            self.client
                .publish_diagnostics(params.uri.clone(), diagnostics, params.version)
                .await;

            self.debug("doesn't reach as far as I understand").await;
        }

        self.debug("document insertion").await;
        let document_map = self.document_map.lock().unwrap();
        document_map.borrow_mut().insert(params.uri, rope);
    }

    // convert circom report to a lsp diagnostic
    fn report_to_diagnostic(rope: &Rope, report: Report) -> Diagnostic {
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

        let range = match label {
            Some(label) => Range {
                start: char_to_position(rope, label.range.start),
                end: char_to_position(rope, label.range.end)
            },
            None => Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 0 }
            }
        };

        let severity = match diagnostic.severity {
            Severity::Bug | Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
            Severity::Help => DiagnosticSeverity::HINT,
            Severity::Note => DiagnosticSeverity::INFORMATION
        };

        let message = diagnostic.message;

        Diagnostic {
            range,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: Some(String::from("circom_lsp")),
            message,
            related_information: None,
            tags: None,
            data: None
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
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

    async fn shutdown(&self) -> Result<()> {
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
            false
        ).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.debug("did change").await;
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
        self.debug("did save").await;
        let result = {
            let document_map = self.document_map.lock().unwrap();
            let document_map = document_map.borrow();
            document_map.get(&params.text_document.uri).map(|x| x.to_string())
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
            None => self.debug("could not find document").await
        };
    }
}

fn char_to_position(rope: &Rope, idx: usize) -> Position {
    let line = rope.char_to_line(idx);
    let line_start = rope.line_to_char(line);
    let character = idx - line_start;

    let line = u32::try_from(line).unwrap();
    let character = u32::try_from(character).unwrap();

    Position {
        line,
        character
    }
}

fn document_map_to_cache(document_map: &HashMap<Url, Rope>) -> HashMap<PathBuf, String> {
    document_map.iter().map(|(file_uri, document)| {
        let (path, content) = (PathBuf::from(url_to_string(&file_uri)), document.to_string());

        (path, content)
    }).collect()
}

fn url_to_string(url: &Url) -> String {
    url
        .to_file_path()
        .expect("Invalid text document URI")
        .into_os_string()
        .into_string()
        .expect("Invalid text document URI")
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    
    let (service, socket) = LspService::new(|client| Backend::new(client));

    Server::new(stdin, stdout, socket).serve(service).await;
}
