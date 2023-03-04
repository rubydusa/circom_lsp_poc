use std::cell::RefCell;
use std::sync::Mutex;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use codespan_reporting::diagnostic::{Severity, LabelStyle};

#[derive(Debug)]
struct Backend {
    client: Client,
    rope: Mutex<RefCell<Rope>>,
}

impl Backend {
    fn new(client: Client) -> Backend { 
        Backend { 
            client,
            rope: Mutex::new(RefCell::new(Rope::from_str(""))), 
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

        let rope = self.rope.lock().unwrap();
        rope.replace(Rope::from_str(&params.text_document.text));
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let rope = self.rope.lock().unwrap();
        rope.replace(Rope::from_str(&params.content_changes[0].text));
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let reports = {
            let file = params.text_document.uri
                .to_file_path()
                .expect("Invalid text document URI")
                .into_os_string()
                .into_string()
                .expect("Invalid text document URI");

            match circom_parser::run_parser(file, "2.1.4", vec![]) {
                Ok((mut archive, mut reports)) => {
                    let mut type_reports = match circom_type_checker::check_types::check_types(&mut archive) {
                        Ok(type_reports) => type_reports,
                        Err(type_reports) => type_reports
                    };
                    reports.append(&mut type_reports);
                    reports
                },
                Err((_, reports)) => reports
            }
        };

        self.client
            .log_message(MessageType::INFO, format!("reports: {:?}", reports
                                                    .clone()
                                                    .into_iter()
                                                    .map(|x| x.to_diagnostic()).collect::<Vec<_>>()))
                .await;

        let diagnostics = {
            /*
            let rope = self.rope.lock().unwrap();
            let rope = rope.borrow().clone();

            match lang::ParseAstParser::new().parse(&rope.clone().to_string()) {
                Err(parse_error) => {
                    let (start, end, message) = match parse_error {
                        ParseError::InvalidToken { location } => (
                            location, 
                            location, 
                            String::from("Invalid token")
                        ),
                        ParseError::UnrecognizedToken { ref token, .. } => (
                            token.0, 
                            token.2, 
                            format!("Unrecognized token: \"{}\"", token.2)
                        ),
                        ParseError::ExtraToken { ref token, .. } => (
                            token.0, 
                            token.2, 
                            format!("Extra token: \"{:?}\"", token.2)
                        ),
                        _ => (0, 0, String::from("A grammar error occured"))
                    };

                    Some(build_diagnostic(&rope, message, start, end))
                },
                _ => None
            }
            */
            let rope = self.rope.lock().unwrap();
            let rope = rope.borrow().clone();

            reports.into_iter().filter_map(|x| report_to_diagnostic(&rope, x)).collect()
        };

        self.client
            .log_message(MessageType::INFO, format!("diagnostics: {:?}", diagnostics))
            .await;

        self.client
            .publish_diagnostics(
                params.text_document.uri, 
                diagnostics,
                None
            ).await;
    }
}

fn report_to_diagnostic(rope: &Rope, report: circom_structure::error_definition::Report) -> Option<Diagnostic> {
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
    })?;

    let range = Range {
        start: char_to_position(rope, label.range.start),
        end: char_to_position(rope, label.range.end)
    };

    let severity = match diagnostic.severity {
        Severity::Bug | Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Help => DiagnosticSeverity::HINT,
        Severity::Note => DiagnosticSeverity::INFORMATION
    };

    let message = diagnostic.message;

    Some(Diagnostic {
        range,
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some(String::from("circom_lsp")),
        message,
        related_information: None,
        tags: None,
        data: None
    })
}
/*
fn build_diagnostic(rope: &Rope, message: String, start: usize, end: usize) -> Diagnostic {
    let range = Range {
        start: char_to_position(rope, start),
        end: char_to_position(rope, end)
    };

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::Number(0)),
        code_description: None,
        source: Some(String::from("circom_lsp")),
        message,
        related_information: None,
        tags: None,
        data: None
    }
}
*/

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

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    
    let (service, socket) = LspService::new(|client| Backend::new(client));

    Server::new(stdin, stdout, socket).serve(service).await;
}

