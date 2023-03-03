use circom::lang;
use std::cell::RefCell;
use std::sync::Mutex;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use lalrpop_util::ParseError;

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
        let diagnostic = {
            let rope = self.rope.lock().unwrap();
            let rope = rope.borrow().clone();

            match lang::ParseAstParser::new().parse(&rope.clone().to_string()) {
                Err(parse_error) => {
                    let (start, end) = match parse_error {
                        ParseError::InvalidToken { location } => (location, location),
                        ParseError::UnrecognizedToken { ref token, .. } => (token.0, token.2),
                        ParseError::ExtraToken { ref token, .. } => (token.0, token.2),
                        _ => (0, 0)
                    };

                    Some(build_diagnostic(&rope, format!("{:?}", parse_error), start, end))
                },
                _ => None
            }
        };

        self.client
            .log_message(MessageType::INFO, format!("{:?}", diagnostic))
            .await;

        self.client
            .publish_diagnostics(
                params.text_document.uri, 
                match diagnostic {
                    Some(x) => vec![x],
                    None => vec![]
                }, 
                None
            ).await;
    }
}

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

    // note on how to test LSP Communication:
    //
    // Need to import these traits:
    //
    // use tower::Service;
    // use tower::util::ServiceExt;
    //
    // to make a call to the service:
    //
    // service.ready().await.unwrap().call(initialize).await;
    //
    // if in doubt, check tower_lsp tests source code
    
    let (service, socket) = LspService::new(|client| Backend::new(client));

    Server::new(stdin, stdout, socket).serve(service).await;
}
