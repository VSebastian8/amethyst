use std::collections::HashMap;
use std::error::Error;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification as _},
    request::{Completion, DocumentDiagnosticRequest, Formatting, HoverRequest},
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticOptions, DiagnosticSeverity, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    DocumentFormattingParams, FullDocumentDiagnosticReport, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, MarkupContent, MarkupKind, OneOf, Position, Range,
    RelatedFullDocumentDiagnosticReport, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Url,
};

use crate::ast::Ast;
use crate::cst::Cst;
use crate::desugar::Desugarer;
use crate::fair::{flatten_automata, FAIR};
use crate::format::{format, serialize, stringify};
use crate::gem;
use crate::info;

// Store the CST of each opened file
struct Docs(HashMap<Url, Cst>);

pub fn run_lsp_server() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Talk LSP over stdio
    let (connection, io_threads) = Connection::stdio();

    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: None,
            ..Default::default()
        }),
        definition_provider: Some(OneOf::Left(false)),
        diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
            DiagnosticOptions {
                inter_file_dependencies: true,
                workspace_diagnostics: false,
                ..Default::default()
            },
        )),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };
    let server_capabilities = serde_json::to_value(&capabilities)?;
    let initialize_params = connection.initialize(server_capabilities)?;
    let _params: InitializeParams = serde_json::from_value(initialize_params)?;

    run(connection, Docs(HashMap::new()))?;

    io_threads.join()?;
    Ok(())
}

fn run(connection: Connection, mut docs: Docs) -> Result<(), Box<dyn Error + Sync + Send>> {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handle_request(&connection, &mut docs, req)?;
            }
            Message::Notification(not) => {
                handle_notification(&mut docs, not);
            }
            Message::Response(_) => {
                // Never send requests to the client
            }
        }
    }
    Ok(())
}

fn handle_request(
    connection: &Connection,
    docs: &mut Docs,
    req: Request,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    // Hover request
    let req = match cast_req::<HoverRequest>(req) {
        Ok((id, params)) => {
            let resp = hover(docs, params);
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, resp)))?;
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(req)) => req,
        Err(ExtractError::JsonError { .. }) => return Ok(()),
    };
    // Autocompletion request
    let req = match cast_req::<Completion>(req) {
        Ok((id, params)) => {
            let resp = completion(docs, params);
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, resp)))?;
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(req)) => req,
        Err(ExtractError::JsonError { .. }) => return Ok(()),
    };
    // Diagnostics request
    let req = match cast_req::<DocumentDiagnosticRequest>(req) {
        Ok((id, params)) => {
            let resp = diagnostic(docs, params);
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, resp)))?;
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(req)) => req,
        Err(ExtractError::JsonError { .. }) => return Ok(()),
    };
    // Formatting request
    let req = match cast_req::<Formatting>(req) {
        Ok((id, params)) => {
            let resp = format_document(docs, params);
            connection
                .sender
                .send(Message::Response(Response::new_ok(id, resp)))?;
            return Ok(());
        }
        Err(ExtractError::MethodMismatch(req)) => req,
        Err(ExtractError::JsonError { .. }) => return Ok(()),
    };
    // Unhandled method
    connection.sender.send(Message::Response(Response::new_err(
        req.id,
        lsp_server::ErrorCode::MethodNotFound as i32,
        format!("unhandled method: {}", req.method),
    )))?;
    Ok(())
}

// Parse the file contents into a CST and store it
fn handle_notification(docs: &mut Docs, not: Notification) {
    let (uri, code) = match not.method.as_str() {
        DidOpenTextDocument::METHOD => {
            if let Ok(p) = serde_json::from_value::<DidOpenTextDocumentParams>(not.params) {
                (p.text_document.uri, p.text_document.text)
            } else {
                return;
            }
        }
        DidChangeTextDocument::METHOD => {
            if let Ok(p) = serde_json::from_value::<DidChangeTextDocumentParams>(not.params) {
                if let Some(change) = p.content_changes.into_iter().last() {
                    (p.text_document.uri, change.text)
                } else {
                    return;
                }
            } else {
                return;
            }
        }
        _ => {
            return;
        }
    };
    docs.0.insert(uri, gem::parse_cst(&code));
}

fn hover(_docs: &Docs, params: HoverParams) -> Option<Hover> {
    // Hover over known states/automata and show their description
    // TODO: ignore comments, distinguish states from different automata
    let _uri = params.text_document_position_params.text_document.uri;
    let _pos = params.text_document_position_params.position;
    let word = "none";

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**{}**\n\n({} characters)", word, word.chars().count()),
        }),
        range: None,
    })
}

fn completion(_docs: &Docs, _params: CompletionParams) -> CompletionResponse {
    // Show keywords and current states/aliases/automata
    // TODO: context sensitive completion
    CompletionResponse::Array(vec![
        CompletionItem {
            label: "automaton".into(),
            kind: Some(CompletionItemKind::TEXT),
            detail: Some("amethyst-lsp suggestion".into()),
            ..Default::default()
        },
        CompletionItem {
            label: "state".into(),
            kind: Some(CompletionItemKind::TEXT),
            detail: Some("amethyst-lsp suggestion".into()),
            ..Default::default()
        },
    ])
}

fn diagnostic(docs: &Docs, params: DocumentDiagnosticParams) -> Option<DocumentDiagnosticReport> {
    let uri = params.text_document.uri;
    let cst = docs.0.get(&uri)?;
    let Ast {
        errors: syntax_errors,
        automata,
    } = Desugarer::new().desugar(cst.clone());
    let FAIR {
        errors: logic_errors,
        ..
    } = flatten_automata(automata);
    let errors: Vec<_> = syntax_errors
        .into_iter()
        .chain(logic_errors.into_iter())
        .collect();

    // Find possible errors in the .myst file
    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
        full_document_diagnostic_report: FullDocumentDiagnosticReport {
            items: errors
                .iter()
                .flat_map(|err| error_diagnostic(&err))
                .collect(),
            ..Default::default()
        },
        ..Default::default()
    })
    .into()
}

fn error_diagnostic(err: &info::ErrorInfo) -> Option<Diagnostic> {
    match err.info {
        None => None,
        Some(info::Info { line, from, to }) => Some(Diagnostic {
            range: Range {
                start: Position {
                    line: line,
                    character: from,
                },
                end: Position {
                    line: line,
                    character: to,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            message: err.error.to_string(),
            ..Default::default()
        }),
    }
}

fn format_document(docs: &Docs, params: DocumentFormattingParams) -> Option<Vec<TextEdit>> {
    let uri = params.text_document.uri;
    let cst = docs.0.get(&uri)?;
    // Format the CST
    let formatted_cst = format(&cst);
    if formatted_cst == *cst {
        // No changes
        return Some(vec![]);
    }
    // Get formatted code
    let formatted_code = stringify(serialize(formatted_cst));
    // Replace the entire document in one edit
    Some(vec![TextEdit {
        range: Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX)),
        new_text: formatted_code,
    }])
}

fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
