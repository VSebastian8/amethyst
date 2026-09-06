use std::collections::HashMap;
use std::error::Error;

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification as _},
    request::{Completion, DocumentDiagnosticRequest, HoverRequest},
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticOptions, DiagnosticSeverity, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport,
    FullDocumentDiagnosticReport, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, MarkupContent, MarkupKind, OneOf, Position, Range,
    RelatedFullDocumentDiagnosticReport, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};

use crate::{ast::Ast, info};
use crate::{
    fair::{flatten_automata, FAIR},
    gem,
};

/// In-memory store of every open document's full text, keyed by URI.
/// Kept trivially simple: whole-file sync, no incremental edits.
struct Docs(HashMap<Url, String>);

impl Docs {
    fn word_at(&self, uri: &Url, pos: Position) -> Option<String> {
        let text = self.0.get(uri)?;
        let line = text.lines().nth(pos.line as usize)?;
        let chars: Vec<char> = line.chars().collect();
        let col = (pos.character as usize).min(chars.len());

        let is_word = |c: char| c.is_alphanumeric() || c == '_';

        let mut start = col;
        while start > 0 && is_word(chars[start - 1]) {
            start -= 1;
        }
        let mut end = col;
        while end < chars.len() && is_word(chars[end]) {
            end += 1;
        }
        if start == end {
            return None;
        }
        Some(chars[start..end].iter().collect())
    }
}

pub fn run_lsp_server() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Talk LSP over stdio, which is how VS Code will spawn us.
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
                // We never send requests to the client in this minimal server,
                // so there's nothing to correlate a response against.
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

    // Unhandled method: reply with a "method not found" style empty error
    // rather than hanging the client.
    connection.sender.send(Message::Response(Response::new_err(
        req.id,
        lsp_server::ErrorCode::MethodNotFound as i32,
        format!("unhandled method: {}", req.method),
    )))?;
    Ok(())
}

fn handle_notification(docs: &mut Docs, not: Notification) {
    match not.method.as_str() {
        DidOpenTextDocument::METHOD => {
            if let Ok(p) = serde_json::from_value::<DidOpenTextDocumentParams>(not.params) {
                docs.0.insert(p.text_document.uri, p.text_document.text);
            }
        }
        DidChangeTextDocument::METHOD => {
            if let Ok(p) = serde_json::from_value::<DidChangeTextDocumentParams>(not.params) {
                // Full sync: the last content change is the entire new document text.
                if let Some(change) = p.content_changes.into_iter().last() {
                    docs.0.insert(p.text_document.uri, change.text);
                }
            }
        }
        _ => {}
    }
}

fn hover(docs: &Docs, params: HoverParams) -> Option<Hover> {
    let uri = params.text_document_position_params.text_document.uri;
    let pos = params.text_document_position_params.position;
    let word = docs.word_at(&uri, pos)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("**{}**\n\n({} characters)", word, word.chars().count()),
        }),
        range: None,
    })
}

fn completion(_docs: &Docs, _params: CompletionParams) -> CompletionResponse {
    // A trivial, static completion list — just enough to prove the round trip
    // from typing in the editor to a populated suggestion widget works.
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
    let code = docs.0.get(&uri)?;
    let Ast {
        errors: syntax_errors,
        automata,
    } = gem::parse_ast(code);
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

fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
