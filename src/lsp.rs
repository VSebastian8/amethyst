use std::error::Error;
use std::{collections::HashMap, rc::Rc};

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

use crate::ast::{Ast, Automaton, StateType};
use crate::cst::Cst;
use crate::desugar::Desugarer;
use crate::fair::{flatten_automata, FAIR};
use crate::format::{format, serialize, stringify};
use crate::gem;
use crate::info::*;

// Usefull info for the lsp for efficient responses
struct LspInfo {
    cst: Cst,
    errors: Vec<ErrorInfo>,
    idents: Vec<StringInfo>,
    descs: HashMap<Rc<str>, (Rc<str>, Rc<str>)>, // ident -> (typ, desc)
}

// Store the LSP info of each opened file
struct Docs(HashMap<Url, LspInfo>);

fn update_docs(docs: &mut Docs, uri: Url, code: String) {
    // Gather necessary info from the myst code
    let cst = gem::parse_cst(&code);
    let Ast {
        errors: syntax_errors,
        automata,
    } = Desugarer::new().desugar(cst.clone());
    let idents = extract_identifiers(&automata);
    let descs = extract_descriptions(&automata);
    let FAIR {
        errors: logic_errors,
        ..
    } = flatten_automata(automata);
    let errors: Vec<_> = syntax_errors
        .into_iter()
        .chain(logic_errors.into_iter())
        .collect();
    // Update the info for the current uri
    docs.0.insert(
        uri,
        LspInfo {
            cst,
            errors,
            idents,
            descs,
        },
    );
}

fn extract_identifiers(automata: &Vec<Automaton>) -> Vec<StringInfo> {
    let mut idents = vec![];
    for automaton in automata {
        idents.push(automaton.name.clone());
        for (blueprint, alias) in &automaton.components {
            idents.push(blueprint.clone());
            idents.push(alias.clone());
        }
        for state in &automaton.states {
            idents.push(state.name.clone());
            if let StateType::State(parent, _, transitions) = &state.typ {
                if let Some(name) = parent {
                    idents.push(name.clone())
                }
                for transition in transitions {
                    idents.push(transition.state.0.clone());
                    if let Some(name) = &transition.state.1 {
                        idents.push(name.clone())
                    }
                }
            }
        }
    }
    idents
}

fn extract_descriptions(automata: &Vec<Automaton>) -> HashMap<Rc<str>, (Rc<str>, Rc<str>)> {
    let mut descs = HashMap::new();
    for automaton in automata {
        descs.insert(
            automaton.name.name.clone(),
            ("automaton".into(), automaton.desc.clone()),
        );
        for state in &automaton.states {
            descs.insert(
                state.name.name.clone(),
                ("state".into(), state.desc.clone()),
            );
        }
    }
    descs
}

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

// Update the docs with the useful info from the code
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
    update_docs(docs, uri, code);
}

fn hover(docs: &Docs, params: HoverParams) -> Option<Hover> {
    // Hover over known states/automata and show their description
    // TODO: distinguish states from different automata
    let uri = params.text_document_position_params.text_document.uri;
    let lsp_info = docs.0.get(&uri)?;
    let pos = params.text_document_position_params.position;
    // Find identifier at position
    let ident = lsp_info.idents.iter().find(|ident| {
        ident.info.line == pos.line
            && ident.info.from <= pos.character
            && ident.info.to >= pos.character
    })?;
    let (typ, desc) = lsp_info
        .descs
        .get(&ident.name)
        .cloned()
        .map(|(typ, desc)| {
            (
                typ,
                if desc.is_empty() {
                    "".into()
                } else {
                    // Pretty markdown formatting
                    format!("\n***\n{}", desc)
                },
            )
        })
        .unwrap_or(("".into(), "".into()));
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```amethyst\n{} {}\n```{}", typ, ident.name, desc),
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
    let lsp_info = docs.0.get(&uri)?;
    // Find possible errors in the .myst file
    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
        full_document_diagnostic_report: FullDocumentDiagnosticReport {
            items: lsp_info
                .errors
                .iter()
                .flat_map(|err| error_diagnostic(&err))
                .collect(),
            ..Default::default()
        },
        ..Default::default()
    })
    .into()
}

fn error_diagnostic(err: &ErrorInfo) -> Option<Diagnostic> {
    match err.info {
        None => None,
        Some(Info { line, from, to }) => Some(Diagnostic {
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
    let lsp_info = docs.0.get(&uri)?;
    // Format the CST
    let formatted_cst = format(&lsp_info.cst);
    if formatted_cst == *lsp_info.cst {
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
