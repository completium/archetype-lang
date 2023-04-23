var api = require("./api.bc.js");

export function version() {
  return api.version;
}

export function compile(src : string, settings : object = {}) {
  return api.compile()(src, settings).trim();
}

export function compile_from_string(src : string, settings : object = {}) {
  return api.compile_from_string()(src, settings).trim();
}

export function decompile(src : string, settings : object = {}) {
  return api.decompile()(src, settings).trim();
}

export function decompile_from_string(src : string, settings : object = {}) {
  return api.decompile_from_string()(src, settings).trim();
}

export function get_expr(data, settings : object = {}) {
  return api.getExpr()(data, settings).trim();
}

export function get_expr_type(data, type, settings : object = {}) {
  return api.getExprType()(data, type, settings).trim();
}

export function show_entries(src, settings : object = {}) {
  return api.showEntries()(src, settings).trim();
}

export function lsp(kind, path, src) {
  return api.lsp()(kind, path, src);
}

export function services(service, src) {
  return api.services()(service, src);
}
