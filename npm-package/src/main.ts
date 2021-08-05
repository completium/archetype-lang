var api = require("./api.bc.js");

export function version() {
  return api.version;
}

export function compile(src : string, settings : object = {}) {
  return api.compile()(src, settings);
}

export function decompile(src : string, settings : object = {}) {
  return api.decompile()(src, settings);
}

export function get_expr(data, settings : object = {}) {
  return api.get_expr()(data, settings);
}

export function get_expr_type(data, type, settings : object = {}) {
  return api.get_expr_type()(data, type, settings);
}

export function with_parameters(src, settings : object = {}) {
  return api.with_parameters()(src, settings);
}

export function show_entries(src, settings : object = {}) {
  return api.show_entries()(src, settings);
}
