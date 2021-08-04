var api = require("./api.bc.js");

export function version() {
  return api.version;
}

export function compile(src) {
  return api.compile()(src);
}

export function compileJS(src) {
  return api.compileJS()(src)
}

export function getWhyml(src) {
  return api.getWhyml()(src)
}

export function getParameters(src) {
  return api.getParameters()(src)
}

export function showEntries(src) {
  return api.showEntries()(src)
}

