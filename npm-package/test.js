// const archetype = require('./index');

var api = require("./src/api.js");

const src = "archetype simple variable n : nat = 0 entry exec () { n := 2 }";
const src_error = "archetypes simple variable n : nat = 0 entry exec () { n := 2 }";
const src_param = "archetype simple(a : nat) variable n : nat = 0 entry exec () { n := a }";
const mic = "{ storage nat; parameter unit; code { UNPAIR; DROP; PUSH nat 2; SWAP; DROP; NIL operation; PAIR }; }"
const expr = "(1, \"mystr\")"

// api.compile()(src);

console.log(api.version);
console.log(api.compile()(src))
console.log(api.compileJS()(src))
console.log(api.getWhyml()(src))
console.log(api.getParameters()(src_param))
console.log(api.showEntries()(mic))

try {
  console.log(api.compile()(src_error));
} catch (e) {
  console.error(JSON.stringify(e))
}

// console.log(api.id()(src));
// console.log(api.id()(src));
