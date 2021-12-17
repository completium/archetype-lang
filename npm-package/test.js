// const archetype = require('@completium/archetype');
const archetype = require('./dist/index.js');
const fs = require('fs');

// const input = fs.readFileSync('resources/type_error.arl').toString();
const input = fs.readFileSync('resources/simple.arl').toString();
try {
const output = archetype.compile(input, {
  target: 'michelson'
});
console.log(output);
} catch (e) {
  console.error(e)
}
