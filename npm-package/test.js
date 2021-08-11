// const archetype = require('@completium/archetype');
const archetype = require('./dist/index.js');
const fs = require('fs');

const input = fs.readFileSync('resources/simple.arl').toString();
const output = archetype.compile(input, {
  target: 'michelson'
});
console.log(output);
