
const archetype = require('@completium/archetype');

const output = archetype.compile('./resources/simple.arl', {
  target: 'michelson'
});
console.log(output);
