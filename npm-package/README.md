# @completium/archetype

Archetype is a domain-specific language (DSL) to develop smart contracts
on the Tezos blockchain, with a specific focus on contract security.

See https://archetype-lang.org for more details.

## Usage

```js
const archetype = require('@completium/archetype');
const fs = require('fs');

const output = archetype.compile('./resources/simple.arl', {
  target: 'michelson'
});
console.log(output);
```
