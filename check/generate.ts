import ts, { createPrinter, createSourceFile, factory, ListFormat, NodeArray, NewLineKind, ScriptKind, ScriptTarget } from 'typescript';
import { BindingSettings, generate_binding, Language, Target } from "@completium/archetype-binder-ts";
import { RawContractInterface } from "@completium/archetype-binder-ts/build/src/utils";

const fs = require('fs')
const path = require('path')

const skip: Array<string> = []

const write_binding = (input: string) => {
  const filename = input + '.json';
  const json = fs.readFileSync(filename);
  let rci: RawContractInterface = JSON.parse(json);
  const settings: BindingSettings = {
    language: Language.Archetype,
    target: Target.Experiment,
    path: '../tests/passed/'
  }
  const output = generate_binding(rci, settings);
  const out_ts = './bindings/passed/' + path.basename(filename.replace('.json', '.ts'));
  fs.writeFileSync(out_ts, output)
}

const generate_spec_template = (input: Array<string>) => {
  let imports: Array<string> = []
  let items: Array<string> = []
  for (const id of input) {
    imports.push(`import * as ${id} from '../bindings/passed/${id}'\n`)
    items.push(`

    // TODO
    it('${id}', async () => {
      await ${id}.${id}.deploy({ as: alice })
      //      const before_expected = new Nat(0)
      //      const after_expected = new Nat(1)
      //      const res_before = await ${id}.${id}.get_res();
      //      assert(res_before.equals(before_expected), "Invalid Value")
      //      await ${id}.${id}.exec({ as: alice })
      //      const res_after = await ${id}.${id}.get_res();
      //      assert(res_after.equals(after_expected), "Invalid Value")
    })`);
  }
  const output = `/* DO NOT EDIT, GENERATED FILE */
import { get_account, set_mockup, set_quiet } from '@completium/experiment-ts';
import assert from 'assert'
import * as att from '@completium/archetype-ts-types';

${(imports.map(x => x)).join('')}

const alice = get_account('alice')

/* Verbose mode ------------------------------------------------------------ */

set_quiet(true);

/* Endpoint ---------------------------------------------------------------- */

set_mockup()

/* Tests-------------------------------------------------------------------- */

describe('Tests', async () => {
  describe('Passed', async () => {
${(items.map(x => x)).join('')}
  })
})
  `
  fs.writeFileSync('./tests/template.spec.ts', output)
}

const generate_spec_passed = (items: Array<string>) => {
  const path = './tests/passed.actual.spec.ts'

  const gen_ImportDeclaration = (name: string, items: Array<string>): ts.ImportDeclaration => {
    return factory.createImportDeclaration(
      undefined,
      factory.createImportClause(
        false,
        undefined,
        factory.createNamedImports(
          items.map(x => factory.createImportSpecifier(
            false,
            undefined,
            factory.createIdentifier(x)
          )
          )
        )
      ),
      factory.createStringLiteral(name),
      undefined
    )
  }

  const gen_item_ImportDeclaration = (name: string) => {
    return factory.createImportDeclaration(
      undefined,
      factory.createImportClause(
        false,
        undefined,
        factory.createNamespaceImport(factory.createIdentifier(name))
      ),
      factory.createStringLiteral(`../bindings/passed/${name}`),
      undefined
    )
  }

  const import_completium_experiment = gen_ImportDeclaration(
    "@completium/experiment-ts",
    [
      'expect_to_fail',
      'get_account',
      'set_mockup',
      'set_quiet',
    ])
  const import_completium_archetype_ts_types = gen_ImportDeclaration(
    "@completium/archetype-ts-types",
    [
      'Address',
      'Bytes',
      'Int',
      'Micheline',
      'Nat',
      'Option',
      'Or',
      'Rational',
      'Tez'
    ])
  const import_assert = factory.createImportDeclaration(
    undefined,
    factory.createImportClause(
      false,
      factory.createIdentifier("assert"),
      undefined
    ),
    factory.createStringLiteral("assert"),
    undefined
  )
  const import_BigNumber = gen_ImportDeclaration("bignumber.js", ["BigNumber"])

  const gen_account = (name: string): ts.VariableStatement => {
    return factory.createVariableStatement(
      undefined,
      factory.createVariableDeclarationList(
        [factory.createVariableDeclaration(
          factory.createIdentifier(name),
          undefined,
          undefined,
          factory.createCallExpression(
            factory.createIdentifier("get_account"),
            undefined,
            [factory.createStringLiteral(name)]
          )
        )],
        ts.NodeFlags.Const
      )
    )
  }

  const gen_expr_statement = (input: [string, Array<ts.Expression>]): ts.ExpressionStatement => {
    return factory.createExpressionStatement(factory.createCallExpression(
      factory.createIdentifier(input[0]),
      undefined,
      input[1]
    ))
  }

  const basic_imports: Array<ts.ImportDeclaration> = [import_completium_experiment, import_completium_archetype_ts_types, import_assert, import_BigNumber]
  const item_imports: Array<ts.ImportDeclaration> = items.map(gen_item_ImportDeclaration)
  const accounts: Array<ts.VariableStatement> = ['alice', 'bob', 'carl'].map(gen_account)
  const aaa: Array<[string, Array<ts.Expression>]> = [["set_quiet", [factory.createTrue()]], ["set_mockup", []]]
  const expr_statements: Array<ts.ExpressionStatement> = aaa.map(gen_expr_statement)
  const gen_it = (name: string) => {
    return factory.createExpressionStatement(factory.createCallExpression(
      factory.createIdentifier("it"),
      undefined,
      [
        factory.createStringLiteral(name),
        factory.createArrowFunction(
          [factory.createToken(ts.SyntaxKind.AsyncKeyword)],
          undefined,
          [],
          undefined,
          factory.createToken(ts.SyntaxKind.EqualsGreaterThanToken),
          factory.createBlock(
            [factory.createExpressionStatement(factory.createAwaitExpression(factory.createCallExpression(
              factory.createPropertyAccessExpression(
                factory.createPropertyAccessExpression(
                  factory.createIdentifier(name),
                  factory.createIdentifier(name)
                ),
                factory.createIdentifier("deploy")
              ),
              undefined,
              [factory.createObjectLiteralExpression(
                [factory.createPropertyAssignment(
                  factory.createIdentifier("as"),
                  factory.createIdentifier("alice")
                )],
                false
              )]
            )))],
            true
          )
        )
      ]
    ))
  }
  const desc = [
    factory.createExpressionStatement(factory.createCallExpression(
      factory.createIdentifier("describe"),
      undefined,
      [
        factory.createStringLiteral("passed"),
        factory.createArrowFunction(
          [factory.createToken(ts.SyntaxKind.AsyncKeyword)],
          undefined,
          [],
          undefined,
          factory.createToken(ts.SyntaxKind.EqualsGreaterThanToken),
          factory.createBlock(
            items.map(gen_it),
            true
          )
        )
      ]
    ))];
  const decls: Array<Array<ts.ImportDeclaration | ts.VariableStatement | ts.ExpressionStatement>> = [basic_imports, item_imports, accounts, expr_statements, desc]
  const decls2: Array<ts.ImportDeclaration | ts.VariableStatement | ts.ExpressionStatement> = decls.flat()
  const nodeArr = factory.createNodeArray(decls2);
  const printer = createPrinter({ newLine: NewLineKind.LineFeed });
  const file = createSourceFile("source.ts", "", ScriptTarget.ESNext, false, ScriptKind.TS);
  const output = printer.printList(ListFormat.MultiLine, nodeArr, file);
  fs.writeFileSync(path, output)
}

const generate_spec_error = (input: Array<string>, name: string, path: string, code: number) => {
  let items: Array<string> = []
  for (const id of input) {
    items.push(`
  it('${id}', async () => {
    const stat = compile("${path}/${id}.arl")
    assert(stat.status == ${code}, "Invalid status code, actual: " + stat.status + ", expected: ${code}")
  })`);
  }
  const output = `/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'

/* Utils ------------------------------------------------------------------- */

const compile = (p : string) => {
  const spawn = require('cross-spawn');
  const bin = '../_build/default/src/compiler.exe'
  const res = spawn.sync(bin, [p], { });
  return res
}

/* Tests ------------------------------------------------------------------- */

describe('${name}', async () => {${(items.map(x => x)).join('')}
})
  `
  fs.writeFileSync(`./tests/${name}.spec.ts`, output)
}

const extract_file_dir = (path: string, ext: string): Array<string> => {
  const dir = fs.opendirSync(path)
  let dirent;
  let filenames: Array<string> = []
  while ((dirent = dir.readSync()) !== null) {
    const filename = dirent.name as string;
    if (filename.endsWith(ext)) {
      if (!skip.includes(filename)) {
        const f: string = filename.substring(0, (filename.length - ext.length));
        filenames.push(f)
      }
    }
  }
  dir.closeSync()
  filenames.sort((x, y) => (x > y ? 1 : -1));
  return filenames
}

describe('Generate binding', async () => {
  describe('passed', async () => {
    const p = './json/passed'
    const filenames = extract_file_dir(p, '.json')
    for (const filename of filenames) {
      it(filename, () => {
        write_binding(p + '/' + filename)
      });
    }
    it('Generate passed.spec.ts', async () => {
      generate_spec_passed(filenames)
    })
  })

  describe('Generate spec.ts files', async () => {
    const items: Array<[string, string, number]> = [
      ['syntax-errors', '../tests/syntax-errors', 1],
      ['type-errors', '../tests/type-errors', 3],
      ['model-errors', '../tests/model-errors', 5],
      ['proposal-type-errors', '../tests/proposal-type-errors', 3],
      ['proposal-model-errors', '../tests/proposal-model-errors', 5]
    ]
    for (const item of items) {
      const name = item[0]
      const path = item[1]
      const code = item[2]

      it(name, () => {
        const filenames = extract_file_dir(path, ".arl")
        generate_spec_error(filenames, name, path, code)
      })
    }
  })
})
