// import { execa } from 'execa';

describe('Generate binding', async () => {
  it('up', async () => {
    const spawn = require('cross-spawn');

    const bin = '../archetype.exe'


    // const { stdout, stderr, failed } = await execa(bin, ["-v"], {})
    // console.log(stdout)

    const res = spawn.sync(bin, ['-v'], { });
    console.log(JSON.stringify(res, null, 2))
    // const stdout = res.stderr;
    // console.log(stdout.toString())
  })
})
