/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'
import { interp } from '@completium/completium-cli'

/* Tools ------------------------------------------------------------------- */

interface ConfInput {
  entrypoint?: string,
  parameter?: string,
  storage?: string
  amount?: number,
}

async function check_prelude(ref : string, act : string) {

}

async function check_transaction(ref : string, act : string, conf : ConfInput) {
  const conf_ref = {
    path: ref,
    entry: conf.entrypoint,
    argMichelson: conf.parameter,
    storage: conf.storage,
  }
  const conf_act = {
    path: act,
    entry: conf.entrypoint,
    argMichelson: conf.parameter,
    storage: conf.storage,
  }

  const res_ref = await interp(conf_ref)
  const res_act = await interp(conf_act)

  assert(JSON.stringify(res_ref) === JSON.stringify(res_act))
}

/* Tests ------------------------------------------------------------------- */

describe('decomp_mainnet', async () => {

  it('KT1NhtHwHD5cqabfSdwg1Fowud5f175eShwx', async () => {
    const ref = "./michelson/mainnet/KT1NhtHwHD5cqabfSdwg1Fowud5f175eShwx.tz"
    const act = "./archetype/mainnet/KT1NhtHwHD5cqabfSdwg1Fowud5f175eShwx.arl"
    await check_prelude(ref, act)
    await check_transaction(ref, act, {parameter: "2"})
    await check_transaction(ref, act, {parameter: "3"})
  })

  // it('KT1Lj4y492KN1zDyeeKR2HG74SR2j5tcenMV', async () => {
  //   const ref = "./michelson/mainnet/KT1Lj4y492KN1zDyeeKR2HG74SR2j5tcenMV.tz"
  //   const act = "./archetype/mainnet/KT1Lj4y492KN1zDyeeKR2HG74SR2j5tcenMV.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19ptNzn4MVAN45KUUNpyL5AdLVhujk815u', async () => {
  //   const ref = "./michelson/mainnet/KT19ptNzn4MVAN45KUUNpyL5AdLVhujk815u.tz"
  //   const act = "./archetype/mainnet/KT19ptNzn4MVAN45KUUNpyL5AdLVhujk815u.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CJ1CFgnwoYvaJN9H3j1GVjnzcKy4qBDh8', async () => {
  //   const ref = "./michelson/mainnet/KT1CJ1CFgnwoYvaJN9H3j1GVjnzcKy4qBDh8.tz"
  //   const act = "./archetype/mainnet/KT1CJ1CFgnwoYvaJN9H3j1GVjnzcKy4qBDh8.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1TYsdDZBq92cxENbke1YxXEBWdQ6i6WJua', async () => {
  //   const ref = "./michelson/mainnet/KT1TYsdDZBq92cxENbke1YxXEBWdQ6i6WJua.tz"
  //   const act = "./archetype/mainnet/KT1TYsdDZBq92cxENbke1YxXEBWdQ6i6WJua.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1QbprEHD71e6YzWZXNcmxV87gCV5etmpsd', async () => {
  //   const ref = "./michelson/mainnet/KT1QbprEHD71e6YzWZXNcmxV87gCV5etmpsd.tz"
  //   const act = "./archetype/mainnet/KT1QbprEHD71e6YzWZXNcmxV87gCV5etmpsd.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18iWQLZDeJ5tTCLrfnPNip2KSZHmFJFUc7', async () => {
  //   const ref = "./michelson/mainnet/KT18iWQLZDeJ5tTCLrfnPNip2KSZHmFJFUc7.tz"
  //   const act = "./archetype/mainnet/KT18iWQLZDeJ5tTCLrfnPNip2KSZHmFJFUc7.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1HGWymKFrVKKc2U4cFgCWaXsbMsH3x39sD', async () => {
  //   const ref = "./michelson/mainnet/KT1HGWymKFrVKKc2U4cFgCWaXsbMsH3x39sD.tz"
  //   const act = "./archetype/mainnet/KT1HGWymKFrVKKc2U4cFgCWaXsbMsH3x39sD.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE', async () => {
  //   const ref = "./michelson/mainnet/KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE.tz"
  //   const act = "./archetype/mainnet/KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1TgnUythoUoLKxCCEdR1VkjiiY5TmE7M7r', async () => {
  //   const ref = "./michelson/mainnet/KT1TgnUythoUoLKxCCEdR1VkjiiY5TmE7M7r.tz"
  //   const act = "./archetype/mainnet/KT1TgnUythoUoLKxCCEdR1VkjiiY5TmE7M7r.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1XGta53KtvSGfnDNaTcNxXygSgxUL9yNN1', async () => {
  //   const ref = "./michelson/mainnet/KT1XGta53KtvSGfnDNaTcNxXygSgxUL9yNN1.tz"
  //   const act = "./archetype/mainnet/KT1XGta53KtvSGfnDNaTcNxXygSgxUL9yNN1.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FBhqB85ACSpXTzLUTGVa2tT5iAPYtmNDJ', async () => {
  //   const ref = "./michelson/mainnet/KT1FBhqB85ACSpXTzLUTGVa2tT5iAPYtmNDJ.tz"
  //   const act = "./archetype/mainnet/KT1FBhqB85ACSpXTzLUTGVa2tT5iAPYtmNDJ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19gE62t4H1vqMD7UXZWFr28j16tBnPYtMa', async () => {
  //   const ref = "./michelson/mainnet/KT19gE62t4H1vqMD7UXZWFr28j16tBnPYtMa.tz"
  //   const act = "./archetype/mainnet/KT19gE62t4H1vqMD7UXZWFr28j16tBnPYtMa.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AbjG7vtpV8osdoJXcMRck8eTwst8dWoz4', async () => {
  //   const ref = "./michelson/mainnet/KT1AbjG7vtpV8osdoJXcMRck8eTwst8dWoz4.tz"
  //   const act = "./archetype/mainnet/KT1AbjG7vtpV8osdoJXcMRck8eTwst8dWoz4.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GyCkoqwnVQoSYwVKBxMAKprdgXsbkBHa9', async () => {
  //   const ref = "./michelson/mainnet/KT1GyCkoqwnVQoSYwVKBxMAKprdgXsbkBHa9.tz"
  //   const act = "./archetype/mainnet/KT1GyCkoqwnVQoSYwVKBxMAKprdgXsbkBHa9.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AnWaRQQNpJiEGWuSoTdwik6eQP4ZCeYTG', async () => {
  //   const ref = "./michelson/mainnet/KT1AnWaRQQNpJiEGWuSoTdwik6eQP4ZCeYTG.tz"
  //   const act = "./archetype/mainnet/KT1AnWaRQQNpJiEGWuSoTdwik6eQP4ZCeYTG.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Jpvv5APKoncTq39tabQ44bqrqLGpQuQha', async () => {
  //   const ref = "./michelson/mainnet/KT1Jpvv5APKoncTq39tabQ44bqrqLGpQuQha.tz"
  //   const act = "./archetype/mainnet/KT1Jpvv5APKoncTq39tabQ44bqrqLGpQuQha.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1M6LZwxMYBYbUrg1xxuZGN4XWNFuHK7g6X', async () => {
  //   const ref = "./michelson/mainnet/KT1M6LZwxMYBYbUrg1xxuZGN4XWNFuHK7g6X.tz"
  //   const act = "./archetype/mainnet/KT1M6LZwxMYBYbUrg1xxuZGN4XWNFuHK7g6X.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1QaP4JG3RixSgAPwGsc3AuZhzUZsbc3KjC', async () => {
  //   const ref = "./michelson/mainnet/KT1QaP4JG3RixSgAPwGsc3AuZhzUZsbc3KjC.tz"
  //   const act = "./archetype/mainnet/KT1QaP4JG3RixSgAPwGsc3AuZhzUZsbc3KjC.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1BRudFZEXLYANgmZTka1xCDN5nWTMWY7SZ', async () => {
  //   const ref = "./michelson/mainnet/KT1BRudFZEXLYANgmZTka1xCDN5nWTMWY7SZ.tz"
  //   const act = "./archetype/mainnet/KT1BRudFZEXLYANgmZTka1xCDN5nWTMWY7SZ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1QjUt6TyeV4EMYdouBMyvUiK4JjQJyY1EK', async () => {
  //   const ref = "./michelson/mainnet/KT1QjUt6TyeV4EMYdouBMyvUiK4JjQJyY1EK.tz"
  //   const act = "./archetype/mainnet/KT1QjUt6TyeV4EMYdouBMyvUiK4JjQJyY1EK.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CvzXrz19fnHKuWedFY3WqmVAB7kMTPLLS', async () => {
  //   const ref = "./michelson/mainnet/KT1CvzXrz19fnHKuWedFY3WqmVAB7kMTPLLS.tz"
  //   const act = "./archetype/mainnet/KT1CvzXrz19fnHKuWedFY3WqmVAB7kMTPLLS.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1NvAD7W9gqgEs91ztyuUfzXp8ZKcNxVq4S', async () => {
  //   const ref = "./michelson/mainnet/KT1NvAD7W9gqgEs91ztyuUfzXp8ZKcNxVq4S.tz"
  //   const act = "./archetype/mainnet/KT1NvAD7W9gqgEs91ztyuUfzXp8ZKcNxVq4S.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1TzZ4CwKiukbASXEe7RiCFonpud47ek5rX', async () => {
  //   const ref = "./michelson/mainnet/KT1TzZ4CwKiukbASXEe7RiCFonpud47ek5rX.tz"
  //   const act = "./archetype/mainnet/KT1TzZ4CwKiukbASXEe7RiCFonpud47ek5rX.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Lmy1YpMSFH6APuxcZAekSNTYWdtti9s7J', async () => {
  //   const ref = "./michelson/mainnet/KT1Lmy1YpMSFH6APuxcZAekSNTYWdtti9s7J.tz"
  //   const act = "./archetype/mainnet/KT1Lmy1YpMSFH6APuxcZAekSNTYWdtti9s7J.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18anEhkN2Wgpr5u8UyEHVGsbLnrSjNFkDF', async () => {
  //   const ref = "./michelson/mainnet/KT18anEhkN2Wgpr5u8UyEHVGsbLnrSjNFkDF.tz"
  //   const act = "./archetype/mainnet/KT18anEhkN2Wgpr5u8UyEHVGsbLnrSjNFkDF.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CstdKc9TtdDPkSy9dQVZLMYPCYrzTonSB', async () => {
  //   const ref = "./michelson/mainnet/KT1CstdKc9TtdDPkSy9dQVZLMYPCYrzTonSB.tz"
  //   const act = "./archetype/mainnet/KT1CstdKc9TtdDPkSy9dQVZLMYPCYrzTonSB.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Acfs1M5FXHGYQpvdKUwGbZtrUkqrisweJ', async () => {
  //   const ref = "./michelson/mainnet/KT1Acfs1M5FXHGYQpvdKUwGbZtrUkqrisweJ.tz"
  //   const act = "./archetype/mainnet/KT1Acfs1M5FXHGYQpvdKUwGbZtrUkqrisweJ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1BNuzMP54AoPNWQF45LkjhcFjW3A48rcHb', async () => {
  //   const ref = "./michelson/mainnet/KT1BNuzMP54AoPNWQF45LkjhcFjW3A48rcHb.tz"
  //   const act = "./archetype/mainnet/KT1BNuzMP54AoPNWQF45LkjhcFjW3A48rcHb.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PdsBwNpkn6syQQzkjZPpaDFxWDZvcVqZZ', async () => {
  //   const ref = "./michelson/mainnet/KT1PdsBwNpkn6syQQzkjZPpaDFxWDZvcVqZZ.tz"
  //   const act = "./archetype/mainnet/KT1PdsBwNpkn6syQQzkjZPpaDFxWDZvcVqZZ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1LGCVzePeHZB4jHTdAdrieMPvgahd2x9Qz', async () => {
  //   const ref = "./michelson/mainnet/KT1LGCVzePeHZB4jHTdAdrieMPvgahd2x9Qz.tz"
  //   const act = "./archetype/mainnet/KT1LGCVzePeHZB4jHTdAdrieMPvgahd2x9Qz.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18gJTqHFUu7Zpd5JkdcvSkMy7hyjnmcpuE', async () => {
  //   const ref = "./michelson/mainnet/KT18gJTqHFUu7Zpd5JkdcvSkMy7hyjnmcpuE.tz"
  //   const act = "./archetype/mainnet/KT18gJTqHFUu7Zpd5JkdcvSkMy7hyjnmcpuE.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CeLuUxpZKNF4tDASWt1BabXWcVSLhQn1C', async () => {
  //   const ref = "./michelson/mainnet/KT1CeLuUxpZKNF4tDASWt1BabXWcVSLhQn1C.tz"
  //   const act = "./archetype/mainnet/KT1CeLuUxpZKNF4tDASWt1BabXWcVSLhQn1C.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1A37WhddWH3EFFDM1gi124DRgXTzqGYZDL', async () => {
  //   const ref = "./michelson/mainnet/KT1A37WhddWH3EFFDM1gi124DRgXTzqGYZDL.tz"
  //   const act = "./archetype/mainnet/KT1A37WhddWH3EFFDM1gi124DRgXTzqGYZDL.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PcPM7WqJbfLwNSxXKaNci9D9Z2t5WVYVg', async () => {
  //   const ref = "./michelson/mainnet/KT1PcPM7WqJbfLwNSxXKaNci9D9Z2t5WVYVg.tz"
  //   const act = "./archetype/mainnet/KT1PcPM7WqJbfLwNSxXKaNci9D9Z2t5WVYVg.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PAQLpgjcA4k2wxS9V7oMgLLKcmck9USfo', async () => {
  //   const ref = "./michelson/mainnet/KT1PAQLpgjcA4k2wxS9V7oMgLLKcmck9USfo.tz"
  //   const act = "./archetype/mainnet/KT1PAQLpgjcA4k2wxS9V7oMgLLKcmck9USfo.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1EQX67Ct1xygDBdhQ3aLvS6DbMj9B1PZkA', async () => {
  //   const ref = "./michelson/mainnet/KT1EQX67Ct1xygDBdhQ3aLvS6DbMj9B1PZkA.tz"
  //   const act = "./archetype/mainnet/KT1EQX67Ct1xygDBdhQ3aLvS6DbMj9B1PZkA.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1C8qi9AhK4QXr5CfxK252vxR3dJPYUnGH6', async () => {
  //   const ref = "./michelson/mainnet/KT1C8qi9AhK4QXr5CfxK252vxR3dJPYUnGH6.tz"
  //   const act = "./archetype/mainnet/KT1C8qi9AhK4QXr5CfxK252vxR3dJPYUnGH6.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1MHfvmULc6mAGJLit9NbjxtQWFACKqJXQL', async () => {
  //   const ref = "./michelson/mainnet/KT1MHfvmULc6mAGJLit9NbjxtQWFACKqJXQL.tz"
  //   const act = "./archetype/mainnet/KT1MHfvmULc6mAGJLit9NbjxtQWFACKqJXQL.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Ld1i2aNmR1c1LUvguWWVNM13MNFZQ8t6j', async () => {
  //   const ref = "./michelson/mainnet/KT1Ld1i2aNmR1c1LUvguWWVNM13MNFZQ8t6j.tz"
  //   const act = "./archetype/mainnet/KT1Ld1i2aNmR1c1LUvguWWVNM13MNFZQ8t6j.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1ATFPkGjCbgN8HtWrsx4sucRR96Cy4Enfg', async () => {
  //   const ref = "./michelson/mainnet/KT1ATFPkGjCbgN8HtWrsx4sucRR96Cy4Enfg.tz"
  //   const act = "./archetype/mainnet/KT1ATFPkGjCbgN8HtWrsx4sucRR96Cy4Enfg.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Muk5DHuCxtNzNEaswpQQYPxz2MHqX8pGE', async () => {
  //   const ref = "./michelson/mainnet/KT1Muk5DHuCxtNzNEaswpQQYPxz2MHqX8pGE.tz"
  //   const act = "./archetype/mainnet/KT1Muk5DHuCxtNzNEaswpQQYPxz2MHqX8pGE.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Fswo1Bzys4zjLYMMZZfizKzdbYiBVKoTW', async () => {
  //   const ref = "./michelson/mainnet/KT1Fswo1Bzys4zjLYMMZZfizKzdbYiBVKoTW.tz"
  //   const act = "./archetype/mainnet/KT1Fswo1Bzys4zjLYMMZZfizKzdbYiBVKoTW.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AzYk5bnehTAqYp3i8PBzwCZfBQzWGe3fP', async () => {
  //   const ref = "./michelson/mainnet/KT1AzYk5bnehTAqYp3i8PBzwCZfBQzWGe3fP.tz"
  //   const act = "./archetype/mainnet/KT1AzYk5bnehTAqYp3i8PBzwCZfBQzWGe3fP.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GdTbW7vxjEFheHtgF6hdye2ooFc3A1Dh5', async () => {
  //   const ref = "./michelson/mainnet/KT1GdTbW7vxjEFheHtgF6hdye2ooFc3A1Dh5.tz"
  //   const act = "./archetype/mainnet/KT1GdTbW7vxjEFheHtgF6hdye2ooFc3A1Dh5.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PskqjPYaP6M21jYA2bEfUbSyicNLyE43Q', async () => {
  //   const ref = "./michelson/mainnet/KT1PskqjPYaP6M21jYA2bEfUbSyicNLyE43Q.tz"
  //   const act = "./archetype/mainnet/KT1PskqjPYaP6M21jYA2bEfUbSyicNLyE43Q.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1UZwvunmDXSWfQQHeoMvUW4UKhxyAozxXC', async () => {
  //   const ref = "./michelson/mainnet/KT1UZwvunmDXSWfQQHeoMvUW4UKhxyAozxXC.tz"
  //   const act = "./archetype/mainnet/KT1UZwvunmDXSWfQQHeoMvUW4UKhxyAozxXC.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1SmasuGg8HmU8bxgmghQoHU7Jwnjno9vBX', async () => {
  //   const ref = "./michelson/mainnet/KT1SmasuGg8HmU8bxgmghQoHU7Jwnjno9vBX.tz"
  //   const act = "./archetype/mainnet/KT1SmasuGg8HmU8bxgmghQoHU7Jwnjno9vBX.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1RrxFjkyboXV5b8K2Mchkzm2Q2gdBUoUZK', async () => {
  //   const ref = "./michelson/mainnet/KT1RrxFjkyboXV5b8K2Mchkzm2Q2gdBUoUZK.tz"
  //   const act = "./archetype/mainnet/KT1RrxFjkyboXV5b8K2Mchkzm2Q2gdBUoUZK.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PqL9AhsZo2ZrSNVkF3f2cQ9FE3YALnCsN', async () => {
  //   const ref = "./michelson/mainnet/KT1PqL9AhsZo2ZrSNVkF3f2cQ9FE3YALnCsN.tz"
  //   const act = "./archetype/mainnet/KT1PqL9AhsZo2ZrSNVkF3f2cQ9FE3YALnCsN.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GnnsWW4Nwf5s2VF3pCLkomy6xWUjCusNb', async () => {
  //   const ref = "./michelson/mainnet/KT1GnnsWW4Nwf5s2VF3pCLkomy6xWUjCusNb.tz"
  //   const act = "./archetype/mainnet/KT1GnnsWW4Nwf5s2VF3pCLkomy6xWUjCusNb.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FPiKurM7i57fL9qicEuLdPCjDNmfDcQEv', async () => {
  //   const ref = "./michelson/mainnet/KT1FPiKurM7i57fL9qicEuLdPCjDNmfDcQEv.tz"
  //   const act = "./archetype/mainnet/KT1FPiKurM7i57fL9qicEuLdPCjDNmfDcQEv.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19vbfD5V3qxWmbR9ygj9qQXfcVp2PDnc4u', async () => {
  //   const ref = "./michelson/mainnet/KT19vbfD5V3qxWmbR9ygj9qQXfcVp2PDnc4u.tz"
  //   const act = "./archetype/mainnet/KT19vbfD5V3qxWmbR9ygj9qQXfcVp2PDnc4u.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18wZcxQaCUMPkK5hPrkSjRW8Ma89RKNbRn', async () => {
  //   const ref = "./michelson/mainnet/KT18wZcxQaCUMPkK5hPrkSjRW8Ma89RKNbRn.tz"
  //   const act = "./archetype/mainnet/KT18wZcxQaCUMPkK5hPrkSjRW8Ma89RKNbRn.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18daeYAtnwLPoVB3kJcPiBLQXfvYEAZW5B', async () => {
  //   const ref = "./michelson/mainnet/KT18daeYAtnwLPoVB3kJcPiBLQXfvYEAZW5B.tz"
  //   const act = "./archetype/mainnet/KT18daeYAtnwLPoVB3kJcPiBLQXfvYEAZW5B.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18bWi21MtsdvZUpJE3pwY6enum16x3AXsD', async () => {
  //   const ref = "./michelson/mainnet/KT18bWi21MtsdvZUpJE3pwY6enum16x3AXsD.tz"
  //   const act = "./archetype/mainnet/KT18bWi21MtsdvZUpJE3pwY6enum16x3AXsD.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PmFvPuCBQ6xTkCfPobNfTURw4pZ8v2TV4', async () => {
  //   const ref = "./michelson/mainnet/KT1PmFvPuCBQ6xTkCfPobNfTURw4pZ8v2TV4.tz"
  //   const act = "./archetype/mainnet/KT1PmFvPuCBQ6xTkCfPobNfTURw4pZ8v2TV4.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Dh1hRVA6S8mHBVRi4ZxVUAka5sowZVwMm', async () => {
  //   const ref = "./michelson/mainnet/KT1Dh1hRVA6S8mHBVRi4ZxVUAka5sowZVwMm.tz"
  //   const act = "./archetype/mainnet/KT1Dh1hRVA6S8mHBVRi4ZxVUAka5sowZVwMm.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Gbu1Gm2U47Pmq9VP7ZMy3ZLKecodquAh4', async () => {
  //   const ref = "./michelson/mainnet/KT1Gbu1Gm2U47Pmq9VP7ZMy3ZLKecodquAh4.tz"
  //   const act = "./archetype/mainnet/KT1Gbu1Gm2U47Pmq9VP7ZMy3ZLKecodquAh4.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1TUx83WuwtA2Ku1pi6A9AZqov7CZfYtLUS', async () => {
  //   const ref = "./michelson/mainnet/KT1TUx83WuwtA2Ku1pi6A9AZqov7CZfYtLUS.tz"
  //   const act = "./archetype/mainnet/KT1TUx83WuwtA2Ku1pi6A9AZqov7CZfYtLUS.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FHAtLjG6S6tfjmrDeEySVLeP8a16T4Ngr', async () => {
  //   const ref = "./michelson/mainnet/KT1FHAtLjG6S6tfjmrDeEySVLeP8a16T4Ngr.tz"
  //   const act = "./archetype/mainnet/KT1FHAtLjG6S6tfjmrDeEySVLeP8a16T4Ngr.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1F2WE868QyHbXWV3ti7C3e7vHTdjAjN4TP', async () => {
  //   const ref = "./michelson/mainnet/KT1F2WE868QyHbXWV3ti7C3e7vHTdjAjN4TP.tz"
  //   const act = "./archetype/mainnet/KT1F2WE868QyHbXWV3ti7C3e7vHTdjAjN4TP.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY', async () => {
  //   const ref = "./michelson/mainnet/KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY.tz"
  //   const act = "./archetype/mainnet/KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1XCK3y54in6964u9UvhxxZgJ3Rhi8Jvmpm', async () => {
  //   const ref = "./michelson/mainnet/KT1XCK3y54in6964u9UvhxxZgJ3Rhi8Jvmpm.tz"
  //   const act = "./archetype/mainnet/KT1XCK3y54in6964u9UvhxxZgJ3Rhi8Jvmpm.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1KSuCpVFL592H5D9L4AGDRjJP88DeBaRdP', async () => {
  //   const ref = "./michelson/mainnet/KT1KSuCpVFL592H5D9L4AGDRjJP88DeBaRdP.tz"
  //   const act = "./archetype/mainnet/KT1KSuCpVFL592H5D9L4AGDRjJP88DeBaRdP.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AajLnzG5EyJSZfpSsn44iaHhwdm5AK85b', async () => {
  //   const ref = "./michelson/mainnet/KT1AajLnzG5EyJSZfpSsn44iaHhwdm5AK85b.tz"
  //   const act = "./archetype/mainnet/KT1AajLnzG5EyJSZfpSsn44iaHhwdm5AK85b.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FK5sW8JCQtVezjrWJTGwnZsdtP8AJZewp', async () => {
  //   const ref = "./michelson/mainnet/KT1FK5sW8JCQtVezjrWJTGwnZsdtP8AJZewp.tz"
  //   const act = "./archetype/mainnet/KT1FK5sW8JCQtVezjrWJTGwnZsdtP8AJZewp.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19kgnqC5VWoxktLRdRUERbyUPku9YioE8W', async () => {
  //   const ref = "./michelson/mainnet/KT19kgnqC5VWoxktLRdRUERbyUPku9YioE8W.tz"
  //   const act = "./archetype/mainnet/KT19kgnqC5VWoxktLRdRUERbyUPku9YioE8W.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Tuvu3JCXJWgoL8NLYfy38dMK7jxcdrFkZ', async () => {
  //   const ref = "./michelson/mainnet/KT1Tuvu3JCXJWgoL8NLYfy38dMK7jxcdrFkZ.tz"
  //   const act = "./archetype/mainnet/KT1Tuvu3JCXJWgoL8NLYfy38dMK7jxcdrFkZ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1EeN7JXS6VtJXmdnDKcXGxYoujqmxLFU5b', async () => {
  //   const ref = "./michelson/mainnet/KT1EeN7JXS6VtJXmdnDKcXGxYoujqmxLFU5b.tz"
  //   const act = "./archetype/mainnet/KT1EeN7JXS6VtJXmdnDKcXGxYoujqmxLFU5b.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1WQ6TM26S4hA3VAwvpj1b7D4Pd3jstYsxV', async () => {
  //   const ref = "./michelson/mainnet/KT1WQ6TM26S4hA3VAwvpj1b7D4Pd3jstYsxV.tz"
  //   const act = "./archetype/mainnet/KT1WQ6TM26S4hA3VAwvpj1b7D4Pd3jstYsxV.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1NRnZPgh4ZxD9UMBUQeVyprUxdKQLCiK3Z', async () => {
  //   const ref = "./michelson/mainnet/KT1NRnZPgh4ZxD9UMBUQeVyprUxdKQLCiK3Z.tz"
  //   const act = "./archetype/mainnet/KT1NRnZPgh4ZxD9UMBUQeVyprUxdKQLCiK3Z.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1G9UbzC4H5f4LJiBDCz9gZoURAzFGGYNZo', async () => {
  //   const ref = "./michelson/mainnet/KT1G9UbzC4H5f4LJiBDCz9gZoURAzFGGYNZo.tz"
  //   const act = "./archetype/mainnet/KT1G9UbzC4H5f4LJiBDCz9gZoURAzFGGYNZo.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Ehrn9URyNoPPiHbZ5mcpTbSazHgcV3zAQ', async () => {
  //   const ref = "./michelson/mainnet/KT1Ehrn9URyNoPPiHbZ5mcpTbSazHgcV3zAQ.tz"
  //   const act = "./archetype/mainnet/KT1Ehrn9URyNoPPiHbZ5mcpTbSazHgcV3zAQ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PyEJLNZPg4NrtkfLsR673FwcJEKPGaDrn', async () => {
  //   const ref = "./michelson/mainnet/KT1PyEJLNZPg4NrtkfLsR673FwcJEKPGaDrn.tz"
  //   const act = "./archetype/mainnet/KT1PyEJLNZPg4NrtkfLsR673FwcJEKPGaDrn.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Jz8MjnC3t6Prw5Dr6iiDBGmyMXkQsjPuy', async () => {
  //   const ref = "./michelson/mainnet/KT1Jz8MjnC3t6Prw5Dr6iiDBGmyMXkQsjPuy.tz"
  //   const act = "./archetype/mainnet/KT1Jz8MjnC3t6Prw5Dr6iiDBGmyMXkQsjPuy.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1LKhiZz9XFM9iRppLAdq58XoHK3eNdhhQ5', async () => {
  //   const ref = "./michelson/mainnet/KT1LKhiZz9XFM9iRppLAdq58XoHK3eNdhhQ5.tz"
  //   const act = "./archetype/mainnet/KT1LKhiZz9XFM9iRppLAdq58XoHK3eNdhhQ5.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1BgfSZizFUMBVGwbE9J8twjpTkjiMmQpKg', async () => {
  //   const ref = "./michelson/mainnet/KT1BgfSZizFUMBVGwbE9J8twjpTkjiMmQpKg.tz"
  //   const act = "./archetype/mainnet/KT1BgfSZizFUMBVGwbE9J8twjpTkjiMmQpKg.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1965CKA32PEQ352GqnXgPx93hXYu8q3u2N', async () => {
  //   const ref = "./michelson/mainnet/KT1965CKA32PEQ352GqnXgPx93hXYu8q3u2N.tz"
  //   const act = "./archetype/mainnet/KT1965CKA32PEQ352GqnXgPx93hXYu8q3u2N.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1B5VTw8ZSMnrjhy337CEvAm4tnT8Gu8Geu', async () => {
  //   const ref = "./michelson/mainnet/KT1B5VTw8ZSMnrjhy337CEvAm4tnT8Gu8Geu.tz"
  //   const act = "./archetype/mainnet/KT1B5VTw8ZSMnrjhy337CEvAm4tnT8Gu8Geu.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GfAzvH7aUtVPbqRw6WbYMbd77dFPErQUg', async () => {
  //   const ref = "./michelson/mainnet/KT1GfAzvH7aUtVPbqRw6WbYMbd77dFPErQUg.tz"
  //   const act = "./archetype/mainnet/KT1GfAzvH7aUtVPbqRw6WbYMbd77dFPErQUg.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1DUfaMfTRZZkvZAYQT5b3byXnvqoAykc43', async () => {
  //   const ref = "./michelson/mainnet/KT1DUfaMfTRZZkvZAYQT5b3byXnvqoAykc43.tz"
  //   const act = "./archetype/mainnet/KT1DUfaMfTRZZkvZAYQT5b3byXnvqoAykc43.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1G3UMEkhxso5cdx2fvoJRJu5nUjBWKMrET', async () => {
  //   const ref = "./michelson/mainnet/KT1G3UMEkhxso5cdx2fvoJRJu5nUjBWKMrET.tz"
  //   const act = "./archetype/mainnet/KT1G3UMEkhxso5cdx2fvoJRJu5nUjBWKMrET.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CbcKd5JoDRUnzKshdyBs57QVHuuYhmhgs', async () => {
  //   const ref = "./michelson/mainnet/KT1CbcKd5JoDRUnzKshdyBs57QVHuuYhmhgs.tz"
  //   const act = "./archetype/mainnet/KT1CbcKd5JoDRUnzKshdyBs57QVHuuYhmhgs.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Rc4bAFRiDQ1oDMhGTj1yhndZWo89D9GVq', async () => {
  //   const ref = "./michelson/mainnet/KT1Rc4bAFRiDQ1oDMhGTj1yhndZWo89D9GVq.tz"
  //   const act = "./archetype/mainnet/KT1Rc4bAFRiDQ1oDMhGTj1yhndZWo89D9GVq.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1JMFs8MitF3gceCUKZzEXzCTdkKYgZGkf3', async () => {
  //   const ref = "./michelson/mainnet/KT1JMFs8MitF3gceCUKZzEXzCTdkKYgZGkf3.tz"
  //   const act = "./archetype/mainnet/KT1JMFs8MitF3gceCUKZzEXzCTdkKYgZGkf3.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1HdMf7N7f6GgMuwieYeixpBwCsfvH6yXfW', async () => {
  //   const ref = "./michelson/mainnet/KT1HdMf7N7f6GgMuwieYeixpBwCsfvH6yXfW.tz"
  //   const act = "./archetype/mainnet/KT1HdMf7N7f6GgMuwieYeixpBwCsfvH6yXfW.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Ms8TbcweqwEkeViAXXMHkR3H9pwgLm6DR', async () => {
  //   const ref = "./michelson/mainnet/KT1Ms8TbcweqwEkeViAXXMHkR3H9pwgLm6DR.tz"
  //   const act = "./archetype/mainnet/KT1Ms8TbcweqwEkeViAXXMHkR3H9pwgLm6DR.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1KemKUx79keZgFW756jQrqKcZJ21y4SPdS', async () => {
  //   const ref = "./michelson/mainnet/KT1KemKUx79keZgFW756jQrqKcZJ21y4SPdS.tz"
  //   const act = "./archetype/mainnet/KT1KemKUx79keZgFW756jQrqKcZJ21y4SPdS.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1D37mb2FVxosduikQyeAreJgS1zRVWh8Uc', async () => {
  //   const ref = "./michelson/mainnet/KT1D37mb2FVxosduikQyeAreJgS1zRVWh8Uc.tz"
  //   const act = "./archetype/mainnet/KT1D37mb2FVxosduikQyeAreJgS1zRVWh8Uc.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GFQD8rrvH2ibiuPbzY46PumeS8GqrxtMw', async () => {
  //   const ref = "./michelson/mainnet/KT1GFQD8rrvH2ibiuPbzY46PumeS8GqrxtMw.tz"
  //   const act = "./archetype/mainnet/KT1GFQD8rrvH2ibiuPbzY46PumeS8GqrxtMw.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1KV8qF27sJNdaXx8QR1Th1HrUzRiDAPwbM', async () => {
  //   const ref = "./michelson/mainnet/KT1KV8qF27sJNdaXx8QR1Th1HrUzRiDAPwbM.tz"
  //   const act = "./archetype/mainnet/KT1KV8qF27sJNdaXx8QR1Th1HrUzRiDAPwbM.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1LGscYq8SwwbXXg1Z6ctDrHjMGnfEkjB8X', async () => {
  //   const ref = "./michelson/mainnet/KT1LGscYq8SwwbXXg1Z6ctDrHjMGnfEkjB8X.tz"
  //   const act = "./archetype/mainnet/KT1LGscYq8SwwbXXg1Z6ctDrHjMGnfEkjB8X.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1ChNsEFxwyCbJyWGSL3KdjeXE28AY1Kaog', async () => {
  //   const ref = "./michelson/mainnet/KT1ChNsEFxwyCbJyWGSL3KdjeXE28AY1Kaog.tz"
  //   const act = "./archetype/mainnet/KT1ChNsEFxwyCbJyWGSL3KdjeXE28AY1Kaog.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1UHX2yb1D64iEXMwfh1oMwjyDQoeAKPson', async () => {
  //   const ref = "./michelson/mainnet/KT1UHX2yb1D64iEXMwfh1oMwjyDQoeAKPson.tz"
  //   const act = "./archetype/mainnet/KT1UHX2yb1D64iEXMwfh1oMwjyDQoeAKPson.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1VG2WtYdSWz5E7chTeAdDPZNy2MpP8pTfL', async () => {
  //   const ref = "./michelson/mainnet/KT1VG2WtYdSWz5E7chTeAdDPZNy2MpP8pTfL.tz"
  //   const act = "./archetype/mainnet/KT1VG2WtYdSWz5E7chTeAdDPZNy2MpP8pTfL.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1BAgzAyBV8VZe26gbF7GFzDrAtnHq5YN11', async () => {
  //   const ref = "./michelson/mainnet/KT1BAgzAyBV8VZe26gbF7GFzDrAtnHq5YN11.tz"
  //   const act = "./archetype/mainnet/KT1BAgzAyBV8VZe26gbF7GFzDrAtnHq5YN11.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1NjxTshTx299m6toq3AgMwCf8ZdscWhn3C', async () => {
  //   const ref = "./michelson/mainnet/KT1NjxTshTx299m6toq3AgMwCf8ZdscWhn3C.tz"
  //   const act = "./archetype/mainnet/KT1NjxTshTx299m6toq3AgMwCf8ZdscWhn3C.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1XcAi9gaL5MUkFWoQjD7zZAohRSRukra9D', async () => {
  //   const ref = "./michelson/mainnet/KT1XcAi9gaL5MUkFWoQjD7zZAohRSRukra9D.tz"
  //   const act = "./archetype/mainnet/KT1XcAi9gaL5MUkFWoQjD7zZAohRSRukra9D.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1L5XZbKeMXFDJuwr1zcFzkamTWf7kk6LRd', async () => {
  //   const ref = "./michelson/mainnet/KT1L5XZbKeMXFDJuwr1zcFzkamTWf7kk6LRd.tz"
  //   const act = "./archetype/mainnet/KT1L5XZbKeMXFDJuwr1zcFzkamTWf7kk6LRd.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18aq2Qfvh7pTudu73DLYEarp8g78T2A9Qk', async () => {
  //   const ref = "./michelson/mainnet/KT18aq2Qfvh7pTudu73DLYEarp8g78T2A9Qk.tz"
  //   const act = "./archetype/mainnet/KT18aq2Qfvh7pTudu73DLYEarp8g78T2A9Qk.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1A56vz7tn4xYyM3KPR8DrzbFcgmpKKG3iu', async () => {
  //   const ref = "./michelson/mainnet/KT1A56vz7tn4xYyM3KPR8DrzbFcgmpKKG3iu.tz"
  //   const act = "./archetype/mainnet/KT1A56vz7tn4xYyM3KPR8DrzbFcgmpKKG3iu.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1SsLyfk8F3mRFZGJ8mSHw2fpd5gREm1XfN', async () => {
  //   const ref = "./michelson/mainnet/KT1SsLyfk8F3mRFZGJ8mSHw2fpd5gREm1XfN.tz"
  //   const act = "./archetype/mainnet/KT1SsLyfk8F3mRFZGJ8mSHw2fpd5gREm1XfN.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1SZxx9gHffYwoZqUMXjd49roywFvPU8tDK', async () => {
  //   const ref = "./michelson/mainnet/KT1SZxx9gHffYwoZqUMXjd49roywFvPU8tDK.tz"
  //   const act = "./archetype/mainnet/KT1SZxx9gHffYwoZqUMXjd49roywFvPU8tDK.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GQVru4AXYeH6aiSrbKnh1Tra8XfwXTYrL', async () => {
  //   const ref = "./michelson/mainnet/KT1GQVru4AXYeH6aiSrbKnh1Tra8XfwXTYrL.tz"
  //   const act = "./archetype/mainnet/KT1GQVru4AXYeH6aiSrbKnh1Tra8XfwXTYrL.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Nv5FFx3pYmSQofxc1SujRTCesf41b6q6H', async () => {
  //   const ref = "./michelson/mainnet/KT1Nv5FFx3pYmSQofxc1SujRTCesf41b6q6H.tz"
  //   const act = "./archetype/mainnet/KT1Nv5FFx3pYmSQofxc1SujRTCesf41b6q6H.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1UMJwse4X8pXjSX1THZCNTDYCCiVLi5Gdv', async () => {
  //   const ref = "./michelson/mainnet/KT1UMJwse4X8pXjSX1THZCNTDYCCiVLi5Gdv.tz"
  //   const act = "./archetype/mainnet/KT1UMJwse4X8pXjSX1THZCNTDYCCiVLi5Gdv.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GgUJwMQoFayRYNwamRAYCvHBLzgorLoGo', async () => {
  //   const ref = "./michelson/mainnet/KT1GgUJwMQoFayRYNwamRAYCvHBLzgorLoGo.tz"
  //   const act = "./archetype/mainnet/KT1GgUJwMQoFayRYNwamRAYCvHBLzgorLoGo.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1DLw2U9kWVhvRjBo6yjmgvj4gn8WWRbaVL', async () => {
  //   const ref = "./michelson/mainnet/KT1DLw2U9kWVhvRjBo6yjmgvj4gn8WWRbaVL.tz"
  //   const act = "./archetype/mainnet/KT1DLw2U9kWVhvRjBo6yjmgvj4gn8WWRbaVL.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1M6ETeNgsneoCvbtMNHo5q2YKwki9pbeub', async () => {
  //   const ref = "./michelson/mainnet/KT1M6ETeNgsneoCvbtMNHo5q2YKwki9pbeub.tz"
  //   const act = "./archetype/mainnet/KT1M6ETeNgsneoCvbtMNHo5q2YKwki9pbeub.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Sc2dHqCRLQV9gWLmUndQyowVrv12AtcJi', async () => {
  //   const ref = "./michelson/mainnet/KT1Sc2dHqCRLQV9gWLmUndQyowVrv12AtcJi.tz"
  //   const act = "./archetype/mainnet/KT1Sc2dHqCRLQV9gWLmUndQyowVrv12AtcJi.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1D27eaPDzKMHM1ZsAJNtAHQiHgSrsH32uA', async () => {
  //   const ref = "./michelson/mainnet/KT1D27eaPDzKMHM1ZsAJNtAHQiHgSrsH32uA.tz"
  //   const act = "./archetype/mainnet/KT1D27eaPDzKMHM1ZsAJNtAHQiHgSrsH32uA.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19b6BCikGACdN4uqffgSrwyJ19S2ySjveo', async () => {
  //   const ref = "./michelson/mainnet/KT19b6BCikGACdN4uqffgSrwyJ19S2ySjveo.tz"
  //   const act = "./archetype/mainnet/KT19b6BCikGACdN4uqffgSrwyJ19S2ySjveo.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1LedJHptp1wzu1GCbsu5RttCEBkk7pw7vu', async () => {
  //   const ref = "./michelson/mainnet/KT1LedJHptp1wzu1GCbsu5RttCEBkk7pw7vu.tz"
  //   const act = "./archetype/mainnet/KT1LedJHptp1wzu1GCbsu5RttCEBkk7pw7vu.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1XcR6yEbjjPKbJpo4PoM8bEnpuPTXNKK3N', async () => {
  //   const ref = "./michelson/mainnet/KT1XcR6yEbjjPKbJpo4PoM8bEnpuPTXNKK3N.tz"
  //   const act = "./archetype/mainnet/KT1XcR6yEbjjPKbJpo4PoM8bEnpuPTXNKK3N.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AJfwziXDgJcAmT5t2iRb422NmjYn1FCa3', async () => {
  //   const ref = "./michelson/mainnet/KT1AJfwziXDgJcAmT5t2iRb422NmjYn1FCa3.tz"
  //   const act = "./archetype/mainnet/KT1AJfwziXDgJcAmT5t2iRb422NmjYn1FCa3.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FcmdSroia1ys1WM6mL2rzB1haLxyy3MH9', async () => {
  //   const ref = "./michelson/mainnet/KT1FcmdSroia1ys1WM6mL2rzB1haLxyy3MH9.tz"
  //   const act = "./archetype/mainnet/KT1FcmdSroia1ys1WM6mL2rzB1haLxyy3MH9.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1WsEyYJEGqDHoJrgvRtXCGSCTfCjPNFSpk', async () => {
  //   const ref = "./michelson/mainnet/KT1WsEyYJEGqDHoJrgvRtXCGSCTfCjPNFSpk.tz"
  //   const act = "./archetype/mainnet/KT1WsEyYJEGqDHoJrgvRtXCGSCTfCjPNFSpk.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1S7FxsB6WAnUXANq3F2hu2rKXqy6tcJVR4', async () => {
  //   const ref = "./michelson/mainnet/KT1S7FxsB6WAnUXANq3F2hu2rKXqy6tcJVR4.tz"
  //   const act = "./archetype/mainnet/KT1S7FxsB6WAnUXANq3F2hu2rKXqy6tcJVR4.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1NQfJvo9v8hXmEgqos8NP7sS8V4qaEfvRF', async () => {
  //   const ref = "./michelson/mainnet/KT1NQfJvo9v8hXmEgqos8NP7sS8V4qaEfvRF.tz"
  //   const act = "./archetype/mainnet/KT1NQfJvo9v8hXmEgqos8NP7sS8V4qaEfvRF.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CSYNJ6dFcnsV4QJ6HnBFtdif8LJGPQiDM', async () => {
  //   const ref = "./michelson/mainnet/KT1CSYNJ6dFcnsV4QJ6HnBFtdif8LJGPQiDM.tz"
  //   const act = "./archetype/mainnet/KT1CSYNJ6dFcnsV4QJ6HnBFtdif8LJGPQiDM.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1REHQ183LzfoVoqiDR87mCrt7CLUH1MbcV', async () => {
  //   const ref = "./michelson/mainnet/KT1REHQ183LzfoVoqiDR87mCrt7CLUH1MbcV.tz"
  //   const act = "./archetype/mainnet/KT1REHQ183LzfoVoqiDR87mCrt7CLUH1MbcV.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19nHqEWZxFFbbDL1b7Y86escgEN7qUShGo', async () => {
  //   const ref = "./michelson/mainnet/KT19nHqEWZxFFbbDL1b7Y86escgEN7qUShGo.tz"
  //   const act = "./archetype/mainnet/KT19nHqEWZxFFbbDL1b7Y86escgEN7qUShGo.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1REpgCXL6WRSoGsajB2X7bxGrE3YE8VsbQ', async () => {
  //   const ref = "./michelson/mainnet/KT1REpgCXL6WRSoGsajB2X7bxGrE3YE8VsbQ.tz"
  //   const act = "./archetype/mainnet/KT1REpgCXL6WRSoGsajB2X7bxGrE3YE8VsbQ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1BPrMaPxtKo2ip9vhgAfEP1QdhCETYyemj', async () => {
  //   const ref = "./michelson/mainnet/KT1BPrMaPxtKo2ip9vhgAfEP1QdhCETYyemj.tz"
  //   const act = "./archetype/mainnet/KT1BPrMaPxtKo2ip9vhgAfEP1QdhCETYyemj.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1JZe6ak4yV3PJFR91uJYRn4D36Bd2sEUT1', async () => {
  //   const ref = "./michelson/mainnet/KT1JZe6ak4yV3PJFR91uJYRn4D36Bd2sEUT1.tz"
  //   const act = "./archetype/mainnet/KT1JZe6ak4yV3PJFR91uJYRn4D36Bd2sEUT1.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1KFM29fupjvsXekWRREjN9uXSUvjmNjEDw', async () => {
  //   const ref = "./michelson/mainnet/KT1KFM29fupjvsXekWRREjN9uXSUvjmNjEDw.tz"
  //   const act = "./archetype/mainnet/KT1KFM29fupjvsXekWRREjN9uXSUvjmNjEDw.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Ap287P1NzsnToSJdA4aqSNjPomRaHBZSr', async () => {
  //   const ref = "./michelson/mainnet/KT1Ap287P1NzsnToSJdA4aqSNjPomRaHBZSr.tz"
  //   const act = "./archetype/mainnet/KT1Ap287P1NzsnToSJdA4aqSNjPomRaHBZSr.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GaTgYj4fmMm2JbmhQfRXX1ZZJX4vWPFWB', async () => {
  //   const ref = "./michelson/mainnet/KT1GaTgYj4fmMm2JbmhQfRXX1ZZJX4vWPFWB.tz"
  //   const act = "./archetype/mainnet/KT1GaTgYj4fmMm2JbmhQfRXX1ZZJX4vWPFWB.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FXmd8TsZZesSLPEfJkb4s6bcpLqo4zXKs', async () => {
  //   const ref = "./michelson/mainnet/KT1FXmd8TsZZesSLPEfJkb4s6bcpLqo4zXKs.tz"
  //   const act = "./archetype/mainnet/KT1FXmd8TsZZesSLPEfJkb4s6bcpLqo4zXKs.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1DRfVGaiXiSSCXJSQwCYSjkgmrPQeFRDd9', async () => {
  //   const ref = "./michelson/mainnet/KT1DRfVGaiXiSSCXJSQwCYSjkgmrPQeFRDd9.tz"
  //   const act = "./archetype/mainnet/KT1DRfVGaiXiSSCXJSQwCYSjkgmrPQeFRDd9.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19GoNgcUorknBRr7hpW7XcvJzH4FKs63k6', async () => {
  //   const ref = "./michelson/mainnet/KT19GoNgcUorknBRr7hpW7XcvJzH4FKs63k6.tz"
  //   const act = "./archetype/mainnet/KT19GoNgcUorknBRr7hpW7XcvJzH4FKs63k6.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1WJ1tKARGLmEhrUyLyTUjXdBfaEQQjyvkZ', async () => {
  //   const ref = "./michelson/mainnet/KT1WJ1tKARGLmEhrUyLyTUjXdBfaEQQjyvkZ.tz"
  //   const act = "./archetype/mainnet/KT1WJ1tKARGLmEhrUyLyTUjXdBfaEQQjyvkZ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19n2cM72XnAbBuNrW8vWamFFt6AGJP6R9A', async () => {
  //   const ref = "./michelson/mainnet/KT19n2cM72XnAbBuNrW8vWamFFt6AGJP6R9A.tz"
  //   const act = "./archetype/mainnet/KT19n2cM72XnAbBuNrW8vWamFFt6AGJP6R9A.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18hbep6w7ynhk6FiXkqVyCqTXSL6gCJv8s', async () => {
  //   const ref = "./michelson/mainnet/KT18hbep6w7ynhk6FiXkqVyCqTXSL6gCJv8s.tz"
  //   const act = "./archetype/mainnet/KT18hbep6w7ynhk6FiXkqVyCqTXSL6gCJv8s.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1XFVGGeW6uj2x81GhpV74isogCyU8tYz3M', async () => {
  //   const ref = "./michelson/mainnet/KT1XFVGGeW6uj2x81GhpV74isogCyU8tYz3M.tz"
  //   const act = "./archetype/mainnet/KT1XFVGGeW6uj2x81GhpV74isogCyU8tYz3M.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CkArDT3YjY6GJFuxjhqeY6pBkyQZ2MVA2', async () => {
  //   const ref = "./michelson/mainnet/KT1CkArDT3YjY6GJFuxjhqeY6pBkyQZ2MVA2.tz"
  //   const act = "./archetype/mainnet/KT1CkArDT3YjY6GJFuxjhqeY6pBkyQZ2MVA2.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1VmnipmAyttWiaAGJGB9ovGZyzGVp5rYFE', async () => {
  //   const ref = "./michelson/mainnet/KT1VmnipmAyttWiaAGJGB9ovGZyzGVp5rYFE.tz"
  //   const act = "./archetype/mainnet/KT1VmnipmAyttWiaAGJGB9ovGZyzGVp5rYFE.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1RKXjb3Wcy3aofZFU77dhN6JSjj5gWRgdc', async () => {
  //   const ref = "./michelson/mainnet/KT1RKXjb3Wcy3aofZFU77dhN6JSjj5gWRgdc.tz"
  //   const act = "./archetype/mainnet/KT1RKXjb3Wcy3aofZFU77dhN6JSjj5gWRgdc.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FcqRTa9y2RFjTCTEsn5T9LWfe2VMQY8TD', async () => {
  //   const ref = "./michelson/mainnet/KT1FcqRTa9y2RFjTCTEsn5T9LWfe2VMQY8TD.tz"
  //   const act = "./archetype/mainnet/KT1FcqRTa9y2RFjTCTEsn5T9LWfe2VMQY8TD.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1DquFH1fqBmDmTmaSRumiMdb7AZqarUTBJ', async () => {
  //   const ref = "./michelson/mainnet/KT1DquFH1fqBmDmTmaSRumiMdb7AZqarUTBJ.tz"
  //   const act = "./archetype/mainnet/KT1DquFH1fqBmDmTmaSRumiMdb7AZqarUTBJ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1V9hXfa36F4Zy75CRJH4BNSa3r1L59Lj7J', async () => {
  //   const ref = "./michelson/mainnet/KT1V9hXfa36F4Zy75CRJH4BNSa3r1L59Lj7J.tz"
  //   const act = "./archetype/mainnet/KT1V9hXfa36F4Zy75CRJH4BNSa3r1L59Lj7J.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1BESj6UfiHbHGQo2aWzktRjxguBd1mrbYG', async () => {
  //   const ref = "./michelson/mainnet/KT1BESj6UfiHbHGQo2aWzktRjxguBd1mrbYG.tz"
  //   const act = "./archetype/mainnet/KT1BESj6UfiHbHGQo2aWzktRjxguBd1mrbYG.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18d8c1WjhkBUbcY5XHpK5NQJzruXzAghdC', async () => {
  //   const ref = "./michelson/mainnet/KT18d8c1WjhkBUbcY5XHpK5NQJzruXzAghdC.tz"
  //   const act = "./archetype/mainnet/KT18d8c1WjhkBUbcY5XHpK5NQJzruXzAghdC.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1BePYYc7bnzMvGBp5E9gKfg2dT1L2U3HZg', async () => {
  //   const ref = "./michelson/mainnet/KT1BePYYc7bnzMvGBp5E9gKfg2dT1L2U3HZg.tz"
  //   const act = "./archetype/mainnet/KT1BePYYc7bnzMvGBp5E9gKfg2dT1L2U3HZg.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Mhari6wUqxoqCan6hL7c6XVocd4rjCkZd', async () => {
  //   const ref = "./michelson/mainnet/KT1Mhari6wUqxoqCan6hL7c6XVocd4rjCkZd.tz"
  //   const act = "./archetype/mainnet/KT1Mhari6wUqxoqCan6hL7c6XVocd4rjCkZd.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Tw9RoAGvYC1mjaPNpqbiwpvPm9aHm83J2', async () => {
  //   const ref = "./michelson/mainnet/KT1Tw9RoAGvYC1mjaPNpqbiwpvPm9aHm83J2.tz"
  //   const act = "./archetype/mainnet/KT1Tw9RoAGvYC1mjaPNpqbiwpvPm9aHm83J2.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1UDc2ZUoAAvv8amw2DqVuQK1fKjb1HjxR4', async () => {
  //   const ref = "./michelson/mainnet/KT1UDc2ZUoAAvv8amw2DqVuQK1fKjb1HjxR4.tz"
  //   const act = "./archetype/mainnet/KT1UDc2ZUoAAvv8amw2DqVuQK1fKjb1HjxR4.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FbkiY8Y1gSh4x9QVzfvtcUrXEQAx7wYnf', async () => {
  //   const ref = "./michelson/mainnet/KT1FbkiY8Y1gSh4x9QVzfvtcUrXEQAx7wYnf.tz"
  //   const act = "./archetype/mainnet/KT1FbkiY8Y1gSh4x9QVzfvtcUrXEQAx7wYnf.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1RA48D7YPmS1bcpfhZKsN6DpZbC4oAxpVW', async () => {
  //   const ref = "./michelson/mainnet/KT1RA48D7YPmS1bcpfhZKsN6DpZbC4oAxpVW.tz"
  //   const act = "./archetype/mainnet/KT1RA48D7YPmS1bcpfhZKsN6DpZbC4oAxpVW.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1MQE6F5ute3834xpX53GwuiGSoE51sAEA7', async () => {
  //   const ref = "./michelson/mainnet/KT1MQE6F5ute3834xpX53GwuiGSoE51sAEA7.tz"
  //   const act = "./archetype/mainnet/KT1MQE6F5ute3834xpX53GwuiGSoE51sAEA7.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1R3uoZ6W1ZxEwzqtv75Ro7DhVY6UAcxuK2', async () => {
  //   const ref = "./michelson/mainnet/KT1R3uoZ6W1ZxEwzqtv75Ro7DhVY6UAcxuK2.tz"
  //   const act = "./archetype/mainnet/KT1R3uoZ6W1ZxEwzqtv75Ro7DhVY6UAcxuK2.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1JkE4T6umrTh15kKSyJ8cLjNu2cdd6QtNj', async () => {
  //   const ref = "./michelson/mainnet/KT1JkE4T6umrTh15kKSyJ8cLjNu2cdd6QtNj.tz"
  //   const act = "./archetype/mainnet/KT1JkE4T6umrTh15kKSyJ8cLjNu2cdd6QtNj.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1QtUUB4StHDjTNJb1VSZL8ZquLDNbvgnbA', async () => {
  //   const ref = "./michelson/mainnet/KT1QtUUB4StHDjTNJb1VSZL8ZquLDNbvgnbA.tz"
  //   const act = "./archetype/mainnet/KT1QtUUB4StHDjTNJb1VSZL8ZquLDNbvgnbA.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1ATqnoYSjtiq8UNAUrQmhDX2hEeLVZtgAK', async () => {
  //   const ref = "./michelson/mainnet/KT1ATqnoYSjtiq8UNAUrQmhDX2hEeLVZtgAK.tz"
  //   const act = "./archetype/mainnet/KT1ATqnoYSjtiq8UNAUrQmhDX2hEeLVZtgAK.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1HLp5stiwTRqpA2kdPACpCxjT9XsegrLk3', async () => {
  //   const ref = "./michelson/mainnet/KT1HLp5stiwTRqpA2kdPACpCxjT9XsegrLk3.tz"
  //   const act = "./archetype/mainnet/KT1HLp5stiwTRqpA2kdPACpCxjT9XsegrLk3.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1EzDckZPgPxdizLqS5USBotQzoVwSLoXWo', async () => {
  //   const ref = "./michelson/mainnet/KT1EzDckZPgPxdizLqS5USBotQzoVwSLoXWo.tz"
  //   const act = "./archetype/mainnet/KT1EzDckZPgPxdizLqS5USBotQzoVwSLoXWo.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT18jF2bCerNgrkyk7qd1Bpk9gKnpPKJAvjB', async () => {
  //   const ref = "./michelson/mainnet/KT18jF2bCerNgrkyk7qd1Bpk9gKnpPKJAvjB.tz"
  //   const act = "./archetype/mainnet/KT18jF2bCerNgrkyk7qd1Bpk9gKnpPKJAvjB.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1WRUe3csC1jiThN9KUtaji2bd412upfn1E', async () => {
  //   const ref = "./michelson/mainnet/KT1WRUe3csC1jiThN9KUtaji2bd412upfn1E.tz"
  //   const act = "./archetype/mainnet/KT1WRUe3csC1jiThN9KUtaji2bd412upfn1E.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1JBrvoEawvPM1HvSMwybXVMuHGTXjN9oGu', async () => {
  //   const ref = "./michelson/mainnet/KT1JBrvoEawvPM1HvSMwybXVMuHGTXjN9oGu.tz"
  //   const act = "./archetype/mainnet/KT1JBrvoEawvPM1HvSMwybXVMuHGTXjN9oGu.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GbWiFga8U9ikdxNMeXWoTrx65sCg9MNaU', async () => {
  //   const ref = "./michelson/mainnet/KT1GbWiFga8U9ikdxNMeXWoTrx65sCg9MNaU.tz"
  //   const act = "./archetype/mainnet/KT1GbWiFga8U9ikdxNMeXWoTrx65sCg9MNaU.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1GG1EPtFNqYs2DnwQi9PPSo3CuM7UTDGHZ', async () => {
  //   const ref = "./michelson/mainnet/KT1GG1EPtFNqYs2DnwQi9PPSo3CuM7UTDGHZ.tz"
  //   const act = "./archetype/mainnet/KT1GG1EPtFNqYs2DnwQi9PPSo3CuM7UTDGHZ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1HQmKekWoUfA1ZDFmiSB91qxaAM8uoxkX1', async () => {
  //   const ref = "./michelson/mainnet/KT1HQmKekWoUfA1ZDFmiSB91qxaAM8uoxkX1.tz"
  //   const act = "./archetype/mainnet/KT1HQmKekWoUfA1ZDFmiSB91qxaAM8uoxkX1.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1HS4h6r1WnHVqsCbZELpC92y4ugrZRFhkT', async () => {
  //   const ref = "./michelson/mainnet/KT1HS4h6r1WnHVqsCbZELpC92y4ugrZRFhkT.tz"
  //   const act = "./archetype/mainnet/KT1HS4h6r1WnHVqsCbZELpC92y4ugrZRFhkT.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AyXjyqM48s7RPYTVUMVDm7oPLD8DsoGva', async () => {
  //   const ref = "./michelson/mainnet/KT1AyXjyqM48s7RPYTVUMVDm7oPLD8DsoGva.tz"
  //   const act = "./archetype/mainnet/KT1AyXjyqM48s7RPYTVUMVDm7oPLD8DsoGva.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Bk9otVCwHpYtcwCsUiPNHBf2Zx7VUuCuS', async () => {
  //   const ref = "./michelson/mainnet/KT1Bk9otVCwHpYtcwCsUiPNHBf2Zx7VUuCuS.tz"
  //   const act = "./archetype/mainnet/KT1Bk9otVCwHpYtcwCsUiPNHBf2Zx7VUuCuS.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9', async () => {
  //   const ref = "./michelson/mainnet/KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9.tz"
  //   const act = "./archetype/mainnet/KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1ETPG89SUW4qnuR7WpjcNju9wyjWcjY2W7', async () => {
  //   const ref = "./michelson/mainnet/KT1ETPG89SUW4qnuR7WpjcNju9wyjWcjY2W7.tz"
  //   const act = "./archetype/mainnet/KT1ETPG89SUW4qnuR7WpjcNju9wyjWcjY2W7.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1HRTG12Tj2QSjg1HkSVMXS6uoFaDSCy7cs', async () => {
  //   const ref = "./michelson/mainnet/KT1HRTG12Tj2QSjg1HkSVMXS6uoFaDSCy7cs.tz"
  //   const act = "./archetype/mainnet/KT1HRTG12Tj2QSjg1HkSVMXS6uoFaDSCy7cs.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1SovvF5KRQjBiVX8cHFmEoMc7H54ehxstV', async () => {
  //   const ref = "./michelson/mainnet/KT1SovvF5KRQjBiVX8cHFmEoMc7H54ehxstV.tz"
  //   const act = "./archetype/mainnet/KT1SovvF5KRQjBiVX8cHFmEoMc7H54ehxstV.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1MQ9zy7MTGo4v9uUrMNq2XyukSxX2JDYr1', async () => {
  //   const ref = "./michelson/mainnet/KT1MQ9zy7MTGo4v9uUrMNq2XyukSxX2JDYr1.tz"
  //   const act = "./archetype/mainnet/KT1MQ9zy7MTGo4v9uUrMNq2XyukSxX2JDYr1.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AdbYiPYb5hDuEuVrfxmFehtnBCXv4Np7r', async () => {
  //   const ref = "./michelson/mainnet/KT1AdbYiPYb5hDuEuVrfxmFehtnBCXv4Np7r.tz"
  //   const act = "./archetype/mainnet/KT1AdbYiPYb5hDuEuVrfxmFehtnBCXv4Np7r.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1J6jfcX6DwN8oNXmNoH9ryYcHbekav1vbM', async () => {
  //   const ref = "./michelson/mainnet/KT1J6jfcX6DwN8oNXmNoH9ryYcHbekav1vbM.tz"
  //   const act = "./archetype/mainnet/KT1J6jfcX6DwN8oNXmNoH9ryYcHbekav1vbM.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1XRJqs3xVNk3pE7FmB8JP8d4HWwwXpwFhK', async () => {
  //   const ref = "./michelson/mainnet/KT1XRJqs3xVNk3pE7FmB8JP8d4HWwwXpwFhK.tz"
  //   const act = "./archetype/mainnet/KT1XRJqs3xVNk3pE7FmB8JP8d4HWwwXpwFhK.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1X5Xk2kJHEfcRMRSpXVUzGgGcgKYGue5LT', async () => {
  //   const ref = "./michelson/mainnet/KT1X5Xk2kJHEfcRMRSpXVUzGgGcgKYGue5LT.tz"
  //   const act = "./archetype/mainnet/KT1X5Xk2kJHEfcRMRSpXVUzGgGcgKYGue5LT.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1LHk9bnv2xoj9JSw7mQqfQe6wEqY9ma8RM', async () => {
  //   const ref = "./michelson/mainnet/KT1LHk9bnv2xoj9JSw7mQqfQe6wEqY9ma8RM.tz"
  //   const act = "./archetype/mainnet/KT1LHk9bnv2xoj9JSw7mQqfQe6wEqY9ma8RM.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT19t1Ac1Ja815tY7WU2GKVqm4r8keva1MBt', async () => {
  //   const ref = "./michelson/mainnet/KT19t1Ac1Ja815tY7WU2GKVqm4r8keva1MBt.tz"
  //   const act = "./archetype/mainnet/KT19t1Ac1Ja815tY7WU2GKVqm4r8keva1MBt.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1EofZBeCtMXwQoiqhYEDZpwiBEt3NkUGGd', async () => {
  //   const ref = "./michelson/mainnet/KT1EofZBeCtMXwQoiqhYEDZpwiBEt3NkUGGd.tz"
  //   const act = "./archetype/mainnet/KT1EofZBeCtMXwQoiqhYEDZpwiBEt3NkUGGd.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CBVrTh1i64Mwm1iv1kdcQqJfgFbPaCjZn', async () => {
  //   const ref = "./michelson/mainnet/KT1CBVrTh1i64Mwm1iv1kdcQqJfgFbPaCjZn.tz"
  //   const act = "./archetype/mainnet/KT1CBVrTh1i64Mwm1iv1kdcQqJfgFbPaCjZn.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Ep5RX2VVyP7zD3kYhEzJyJgfb8oBSUv2H', async () => {
  //   const ref = "./michelson/mainnet/KT1Ep5RX2VVyP7zD3kYhEzJyJgfb8oBSUv2H.tz"
  //   const act = "./archetype/mainnet/KT1Ep5RX2VVyP7zD3kYhEzJyJgfb8oBSUv2H.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AKNCvvGN8QEiL6bd8UHDXq4tmiNRsKYs9', async () => {
  //   const ref = "./michelson/mainnet/KT1AKNCvvGN8QEiL6bd8UHDXq4tmiNRsKYs9.tz"
  //   const act = "./archetype/mainnet/KT1AKNCvvGN8QEiL6bd8UHDXq4tmiNRsKYs9.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1AzrrdKcZQ7ApazLcya2VV83WaDrqbvSZr', async () => {
  //   const ref = "./michelson/mainnet/KT1AzrrdKcZQ7ApazLcya2VV83WaDrqbvSZr.tz"
  //   const act = "./archetype/mainnet/KT1AzrrdKcZQ7ApazLcya2VV83WaDrqbvSZr.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1Ag29Dotu8iEigVrCARu7jmW5gkF1RyHUB', async () => {
  //   const ref = "./michelson/mainnet/KT1Ag29Dotu8iEigVrCARu7jmW5gkF1RyHUB.tz"
  //   const act = "./archetype/mainnet/KT1Ag29Dotu8iEigVrCARu7jmW5gkF1RyHUB.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1MPb3oRXNS5ko9gYEhUZjoEGDerpax1auZ', async () => {
  //   const ref = "./michelson/mainnet/KT1MPb3oRXNS5ko9gYEhUZjoEGDerpax1auZ.tz"
  //   const act = "./archetype/mainnet/KT1MPb3oRXNS5ko9gYEhUZjoEGDerpax1auZ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn', async () => {
  //   const ref = "./michelson/mainnet/KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn.tz"
  //   const act = "./archetype/mainnet/KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1KzoKR7v1HjF2JqfYAWFV2ihzmUVJsDqXy', async () => {
  //   const ref = "./michelson/mainnet/KT1KzoKR7v1HjF2JqfYAWFV2ihzmUVJsDqXy.tz"
  //   const act = "./archetype/mainnet/KT1KzoKR7v1HjF2JqfYAWFV2ihzmUVJsDqXy.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1LnWyiJpbgHpesMskFdsmmu7Wgy6Dudusa', async () => {
  //   const ref = "./michelson/mainnet/KT1LnWyiJpbgHpesMskFdsmmu7Wgy6Dudusa.tz"
  //   const act = "./archetype/mainnet/KT1LnWyiJpbgHpesMskFdsmmu7Wgy6Dudusa.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1VZLvWDdnnj8QsnZxKviYjkTs7eXvUprpL', async () => {
  //   const ref = "./michelson/mainnet/KT1VZLvWDdnnj8QsnZxKviYjkTs7eXvUprpL.tz"
  //   const act = "./archetype/mainnet/KT1VZLvWDdnnj8QsnZxKviYjkTs7eXvUprpL.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1ACMUWqzNdxFxkwNsovSnTyXXJfntg4AQy', async () => {
  //   const ref = "./michelson/mainnet/KT1ACMUWqzNdxFxkwNsovSnTyXXJfntg4AQy.tz"
  //   const act = "./archetype/mainnet/KT1ACMUWqzNdxFxkwNsovSnTyXXJfntg4AQy.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1A1N85VE2Mi3zuDvKidWNy6P6Fj4iRz2rA', async () => {
  //   const ref = "./michelson/mainnet/KT1A1N85VE2Mi3zuDvKidWNy6P6Fj4iRz2rA.tz"
  //   const act = "./archetype/mainnet/KT1A1N85VE2Mi3zuDvKidWNy6P6Fj4iRz2rA.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg', async () => {
  //   const ref = "./michelson/mainnet/KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg.tz"
  //   const act = "./archetype/mainnet/KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1WhdeSqfRPyPPuaWWGA5JSykStj5D3W9ux', async () => {
  //   const ref = "./michelson/mainnet/KT1WhdeSqfRPyPPuaWWGA5JSykStj5D3W9ux.tz"
  //   const act = "./archetype/mainnet/KT1WhdeSqfRPyPPuaWWGA5JSykStj5D3W9ux.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ', async () => {
  //   const ref = "./michelson/mainnet/KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ.tz"
  //   const act = "./archetype/mainnet/KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1DrJV8vhkdLEj76h1H9Q4irZDqAkMPo1Qf', async () => {
  //   const ref = "./michelson/mainnet/KT1DrJV8vhkdLEj76h1H9Q4irZDqAkMPo1Qf.tz"
  //   const act = "./archetype/mainnet/KT1DrJV8vhkdLEj76h1H9Q4irZDqAkMPo1Qf.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1EctCuorV2NfVb1XTQgvzJ88MQtWP8cMMv', async () => {
  //   const ref = "./michelson/mainnet/KT1EctCuorV2NfVb1XTQgvzJ88MQtWP8cMMv.tz"
  //   const act = "./archetype/mainnet/KT1EctCuorV2NfVb1XTQgvzJ88MQtWP8cMMv.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1T1tZRqU7DuLf6qsMFxBFFXqLsAG3qhXxY', async () => {
  //   const ref = "./michelson/mainnet/KT1T1tZRqU7DuLf6qsMFxBFFXqLsAG3qhXxY.tz"
  //   const act = "./archetype/mainnet/KT1T1tZRqU7DuLf6qsMFxBFFXqLsAG3qhXxY.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1FftKSYijGge8JF5N5ytyPX4hWzJXwGNFv', async () => {
  //   const ref = "./michelson/mainnet/KT1FftKSYijGge8JF5N5ytyPX4hWzJXwGNFv.tz"
  //   const act = "./archetype/mainnet/KT1FftKSYijGge8JF5N5ytyPX4hWzJXwGNFv.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1PDUV54BBGrg3zwpWaeFYNMUDgbYDPEXvJ', async () => {
  //   const ref = "./michelson/mainnet/KT1PDUV54BBGrg3zwpWaeFYNMUDgbYDPEXvJ.tz"
  //   const act = "./archetype/mainnet/KT1PDUV54BBGrg3zwpWaeFYNMUDgbYDPEXvJ.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1TnwBxgK4ayHuxrti6KKkJpWBHXBYRCX6H', async () => {
  //   const ref = "./michelson/mainnet/KT1TnwBxgK4ayHuxrti6KKkJpWBHXBYRCX6H.tz"
  //   const act = "./archetype/mainnet/KT1TnwBxgK4ayHuxrti6KKkJpWBHXBYRCX6H.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

  // it('KT1UbqtneoB9H2xPrjrJg7SJTKJ57S2cQTYi', async () => {
  //   const ref = "./michelson/mainnet/KT1UbqtneoB9H2xPrjrJg7SJTKJ57S2cQTYi.tz"
  //   const act = "./archetype/mainnet/KT1UbqtneoB9H2xPrjrJg7SJTKJ57S2cQTYi.arl"
  //   await check_prelude(ref, act)
  //   assert(false) // TODO
  // })

})