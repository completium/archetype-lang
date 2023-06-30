/* DO NOT EDIT, GENERATED FILE */
import assert from 'assert'

/* Utils ------------------------------------------------------------------- */

const compile = (p : string) => {
  const spawn = require('cross-spawn');
  const bin = '../_build/default/src/compiler.exe'
  const res = spawn.sync(bin, ['-d', '-mi', p], { });
  return res
}

/* Tests ------------------------------------------------------------------- */

describe('decomp-mic-mainnet', async () => {
  it('KT18d8c1WjhkBUbcY5XHpK5NQJzruXzAghdC', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT18d8c1WjhkBUbcY5XHpK5NQJzruXzAghdC.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT18dDtdzvEj3Y9okKJxASMCyWfSscRfjtbP', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT18dDtdzvEj3Y9okKJxASMCyWfSscRfjtbP.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT18iWQLZDeJ5tTCLrfnPNip2KSZHmFJFUc7', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT18iWQLZDeJ5tTCLrfnPNip2KSZHmFJFUc7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT18jF2bCerNgrkyk7qd1Bpk9gKnpPKJAvjB', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT18jF2bCerNgrkyk7qd1Bpk9gKnpPKJAvjB.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT18ju2Pk6YvNqhD8tsRXGGMRfDVfFkqfhYA', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT18ju2Pk6YvNqhD8tsRXGGMRfDVfFkqfhYA.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT18wngSoTUqEJiNaYuhcrfYCtsczLUVVkTp', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT18wngSoTUqEJiNaYuhcrfYCtsczLUVVkTp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT18xeXDYR2FzKi8PxiC48mfdAChbnaXubbe', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT18xeXDYR2FzKi8PxiC48mfdAChbnaXubbe.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT192oBTQjqQKnLeryezRAyLHsWP5AhA9SxE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT192oBTQjqQKnLeryezRAyLHsWP5AhA9SxE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT197cMAmydiH3QH7Xjqqrf8PgX7Xq5FyDat', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT197cMAmydiH3QH7Xjqqrf8PgX7Xq5FyDat.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19949GQgb9e11oXGrC2iMjABnUzRRP6UfZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19949GQgb9e11oXGrC2iMjABnUzRRP6UfZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19GoNgcUorknBRr7hpW7XcvJzH4FKs63k6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19GoNgcUorknBRr7hpW7XcvJzH4FKs63k6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19Yp8mRkkkPmUJhsbee2Xuqb6hH6wGFb8Q', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19Yp8mRkkkPmUJhsbee2Xuqb6hH6wGFb8Q.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19b6BCikGACdN4uqffgSrwyJ19S2ySjveo', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19b6BCikGACdN4uqffgSrwyJ19S2ySjveo.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19gE62t4H1vqMD7UXZWFr28j16tBnPYtMa', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19gE62t4H1vqMD7UXZWFr28j16tBnPYtMa.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19kgnqC5VWoxktLRdRUERbyUPku9YioE8W', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19kgnqC5VWoxktLRdRUERbyUPku9YioE8W.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19n2cM72XnAbBuNrW8vWamFFt6AGJP6R9A', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19n2cM72XnAbBuNrW8vWamFFt6AGJP6R9A.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19nHqEWZxFFbbDL1b7Y86escgEN7qUShGo', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19nHqEWZxFFbbDL1b7Y86escgEN7qUShGo.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19ovJhcsUn4YU8Q5L3BGovKSixfbWcecEA', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19ovJhcsUn4YU8Q5L3BGovKSixfbWcecEA.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19p7iHW5ooJLx3jdv6zCE6Nr6YyCVNawcs', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19p7iHW5ooJLx3jdv6zCE6Nr6YyCVNawcs.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT19ptNzn4MVAN45KUUNpyL5AdLVhujk815u', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT19ptNzn4MVAN45KUUNpyL5AdLVhujk815u.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1A1N85VE2Mi3zuDvKidWNy6P6Fj4iRz2rA', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1A1N85VE2Mi3zuDvKidWNy6P6Fj4iRz2rA.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1A56vz7tn4xYyM3KPR8DrzbFcgmpKKG3iu', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1A56vz7tn4xYyM3KPR8DrzbFcgmpKKG3iu.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AB4eCmreUZwYniTq64LQsc4QS5DpiT6QW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AB4eCmreUZwYniTq64LQsc4QS5DpiT6QW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ABV9RM3axh6PDjwPWdndrisSweathi9uw', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ABV9RM3axh6PDjwPWdndrisSweathi9uw.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AFCfQN9UqNVVDuNpBZ5zVoGHuE15L9Npm', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AFCfQN9UqNVVDuNpBZ5zVoGHuE15L9Npm.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AJfwziXDgJcAmT5t2iRb422NmjYn1FCa3', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AJfwziXDgJcAmT5t2iRb422NmjYn1FCa3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AKNCvvGN8QEiL6bd8UHDXq4tmiNRsKYs9', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AKNCvvGN8QEiL6bd8UHDXq4tmiNRsKYs9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ATFPkGjCbgN8HtWrsx4sucRR96Cy4Enfg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ATFPkGjCbgN8HtWrsx4sucRR96Cy4Enfg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ATqnoYSjtiq8UNAUrQmhDX2hEeLVZtgAK', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ATqnoYSjtiq8UNAUrQmhDX2hEeLVZtgAK.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ATyL9xzmMcg97XZQspj7ACMhWPUYBKzFt', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ATyL9xzmMcg97XZQspj7ACMhWPUYBKzFt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AUaGsGAkiYgH5wXvQ2tR8JV5dTkenM8XN', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AUaGsGAkiYgH5wXvQ2tR8JV5dTkenM8XN.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ab4zDfjmsMinwqR6fUWTUg4uiLUEnxCkd', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ab4zDfjmsMinwqR6fUWTUg4uiLUEnxCkd.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AbYeDbjjcAnV1QK7EZUUdqku77CdkTuv6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AbYeDbjjcAnV1QK7EZUUdqku77CdkTuv6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AbjG7vtpV8osdoJXcMRck8eTwst8dWoz4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AbjG7vtpV8osdoJXcMRck8eTwst8dWoz4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Acfs1M5FXHGYQpvdKUwGbZtrUkqrisweJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Acfs1M5FXHGYQpvdKUwGbZtrUkqrisweJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ahfc4xCzYW1BskLg4gNuzCkhDhSfbBZxL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ahfc4xCzYW1BskLg4gNuzCkhDhSfbBZxL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ajpb7anQaJqTKSXz6cqqQbWfRefUUrS82', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ajpb7anQaJqTKSXz6cqqQbWfRefUUrS82.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AoqB6oVkjGi1cWJpp8vZmHd94dSGivNzp', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AoqB6oVkjGi1cWJpp8vZmHd94dSGivNzp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ap287P1NzsnToSJdA4aqSNjPomRaHBZSr', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ap287P1NzsnToSJdA4aqSNjPomRaHBZSr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Avd4SfQT7CezSiGYXFgHNKqSyWstYRz53', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Avd4SfQT7CezSiGYXFgHNKqSyWstYRz53.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1AzYk5bnehTAqYp3i8PBzwCZfBQzWGe3fP', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1AzYk5bnehTAqYp3i8PBzwCZfBQzWGe3fP.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1B5VTw8ZSMnrjhy337CEvAm4tnT8Gu8Geu', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1B5VTw8ZSMnrjhy337CEvAm4tnT8Gu8Geu.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1B5Y35Dqt88CLRTsH18Dw2gAnQuwCB91F1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1B5Y35Dqt88CLRTsH18Dw2gAnQuwCB91F1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BNuzMP54AoPNWQF45LkjhcFjW3A48rcHb', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BNuzMP54AoPNWQF45LkjhcFjW3A48rcHb.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BRudFZEXLYANgmZTka1xCDN5nWTMWY7SZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BRudFZEXLYANgmZTka1xCDN5nWTMWY7SZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BePYYc7bnzMvGBp5E9gKfg2dT1L2U3HZg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BePYYc7bnzMvGBp5E9gKfg2dT1L2U3HZg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BidpSguT62g8oyrhyCkBRWuV259HE41fZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BidpSguT62g8oyrhyCkBRWuV259HE41fZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Bn7Hwx6o6oeqCWZYbq4Xh1z57NVR9uRSS', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Bn7Hwx6o6oeqCWZYbq4Xh1z57NVR9uRSS.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BvVxWM6cjFuJNet4R9m64VDCN2iMvjuGE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BvVxWM6cjFuJNet4R9m64VDCN2iMvjuGE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BvvmDsw1VodPssAfdDUY7V78u4qEQrCWD', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BvvmDsw1VodPssAfdDUY7V78u4qEQrCWD.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BwB2qWsaRsTJKZb4u1xoLCHBMNXWu1sZw', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BwB2qWsaRsTJKZb4u1xoLCHBMNXWu1sZw.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1BwxoxUJEnGcrWtfz2CiCZ19KN1xiN7dNF', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1BwxoxUJEnGcrWtfz2CiCZ19KN1xiN7dNF.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1C8qi9AhK4QXr5CfxK252vxR3dJPYUnGH6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1C8qi9AhK4QXr5CfxK252vxR3dJPYUnGH6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CCmPESfp7t484HhgMkakuBQteKb1LKVKk', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CCmPESfp7t484HhgMkakuBQteKb1LKVKk.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CMfPN5SekTkaNd11ij65HNK2BX3PoYtrJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CMfPN5SekTkaNd11ij65HNK2BX3PoYtrJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CMuXyLx7MjkYPjue5LRZdfxD5qqsBgnQu', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CMuXyLx7MjkYPjue5LRZdfxD5qqsBgnQu.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CSYNJ6dFcnsV4QJ6HnBFtdif8LJGPQiDM', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CSYNJ6dFcnsV4QJ6HnBFtdif8LJGPQiDM.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CbcKd5JoDRUnzKshdyBs57QVHuuYhmhgs', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CbcKd5JoDRUnzKshdyBs57QVHuuYhmhgs.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CcizgAUXomE1dqvGb3KdEsxFHCWsvuyuz', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CcizgAUXomE1dqvGb3KdEsxFHCWsvuyuz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CeLuUxpZKNF4tDASWt1BabXWcVSLhQn1C', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CeLuUxpZKNF4tDASWt1BabXWcVSLhQn1C.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ChJ6h8Crjdfds99DLpE5USynQTmCJtB3T', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ChJ6h8Crjdfds99DLpE5USynQTmCJtB3T.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ChNsEFxwyCbJyWGSL3KdjeXE28AY1Kaog', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ChNsEFxwyCbJyWGSL3KdjeXE28AY1Kaog.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CjfCztmRpsyUee1nLa9Wcpfr7vgwqRZmk', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CjfCztmRpsyUee1nLa9Wcpfr7vgwqRZmk.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CkArDT3YjY6GJFuxjhqeY6pBkyQZ2MVA2', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CkArDT3YjY6GJFuxjhqeY6pBkyQZ2MVA2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CkmTrnRFrn5JKyAaKpzhEf3Rc5Nv81bsx', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CkmTrnRFrn5JKyAaKpzhEf3Rc5Nv81bsx.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CpeSQKdkhWi4pinYcseCFKmDhs5M74BkU', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CpeSQKdkhWi4pinYcseCFKmDhs5M74BkU.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CstdKc9TtdDPkSy9dQVZLMYPCYrzTonSB', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CstdKc9TtdDPkSy9dQVZLMYPCYrzTonSB.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1CvzXrz19fnHKuWedFY3WqmVAB7kMTPLLS', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1CvzXrz19fnHKuWedFY3WqmVAB7kMTPLLS.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1D27eaPDzKMHM1ZsAJNtAHQiHgSrsH32uA', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1D27eaPDzKMHM1ZsAJNtAHQiHgSrsH32uA.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1D37mb2FVxosduikQyeAreJgS1zRVWh8Uc', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1D37mb2FVxosduikQyeAreJgS1zRVWh8Uc.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1D66GWbmhw9sn6CLJSJs95jGn5YTuCcfYW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1D66GWbmhw9sn6CLJSJs95jGn5YTuCcfYW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DFqmqiZeeJigNdq4SdEVR1gykk1V7eed4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DFqmqiZeeJigNdq4SdEVR1gykk1V7eed4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DGTathSM8Hcxxdm6m6ogxwCmDrFCJrhhs', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DGTathSM8Hcxxdm6m6ogxwCmDrFCJrhhs.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DLvtWesWb9F29MKUPDJpNzrTj9YxBkUQX', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DLvtWesWb9F29MKUPDJpNzrTj9YxBkUQX.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DLw2U9kWVhvRjBo6yjmgvj4gn8WWRbaVL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DLw2U9kWVhvRjBo6yjmgvj4gn8WWRbaVL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DRfVGaiXiSSCXJSQwCYSjkgmrPQeFRDd9', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DRfVGaiXiSSCXJSQwCYSjkgmrPQeFRDd9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DUfaMfTRZZkvZAYQT5b3byXnvqoAykc43', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DUfaMfTRZZkvZAYQT5b3byXnvqoAykc43.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DVec3RZ2qEhtdzCkm2VhQCcTR961PET6A', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DVec3RZ2qEhtdzCkm2VhQCcTR961PET6A.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DVkpd3UKJMe496e3fcB2ZZDjr1wvWPEcc', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DVkpd3UKJMe496e3fcB2ZZDjr1wvWPEcc.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DaYTreX1LUY6cmrneZkAu3rwCXktnvqvp', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DaYTreX1LUY6cmrneZkAu3rwCXktnvqvp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DgfA9saiQYhqXivu8KKHWCEKtBCbbbXqH', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DgfA9saiQYhqXivu8KKHWCEKtBCbbbXqH.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DqDp4cdCrQ1ULbDMg2h54Te3TVE4MUZyE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DqDp4cdCrQ1ULbDMg2h54Te3TVE4MUZyE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DquFH1fqBmDmTmaSRumiMdb7AZqarUTBJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DquFH1fqBmDmTmaSRumiMdb7AZqarUTBJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1DsBffbR1JTvSw8idmNXAe9qTCsLvGJB64', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1DsBffbR1JTvSw8idmNXAe9qTCsLvGJB64.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Dvx2TMCec1sVm53H3wzfobeKsjoFhSwuC', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Dvx2TMCec1sVm53H3wzfobeKsjoFhSwuC.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1E2NUZoMUdxwtqPqDPZifMcdXeLWGP3rvB', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1E2NUZoMUdxwtqPqDPZifMcdXeLWGP3rvB.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EH8yKXkRoxNkULRB1dSuwhkKyi5LJH82o', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EH8yKXkRoxNkULRB1dSuwhkKyi5LJH82o.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EQX67Ct1xygDBdhQ3aLvS6DbMj9B1PZkA', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EQX67Ct1xygDBdhQ3aLvS6DbMj9B1PZkA.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ETPG89SUW4qnuR7WpjcNju9wyjWcjY2W7', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ETPG89SUW4qnuR7WpjcNju9wyjWcjY2W7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EctCuorV2NfVb1XTQgvzJ88MQtWP8cMMv', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EctCuorV2NfVb1XTQgvzJ88MQtWP8cMMv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EeN7JXS6VtJXmdnDKcXGxYoujqmxLFU5b', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EeN7JXS6VtJXmdnDKcXGxYoujqmxLFU5b.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EhTDnyFoRs3J6giL9gWJJEsPKY6uJ6asZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EhTDnyFoRs3J6giL9gWJJEsPKY6uJ6asZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ei4QkvFWjCnQah3oxxAd36jdn3LQ9ydM7', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ei4QkvFWjCnQah3oxxAd36jdn3LQ9ydM7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EofZBeCtMXwQoiqhYEDZpwiBEt3NkUGGd', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EofZBeCtMXwQoiqhYEDZpwiBEt3NkUGGd.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ep5RX2VVyP7zD3kYhEzJyJgfb8oBSUv2H', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ep5RX2VVyP7zD3kYhEzJyJgfb8oBSUv2H.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EpGgjQs73QfFJs9z7m1Mxm5MTnpC2tqse', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EpGgjQs73QfFJs9z7m1Mxm5MTnpC2tqse.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EpQVwqLGSH7vMCWKJnq6Uxi851sEDbhWL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EpQVwqLGSH7vMCWKJnq6Uxi851sEDbhWL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EpihM8tQSBwqYB6NtCT8N67pq8rKwoD93', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EpihM8tQSBwqYB6NtCT8N67pq8rKwoD93.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ex8LrDbCrZuTgmWin8eEo7HFw74jAqTvz', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ex8LrDbCrZuTgmWin8eEo7HFw74jAqTvz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ExDNHeoxNsNJJzjxrctxRZ5c8eWwRkr7v', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ExDNHeoxNsNJJzjxrctxRZ5c8eWwRkr7v.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1EzDckZPgPxdizLqS5USBotQzoVwSLoXWo', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1EzDckZPgPxdizLqS5USBotQzoVwSLoXWo.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1F25MTKpQJF8xJXVCNhweGmsxHtAjCDTFx', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1F25MTKpQJF8xJXVCNhweGmsxHtAjCDTFx.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FBhqB85ACSpXTzLUTGVa2tT5iAPYtmNDJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FBhqB85ACSpXTzLUTGVa2tT5iAPYtmNDJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FCdgksuWJPLKfkgLU4BGxEjDyPBMgs4Rw', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FCdgksuWJPLKfkgLU4BGxEjDyPBMgs4Rw.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FHAtLjG6S6tfjmrDeEySVLeP8a16T4Ngr', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FHAtLjG6S6tfjmrDeEySVLeP8a16T4Ngr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FK5sW8JCQtVezjrWJTGwnZsdtP8AJZewp', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FK5sW8JCQtVezjrWJTGwnZsdtP8AJZewp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FWEZVq2WgqZnbWmxwM4c8RCHc558sxPvS', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FWEZVq2WgqZnbWmxwM4c8RCHc558sxPvS.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FYoPoDSzRM74Zfug1wcpnoi6oayEz7KND', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FYoPoDSzRM74Zfug1wcpnoi6oayEz7KND.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FaKvzjgVGZtiA7yyx97txY8J5cE5qpjQ1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FaKvzjgVGZtiA7yyx97txY8J5cE5qpjQ1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FcqRTa9y2RFjTCTEsn5T9LWfe2VMQY8TD', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FcqRTa9y2RFjTCTEsn5T9LWfe2VMQY8TD.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FftKSYijGge8JF5N5ytyPX4hWzJXwGNFv', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FftKSYijGge8JF5N5ytyPX4hWzJXwGNFv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Frdoq1pyxw3b5V4nXmSVYeWngXAABY7Sn', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Frdoq1pyxw3b5V4nXmSVYeWngXAABY7Sn.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Fswo1Bzys4zjLYMMZZfizKzdbYiBVKoTW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Fswo1Bzys4zjLYMMZZfizKzdbYiBVKoTW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Fv5xCoUqEeb2TycB7ijXdAXUFH4uPnRNN', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Fv5xCoUqEeb2TycB7ijXdAXUFH4uPnRNN.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1FyaDqiMQWg7Exo7VUiXAgZbd2kCzo3d4s', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1FyaDqiMQWg7Exo7VUiXAgZbd2kCzo3d4s.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1G2kcUaZjFbbB6qzNpQCGdmJr9Xyu1tL5o', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1G2kcUaZjFbbB6qzNpQCGdmJr9Xyu1tL5o.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GALBSRLbY3iNb1P1Dzbdrx1Phu9d9f4Xv', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GALBSRLbY3iNb1P1Dzbdrx1Phu9d9f4Xv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GBmWKvbr4U15uGhvyLCiyaZif5i7AYeDY', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GBmWKvbr4U15uGhvyLCiyaZif5i7AYeDY.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GFB7AbbJGaX2ZKERJS56D77yVqjoQiMtz', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GFB7AbbJGaX2ZKERJS56D77yVqjoQiMtz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GFQD8rrvH2ibiuPbzY46PumeS8GqrxtMw', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GFQD8rrvH2ibiuPbzY46PumeS8GqrxtMw.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GFVf3ZVLiSxgU5EqBrmboEEU16prAdeJQ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GFVf3ZVLiSxgU5EqBrmboEEU16prAdeJQ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GG1EPtFNqYs2DnwQi9PPSo3CuM7UTDGHZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GG1EPtFNqYs2DnwQi9PPSo3CuM7UTDGHZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GHNuuVmE5KzmPZ6df5pPmXTdudSoVtBpx', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GHNuuVmE5KzmPZ6df5pPmXTdudSoVtBpx.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GQVru4AXYeH6aiSrbKnh1Tra8XfwXTYrL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GQVru4AXYeH6aiSrbKnh1Tra8XfwXTYrL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GaTgYj4fmMm2JbmhQfRXX1ZZJX4vWPFWB', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GaTgYj4fmMm2JbmhQfRXX1ZZJX4vWPFWB.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Gbu1Gm2U47Pmq9VP7ZMy3ZLKecodquAh4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Gbu1Gm2U47Pmq9VP7ZMy3ZLKecodquAh4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GcQJjWhbzvsxwrSeJYEgndiHsQTZ1rNf8', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GcQJjWhbzvsxwrSeJYEgndiHsQTZ1rNf8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GdTbW7vxjEFheHtgF6hdye2ooFc3A1Dh5', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GdTbW7vxjEFheHtgF6hdye2ooFc3A1Dh5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GetVcigbLbWExeb6BqxHtZCbPGndJX2Xg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GetVcigbLbWExeb6BqxHtZCbPGndJX2Xg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GfAzvH7aUtVPbqRw6WbYMbd77dFPErQUg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GfAzvH7aUtVPbqRw6WbYMbd77dFPErQUg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GgUJwMQoFayRYNwamRAYCvHBLzgorLoGo', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GgUJwMQoFayRYNwamRAYCvHBLzgorLoGo.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GtQiP1AQ7zjL7teSTmDBaPu3ueJAUH264', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GtQiP1AQ7zjL7teSTmDBaPu3ueJAUH264.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GtSi4TKn9i5X1sNCJijQQKQp7ANutWNxw', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GtSi4TKn9i5X1sNCJijQQKQp7ANutWNxw.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GuLMziKGoekEPseAjr4arL88bnCuHg4Bh', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GuLMziKGoekEPseAjr4arL88bnCuHg4Bh.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Gw23KjwqWixhEHUocjqJgmSdCRDsyGHAX', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Gw23KjwqWixhEHUocjqJgmSdCRDsyGHAX.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1GyCkoqwnVQoSYwVKBxMAKprdgXsbkBHa9', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1GyCkoqwnVQoSYwVKBxMAKprdgXsbkBHa9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Gz2D9cB5WRk45a3KUb1CSJ34DTfAUPN7T', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Gz2D9cB5WRk45a3KUb1CSJ34DTfAUPN7T.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1H1EYPpivygggzc3jsv9SZXzCCCp6qAq7t', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1H1EYPpivygggzc3jsv9SZXzCCCp6qAq7t.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1H28iie4mW9LmmJeYLjH6zkC8wwSmfHf5P', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1H28iie4mW9LmmJeYLjH6zkC8wwSmfHf5P.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1H6FUVoLSABZZod7h8HEvqUaXfweJPwXLR', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1H6FUVoLSABZZod7h8HEvqUaXfweJPwXLR.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1H7ybGEKqsgUEvPja26rQfzWxw8wji3BY8', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1H7ybGEKqsgUEvPja26rQfzWxw8wji3BY8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HGWymKFrVKKc2U4cFgCWaXsbMsH3x39sD', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HGWymKFrVKKc2U4cFgCWaXsbMsH3x39sD.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HLhNpBQaKqVn65T1uLz4VATeZrxygdB78', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HLhNpBQaKqVn65T1uLz4VATeZrxygdB78.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HPeid5ZGEuwrpySzxauw7eGUGc8ZaAg7T', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HPeid5ZGEuwrpySzxauw7eGUGc8ZaAg7T.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HQmKekWoUfA1ZDFmiSB91qxaAM8uoxkX1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HQmKekWoUfA1ZDFmiSB91qxaAM8uoxkX1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HYsh4BFcKzdm212zVjNrmQN4MfcHGNBqE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HYsh4BFcKzdm212zVjNrmQN4MfcHGNBqE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HkcGxHDdT7t1xUjnWE7sQoV2DEYXg3GWW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HkcGxHDdT7t1xUjnWE7sQoV2DEYXg3GWW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HtEsA6VgskvHg7KmJiM52Sx3pBXRgQmRA', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HtEsA6VgskvHg7KmJiM52Sx3pBXRgQmRA.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1HvpCCHvC9c4iNzAa6rx4MqNsmPRJf6CEw', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1HvpCCHvC9c4iNzAa6rx4MqNsmPRJf6CEw.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1J5dqegz1qYaYc7X3KjynYL9St1wcZ8ZyV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1J5dqegz1qYaYc7X3KjynYL9St1wcZ8ZyV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1J623FNZ6an8NHkWFbtvm5bKXgFzhBc5Zf', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1J623FNZ6an8NHkWFbtvm5bKXgFzhBc5Zf.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1J6bTtBT7hFB4VqETsdvYDzZMb39uJsXae', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1J6bTtBT7hFB4VqETsdvYDzZMb39uJsXae.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JBrvoEawvPM1HvSMwybXVMuHGTXjN9oGu', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JBrvoEawvPM1HvSMwybXVMuHGTXjN9oGu.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JMFs8MitF3gceCUKZzEXzCTdkKYgZGkf3', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JMFs8MitF3gceCUKZzEXzCTdkKYgZGkf3.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JVJuqYUY15mWPvsQYcz89NxUmxYKFcB7g', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JVJuqYUY15mWPvsQYcz89NxUmxYKFcB7g.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JZe6ak4yV3PJFR91uJYRn4D36Bd2sEUT1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JZe6ak4yV3PJFR91uJYRn4D36Bd2sEUT1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JdufSdfg3WyxWJcCRNsBFV9V3x9TQBkJ2', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JdufSdfg3WyxWJcCRNsBFV9V3x9TQBkJ2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JeHfZyZxa75QCz9KvSajiyA4qfomyzFS2', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JeHfZyZxa75QCz9KvSajiyA4qfomyzFS2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JgND55drqnUQ3H7Q7AWxRSxdNM3qs5UJS', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JgND55drqnUQ3H7Q7AWxRSxdNM3qs5UJS.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JkE4T6umrTh15kKSyJ8cLjNu2cdd6QtNj', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JkE4T6umrTh15kKSyJ8cLjNu2cdd6QtNj.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JnU6thJd5A1hW5mGaCa2sWBuSkriTKbqq', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JnU6thJd5A1hW5mGaCa2sWBuSkriTKbqq.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Jpvv5APKoncTq39tabQ44bqrqLGpQuQha', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Jpvv5APKoncTq39tabQ44bqrqLGpQuQha.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1JtBpbx9ZSUrmfdLbqxtycTQCzozUbT2fj', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1JtBpbx9ZSUrmfdLbqxtycTQCzozUbT2fj.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Jz8MjnC3t6Prw5Dr6iiDBGmyMXkQsjPuy', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Jz8MjnC3t6Prw5Dr6iiDBGmyMXkQsjPuy.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1K5JcJeWmnvjdPcFi8yTytWsYu6boLEPBg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1K5JcJeWmnvjdPcFi8yTytWsYu6boLEPBg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1K5npkpWK6wxkcBg97dZD77c2J7DmWvxSb', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1K5npkpWK6wxkcBg97dZD77c2J7DmWvxSb.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1K7whn5yHucGXMN7ymfKiX5r534QeaJM29', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1K7whn5yHucGXMN7ymfKiX5r534QeaJM29.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1K9gCRgaLRFKTErYt1wVxA3Frb9FjasjTV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1K9gCRgaLRFKTErYt1wVxA3Frb9FjasjTV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KAiw9Ak8RmiUrmDbUjiMb6keQUPgJp3mz', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KAiw9Ak8RmiUrmDbUjiMb6keQUPgJp3mz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KFM29fupjvsXekWRREjN9uXSUvjmNjEDw', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KFM29fupjvsXekWRREjN9uXSUvjmNjEDw.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KR47vPVf5absGPTu3WxMy4idoypfRG3vW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KR47vPVf5absGPTu3WxMy4idoypfRG3vW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KV8qF27sJNdaXx8QR1Th1HrUzRiDAPwbM', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KV8qF27sJNdaXx8QR1Th1HrUzRiDAPwbM.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KYWFZtuaJwwmNscLw7ipUqiq62RHvhuCG', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KYWFZtuaJwwmNscLw7ipUqiq62RHvhuCG.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KdyHZKwHq19iDV1jj1ftqb7jz8Tu6v5Vq', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KdyHZKwHq19iDV1jj1ftqb7jz8Tu6v5Vq.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Kfbk3B6NYPCPohPBDU3Hxf5Xeyy9PdkNp', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Kfbk3B6NYPCPohPBDU3Hxf5Xeyy9PdkNp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KsXrBSAKqeQeYsdNpwJjByUaciHfKvJBV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KsXrBSAKqeQeYsdNpwJjByUaciHfKvJBV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KtXx8MmRWJmSCtuDBNckzp5jQmfPBZ9fK', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KtXx8MmRWJmSCtuDBNckzp5jQmfPBZ9fK.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1KzoKR7v1HjF2JqfYAWFV2ihzmUVJsDqXy', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1KzoKR7v1HjF2JqfYAWFV2ihzmUVJsDqXy.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1L16VBmW8tkovhLmonhfvf6dtTZya6k6af', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1L16VBmW8tkovhLmonhfvf6dtTZya6k6af.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1L5XZbKeMXFDJuwr1zcFzkamTWf7kk6LRd', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1L5XZbKeMXFDJuwr1zcFzkamTWf7kk6LRd.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1L7PkmBuD1eJY6WjCbUj9qVLYzEXVKbT6S', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1L7PkmBuD1eJY6WjCbUj9qVLYzEXVKbT6S.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LCDCSGBHdKcxp7ZsRKNqmdgYourimhCmG', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LCDCSGBHdKcxp7ZsRKNqmdgYourimhCmG.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LGCVzePeHZB4jHTdAdrieMPvgahd2x9Qz', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LGCVzePeHZB4jHTdAdrieMPvgahd2x9Qz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LGscYq8SwwbXXg1Z6ctDrHjMGnfEkjB8X', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LGscYq8SwwbXXg1Z6ctDrHjMGnfEkjB8X.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LHk9bnv2xoj9JSw7mQqfQe6wEqY9ma8RM', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LHk9bnv2xoj9JSw7mQqfQe6wEqY9ma8RM.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LKhiZz9XFM9iRppLAdq58XoHK3eNdhhQ5', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LKhiZz9XFM9iRppLAdq58XoHK3eNdhhQ5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LMhkcSWmnYJZHzRmDg9NRaUwiio2nvazq', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LMhkcSWmnYJZHzRmDg9NRaUwiio2nvazq.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Ldn1XWQmk7J4pYgGFjjwV57Ew8NYvcNtJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Ldn1XWQmk7J4pYgGFjjwV57Ew8NYvcNtJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LhNu3v6rCa3Ura3bompAAJZD9io5VRaWZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LhNu3v6rCa3Ura3bompAAJZD9io5VRaWZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Lj4y492KN1zDyeeKR2HG74SR2j5tcenMV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Lj4y492KN1zDyeeKR2HG74SR2j5tcenMV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Lmy1YpMSFH6APuxcZAekSNTYWdtti9s7J', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Lmy1YpMSFH6APuxcZAekSNTYWdtti9s7J.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LnWyiJpbgHpesMskFdsmmu7Wgy6Dudusa', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LnWyiJpbgHpesMskFdsmmu7Wgy6Dudusa.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1LyHDYnML5eCuTEVCTynUpivwG6ns6khiG', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1LyHDYnML5eCuTEVCTynUpivwG6ns6khiG.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1M2Gng6zuDEy7PFivXPDYgrNnYEfLsc1z1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1M2Gng6zuDEy7PFivXPDYgrNnYEfLsc1z1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1M6ETeNgsneoCvbtMNHo5q2YKwki9pbeub', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1M6ETeNgsneoCvbtMNHo5q2YKwki9pbeub.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1M6thU61gFS3osWh9UhypLjn2NrGMEL85N', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1M6thU61gFS3osWh9UhypLjn2NrGMEL85N.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1M7gYMvKJKU2EbEixmtfcakriBHx1qUDKU', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1M7gYMvKJKU2EbEixmtfcakriBHx1qUDKU.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MDpipLT6DaPQZKSJtCM2N572vwGbSp62n', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MDpipLT6DaPQZKSJtCM2N572vwGbSp62n.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MHDHRLugz3A4qP6KqZDpa7FFmZfcJauV4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MHDHRLugz3A4qP6KqZDpa7FFmZfcJauV4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MHfvmULc6mAGJLit9NbjxtQWFACKqJXQL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MHfvmULc6mAGJLit9NbjxtQWFACKqJXQL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MMLb2FVrrE9Do74J3FH1RNNc4QhDuVCNX', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MMLb2FVrrE9Do74J3FH1RNNc4QhDuVCNX.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MQE6F5ute3834xpX53GwuiGSoE51sAEA7', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MQE6F5ute3834xpX53GwuiGSoE51sAEA7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MWpEoeXh4DqxxCKad3867XXetF2EBxan1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MWpEoeXh4DqxxCKad3867XXetF2EBxan1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MdeS1LnHHnjiXy1PB9f8Rk5TQzYrZbF6A', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MdeS1LnHHnjiXy1PB9f8Rk5TQzYrZbF6A.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Mgy95DVzqVBNYhsW93cyHuB57Q94UFhrh', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Mgy95DVzqVBNYhsW93cyHuB57Q94UFhrh.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Mhari6wUqxoqCan6hL7c6XVocd4rjCkZd', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Mhari6wUqxoqCan6hL7c6XVocd4rjCkZd.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MiSxkVDFDrAMYCZZXdBEkNrf1NWzfnnRR', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MiSxkVDFDrAMYCZZXdBEkNrf1NWzfnnRR.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Mq9mPHSgrotWmoWnyMw11cRmAsvNn1yXf', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Mq9mPHSgrotWmoWnyMw11cRmAsvNn1yXf.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Muk5DHuCxtNzNEaswpQQYPxz2MHqX8pGE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Muk5DHuCxtNzNEaswpQQYPxz2MHqX8pGE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MvtUZBsxhzMcFhUfWfMr4uBeXzgxfRbWg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MvtUZBsxhzMcFhUfWfMr4uBeXzgxfRbWg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Mx5sFU4BZqnAaJRpMzqaPbd2qMCFmcqea', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Mx5sFU4BZqnAaJRpMzqaPbd2qMCFmcqea.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1MzfYSbq18fYr4f44aQRoZBQN72BAtiz5j', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1MzfYSbq18fYr4f44aQRoZBQN72BAtiz5j.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1N1wwNPqT5jGhM91GQ2ae5uY8UzFaXHMJS', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1N1wwNPqT5jGhM91GQ2ae5uY8UzFaXHMJS.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1N8KZ2JETP6ku7kqt7yC8CtUdsRg3z9Dbb', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1N8KZ2JETP6ku7kqt7yC8CtUdsRg3z9Dbb.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NRnZPgh4ZxD9UMBUQeVyprUxdKQLCiK3Z', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NRnZPgh4ZxD9UMBUQeVyprUxdKQLCiK3Z.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NUhpSixECn15ceV81nHbSY92DmudsD9RL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NUhpSixECn15ceV81nHbSY92DmudsD9RL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Nh9pCoWdQFPT2gXBJNComX63KiZhjqJbi', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Nh9pCoWdQFPT2gXBJNComX63KiZhjqJbi.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NhRxsiUq3AXirM3fKjaF9qZ2vWmGQDMmu', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NhRxsiUq3AXirM3fKjaF9qZ2vWmGQDMmu.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NhUUX1yCjyBuiHyaJme6rsPNKHmizKb7e', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NhUUX1yCjyBuiHyaJme6rsPNKHmizKb7e.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NhtHwHD5cqabfSdwg1Fowud5f175eShwx', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NhtHwHD5cqabfSdwg1Fowud5f175eShwx.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NjxTshTx299m6toq3AgMwCf8ZdscWhn3C', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NjxTshTx299m6toq3AgMwCf8ZdscWhn3C.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NmoofGosSaWFKgAbt7AMTqnV1xfqeAhLT', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NmoofGosSaWFKgAbt7AMTqnV1xfqeAhLT.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1NtWLeCJYASUoHm6erMwLV6BAT6Mw5cQC6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1NtWLeCJYASUoHm6erMwLV6BAT6Mw5cQC6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Nv5FFx3pYmSQofxc1SujRTCesf41b6q6H', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Nv5FFx3pYmSQofxc1SujRTCesf41b6q6H.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PAV4ayvsDYi9zBFsLepnkPkpEspeYefNX', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PAV4ayvsDYi9zBFsLepnkPkpEspeYefNX.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PDUV54BBGrg3zwpWaeFYNMUDgbYDPEXvJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PDUV54BBGrg3zwpWaeFYNMUDgbYDPEXvJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PFtWiYgFmJHcdstouZ4mxheU6qWaHXmuF', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PFtWiYgFmJHcdstouZ4mxheU6qWaHXmuF.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PJHDLSHrx2T8VQ5ab6VoUVQPmdTvPX4Ze', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PJHDLSHrx2T8VQ5ab6VoUVQPmdTvPX4Ze.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PYnvMA8Tso6G5LjHufuEemNJQXKfnxdGf', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PYnvMA8Tso6G5LjHufuEemNJQXKfnxdGf.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PYpcopEjbt8A5LY4YF7Fd7VYV3GTqcbx6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PYpcopEjbt8A5LY4YF7Fd7VYV3GTqcbx6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PdsBwNpkn6syQQzkjZPpaDFxWDZvcVqZZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PdsBwNpkn6syQQzkjZPpaDFxWDZvcVqZZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PewsC6imsd9tfSLTdv3E6vF7aNEXJis1f', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PewsC6imsd9tfSLTdv3E6vF7aNEXJis1f.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PhV2KXC1Nbu4qaJkcYZirzKqjNtjkNyiC', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PhV2KXC1Nbu4qaJkcYZirzKqjNtjkNyiC.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PmFvPuCBQ6xTkCfPobNfTURw4pZ8v2TV4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PmFvPuCBQ6xTkCfPobNfTURw4pZ8v2TV4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PohHkaJF1xj4k3v7JABQXJQYwFTpvi7C7', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PohHkaJF1xj4k3v7JABQXJQYwFTpvi7C7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PpouVgF59r7MBG58DWFd4j2pyV2q9msyV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PpouVgF59r7MBG58DWFd4j2pyV2q9msyV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PskqjPYaP6M21jYA2bEfUbSyicNLyE43Q', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PskqjPYaP6M21jYA2bEfUbSyicNLyE43Q.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PuPcFhmCmeV6MR5geCUX5TeWrCYjDQzig', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PuPcFhmCmeV6MR5geCUX5TeWrCYjDQzig.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PxXVoxodKsqvdYBpkDrnFqxEkjPneeoSg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PxXVoxodKsqvdYBpkDrnFqxEkjPneeoSg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1PyX9b8WmShQjqNgDQsvxqj9UYdmHLr3xg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1PyX9b8WmShQjqNgDQsvxqj9UYdmHLr3xg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Q6SisjDtrHwWBX1sbVHkXqUhqUqzPNufN', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Q6SisjDtrHwWBX1sbVHkXqUhqUqzPNufN.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Q6bYFw1FGLNswZnKVR3EdxqVGoa85DThe', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Q6bYFw1FGLNswZnKVR3EdxqVGoa85DThe.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Q72pNNiCnBamwttWvXGE9N2yuz6c7guSD', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Q72pNNiCnBamwttWvXGE9N2yuz6c7guSD.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Q8pyEMUKdvpMgA1v5e7kkLUtkctpT1hoK', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Q8pyEMUKdvpMgA1v5e7kkLUtkctpT1hoK.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QBY6Dt5Tq7WGc8eAyXwoU47M3ABHua2B5', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QBY6Dt5Tq7WGc8eAyXwoU47M3ABHua2B5.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QMq5TV9jZsxZk9c2kSwsMriaTijSQG73X', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QMq5TV9jZsxZk9c2kSwsMriaTijSQG73X.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QWEQwbnzSmHfG9NCDbD3ko5jD5av8WMYx', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QWEQwbnzSmHfG9NCDbD3ko5jD5av8WMYx.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QZt5o2eU2ymEpUW8EFhybaLpd9cWfqfUL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QZt5o2eU2ymEpUW8EFhybaLpd9cWfqfUL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QaP4JG3RixSgAPwGsc3AuZhzUZsbc3KjC', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QaP4JG3RixSgAPwGsc3AuZhzUZsbc3KjC.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QbprEHD71e6YzWZXNcmxV87gCV5etmpsd', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QbprEHD71e6YzWZXNcmxV87gCV5etmpsd.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QeoDkCAdJSSnm7zWz6Nv7W82qaoUz7kC9', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QeoDkCAdJSSnm7zWz6Nv7W82qaoUz7kC9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1QjUt6TyeV4EMYdouBMyvUiK4JjQJyY1EK', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1QjUt6TyeV4EMYdouBMyvUiK4JjQJyY1EK.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1R3uoZ6W1ZxEwzqtv75Ro7DhVY6UAcxuK2', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1R3uoZ6W1ZxEwzqtv75Ro7DhVY6UAcxuK2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RA48D7YPmS1bcpfhZKsN6DpZbC4oAxpVW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RA48D7YPmS1bcpfhZKsN6DpZbC4oAxpVW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1REEb5VxWRjcHm5GzDMwErMmNFftsE5Gpf', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1REEb5VxWRjcHm5GzDMwErMmNFftsE5Gpf.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1REHQ183LzfoVoqiDR87mCrt7CLUH1MbcV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1REHQ183LzfoVoqiDR87mCrt7CLUH1MbcV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1REpgCXL6WRSoGsajB2X7bxGrE3YE8VsbQ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1REpgCXL6WRSoGsajB2X7bxGrE3YE8VsbQ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RGF1N2Xj372Tan5YPP1yPBkZbaHhmTSGC', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RGF1N2Xj372Tan5YPP1yPBkZbaHhmTSGC.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RH6G5gy39M2Eha1832ETJCWG1ESUMHDLn', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RH6G5gy39M2Eha1832ETJCWG1ESUMHDLn.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RHkGHmMTvi4wimZYEbuV1gfY9MGm8meWg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RHkGHmMTvi4wimZYEbuV1gfY9MGm8meWg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RKXjb3Wcy3aofZFU77dhN6JSjj5gWRgdc', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RKXjb3Wcy3aofZFU77dhN6JSjj5gWRgdc.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RPW5kTX6WFxg8JK34rGEU24gqEEudyfvz', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RPW5kTX6WFxg8JK34rGEU24gqEEudyfvz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RVnHTPeg61SXGRLPmYRWSRNeVgTU3JT3e', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RVnHTPeg61SXGRLPmYRWSRNeVgTU3JT3e.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Rc4bAFRiDQ1oDMhGTj1yhndZWo89D9GVq', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Rc4bAFRiDQ1oDMhGTj1yhndZWo89D9GVq.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RiQSqBHgGcPX1oSMkdLRdXmPFWUyvfSSp', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RiQSqBHgGcPX1oSMkdLRdXmPFWUyvfSSp.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Rk6XUVAT89wAQnsE15TZq4ezp4sVmSXAa', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Rk6XUVAT89wAQnsE15TZq4ezp4sVmSXAa.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RmJpw5aT3WJxhDMt3hxMEtjNaD4XqfZW8', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RmJpw5aT3WJxhDMt3hxMEtjNaD4XqfZW8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RrM7fzUXzx14Gcryromgiz7GfmmACmgAv', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RrM7fzUXzx14Gcryromgiz7GfmmACmgAv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1RtQd7aSQPJhMMdkcFdSbXF8ZcZa9z2dM4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1RtQd7aSQPJhMMdkcFdSbXF8ZcZa9z2dM4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Rx8CLp7phE6WT4pgyXDZfqgHHeJqAHy1M', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Rx8CLp7phE6WT4pgyXDZfqgHHeJqAHy1M.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1S4ne7rmQArkAeEzXqRYVhGU1dtDwPW1zY', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1S4ne7rmQArkAeEzXqRYVhGU1dtDwPW1zY.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1S5LfR9YKEXERGyS6oq7UvAg7444Q6qAPo', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1S5LfR9YKEXERGyS6oq7UvAg7444Q6qAPo.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1S6t5PrHXnozytDU3vYdajmsenoBNYY8WJ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1S6t5PrHXnozytDU3vYdajmsenoBNYY8WJ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1S7FxsB6WAnUXANq3F2hu2rKXqy6tcJVR4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1S7FxsB6WAnUXANq3F2hu2rKXqy6tcJVR4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SAaFjYUD5KFYidYxPzpnf6HgFs4oAJuTz', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SAaFjYUD5KFYidYxPzpnf6HgFs4oAJuTz.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SCjWf4jzwFiX62c6YjQqTZ4wm6C8mQH5y', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SCjWf4jzwFiX62c6YjQqTZ4wm6C8mQH5y.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SDosU2BYNH4YGF9tZ3ovBZ5FnACBKCU5h', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SDosU2BYNH4YGF9tZ3ovBZ5FnACBKCU5h.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SHfrdJjpZNexZdrM8YvDjaVas2i8p8xbZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SHfrdJjpZNexZdrM8YvDjaVas2i8p8xbZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SRTR7jVzrBgwzbvbhp7SQ2WFomfqgsYAP', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SRTR7jVzrBgwzbvbhp7SQ2WFomfqgsYAP.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SYGqkj4kETJmkybyi3XBo4yMg4XysmeVb', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SYGqkj4kETJmkybyi3XBo4yMg4XysmeVb.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Sc2dHqCRLQV9gWLmUndQyowVrv12AtcJi', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Sc2dHqCRLQV9gWLmUndQyowVrv12AtcJi.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SjyYENv3zUemTXSszg244UTnycMwuFYTW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SjyYENv3zUemTXSszg244UTnycMwuFYTW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SnstePbB9qpEpUMScrxFvvdWD6KEoFqAN', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SnstePbB9qpEpUMScrxFvvdWD6KEoFqAN.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SorR4UFBkUJeYVbtXZBNivUV1cQM6AqRR', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SorR4UFBkUJeYVbtXZBNivUV1cQM6AqRR.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SovvF5KRQjBiVX8cHFmEoMc7H54ehxstV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SovvF5KRQjBiVX8cHFmEoMc7H54ehxstV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SsLyfk8F3mRFZGJ8mSHw2fpd5gREm1XfN', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SsLyfk8F3mRFZGJ8mSHw2fpd5gREm1XfN.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1SxZ5k4tdtX7rNxNN2Vg5uueVy8Gu3JUUM', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1SxZ5k4tdtX7rNxNN2Vg5uueVy8Gu3JUUM.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1T1YzfVkbpeTtmY4vQJMgMs8QMHG8vjtQ4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1T1YzfVkbpeTtmY4vQJMgMs8QMHG8vjtQ4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1T1tZRqU7DuLf6qsMFxBFFXqLsAG3qhXxY', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1T1tZRqU7DuLf6qsMFxBFFXqLsAG3qhXxY.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1T3MZ2R44tUNKq1pNe516hfWJVEhngKwMP', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1T3MZ2R44tUNKq1pNe516hfWJVEhngKwMP.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TBaU9Jz3DnZAifL9pcW4g3EvH9WpNFyo1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TBaU9Jz3DnZAifL9pcW4g3EvH9WpNFyo1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TKR7s5yAuxkdz9Coq4VgV93TNob2Lwm4o', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TKR7s5yAuxkdz9Coq4VgV93TNob2Lwm4o.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TUx83WuwtA2Ku1pi6A9AZqov7CZfYtLUS', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TUx83WuwtA2Ku1pi6A9AZqov7CZfYtLUS.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TX6DL4LJ5boKSMvuFxApjzu5MKPRkPBT1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TX6DL4LJ5boKSMvuFxApjzu5MKPRkPBT1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TYsdDZBq92cxENbke1YxXEBWdQ6i6WJua', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TYsdDZBq92cxENbke1YxXEBWdQ6i6WJua.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TgnUythoUoLKxCCEdR1VkjiiY5TmE7M7r', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TgnUythoUoLKxCCEdR1VkjiiY5TmE7M7r.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Tj6xknbwjyK5gyfESNR6WESBcP3yX1mmj', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Tj6xknbwjyK5gyfESNR6WESBcP3yX1mmj.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TkDSeHCDsoszfXiU5KqJsQZPs9GB1GeMx', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TkDSeHCDsoszfXiU5KqJsQZPs9GB1GeMx.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TnJ9n6d3Qd7SNzBfjtVUAdYiKr1yAvzjZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TnJ9n6d3Qd7SNzBfjtVUAdYiKr1yAvzjZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TnwBxgK4ayHuxrti6KKkJpWBHXBYRCX6H', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TnwBxgK4ayHuxrti6KKkJpWBHXBYRCX6H.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TpBh6jsrn7EW9wdxHRM38Bzet51hikcZf', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TpBh6jsrn7EW9wdxHRM38Bzet51hikcZf.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TsnYuiJmbuCK9x7Gvj812UaNyKKh8YCTN', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TsnYuiJmbuCK9x7Gvj812UaNyKKh8YCTN.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Tuvu3JCXJWgoL8NLYfy38dMK7jxcdrFkZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Tuvu3JCXJWgoL8NLYfy38dMK7jxcdrFkZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Tw9RoAGvYC1mjaPNpqbiwpvPm9aHm83J2', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Tw9RoAGvYC1mjaPNpqbiwpvPm9aHm83J2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TxmCex2sYHokDuyPDzTd5EicimiY2xBjD', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TxmCex2sYHokDuyPDzTd5EicimiY2xBjD.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TybhR7XraG75JFYKSrh7KnxukMBT5dor6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TybhR7XraG75JFYKSrh7KnxukMBT5dor6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1TzZ4CwKiukbASXEe7RiCFonpud47ek5rX', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1TzZ4CwKiukbASXEe7RiCFonpud47ek5rX.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1U1JZaXoG4u1EPnhHL4R4otzkWc1L34q3c', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1U1JZaXoG4u1EPnhHL4R4otzkWc1L34q3c.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UDc2ZUoAAvv8amw2DqVuQK1fKjb1HjxR4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UDc2ZUoAAvv8amw2DqVuQK1fKjb1HjxR4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UHX2yb1D64iEXMwfh1oMwjyDQoeAKPson', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UHX2yb1D64iEXMwfh1oMwjyDQoeAKPson.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UMJwse4X8pXjSX1THZCNTDYCCiVLi5Gdv', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UMJwse4X8pXjSX1THZCNTDYCCiVLi5Gdv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1USPBofbzfXpuCDBsy39H3YjTB5H7dwGL2', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1USPBofbzfXpuCDBsy39H3YjTB5H7dwGL2.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UVV6uMNDJDDRLW2qZBKt8mz16Aj9Btdx6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UVV6uMNDJDDRLW2qZBKt8mz16Aj9Btdx6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UbqtneoB9H2xPrjrJg7SJTKJ57S2cQTYi', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UbqtneoB9H2xPrjrJg7SJTKJ57S2cQTYi.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UcPh3S1K1GAFqUt212H9qjFVdxEnca1qk', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UcPh3S1K1GAFqUt212H9qjFVdxEnca1qk.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UiXotpviCQZgVpJEXmokfDTvp8irFprXj', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UiXotpviCQZgVpJEXmokfDTvp8irFprXj.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UmxSSUQ5716tRa2RLNSAkiSG6TWbzZ7GL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UmxSSUQ5716tRa2RLNSAkiSG6TWbzZ7GL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1V2i7thU8h4ZFKA5FDux49ak2eiuY6AYBb', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1V2i7thU8h4ZFKA5FDux49ak2eiuY6AYBb.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1V4Vp7zhynCNuaBjWMpNU535Xm2sgqkz6M', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1V4Vp7zhynCNuaBjWMpNU535Xm2sgqkz6M.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1V6cNW5jTUxEwmMhxvNHkMF3Bkm5a9Cfrt', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1V6cNW5jTUxEwmMhxvNHkMF3Bkm5a9Cfrt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1V7VoyjbvqSmnRtv9pHkRuBCPT7UubCrCX', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1V7VoyjbvqSmnRtv9pHkRuBCPT7UubCrCX.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1V9hXfa36F4Zy75CRJH4BNSa3r1L59Lj7J', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1V9hXfa36F4Zy75CRJH4BNSa3r1L59Lj7J.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VG2WtYdSWz5E7chTeAdDPZNy2MpP8pTfL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VG2WtYdSWz5E7chTeAdDPZNy2MpP8pTfL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VHKhuupGGEMukAVpJoHgpqCWBmAoag6oE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VHKhuupGGEMukAVpJoHgpqCWBmAoag6oE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VJsKdNFYueffX6xcfe6Gg9eJA6RUnFpYr', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VJsKdNFYueffX6xcfe6Gg9eJA6RUnFpYr.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VUDwGZH3y4iEtBB2ykYSh2KAAa8r5xD6w', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VUDwGZH3y4iEtBB2ykYSh2KAAa8r5xD6w.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VUxrH7x6QpkVieeTH3J7UeNNMc18ZpwJ6', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VUxrH7x6QpkVieeTH3J7UeNNMc18ZpwJ6.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VYsVfmobT7rsMVivvZ4J8i3bPiqz12NaH', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VYsVfmobT7rsMVivvZ4J8i3bPiqz12NaH.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VZLvWDdnnj8QsnZxKviYjkTs7eXvUprpL', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VZLvWDdnnj8QsnZxKviYjkTs7eXvUprpL.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VcAVYGg2gvZ1NxnZwAzUaPf7i8iGjoAtv', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VcAVYGg2gvZ1NxnZwAzUaPf7i8iGjoAtv.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VgXHLXRgh6J5iGw4zkk7vUfjLoPhRnt9L', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VgXHLXRgh6J5iGw4zkk7vUfjLoPhRnt9L.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Vj3PaHEDx6TPJUc2we1TvJfbTqvJ1FehW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Vj3PaHEDx6TPJUc2we1TvJfbTqvJ1FehW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VjEfVuwA4A1cuHvugTwrtZSvhPv9JwwbR', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VjEfVuwA4A1cuHvugTwrtZSvhPv9JwwbR.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VmnipmAyttWiaAGJGB9ovGZyzGVp5rYFE', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VmnipmAyttWiaAGJGB9ovGZyzGVp5rYFE.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VqeMD1xwDKonTLsUicK93zjeZZFaRqwrq', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VqeMD1xwDKonTLsUicK93zjeZZFaRqwrq.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1VuzbGgSgi2xCpmj1BwoZbJ11iUiQe9CFj', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1VuzbGgSgi2xCpmj1BwoZbJ11iUiQe9CFj.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1W3TjLKJtNoCwPrep3GNcVoAxutgspppFW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1W3TjLKJtNoCwPrep3GNcVoAxutgspppFW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1W4wh1qDc2g22DToaTfnCtALLJ7jHn38Xc', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1W4wh1qDc2g22DToaTfnCtALLJ7jHn38Xc.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1W5WVJk4yAm6cEzpLkCf5o1PQqi7hpJ5Nh', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1W5WVJk4yAm6cEzpLkCf5o1PQqi7hpJ5Nh.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WA4uumJ9vqr9TLeQtHoZvoKXi6NdA37CD', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WA4uumJ9vqr9TLeQtHoZvoKXi6NdA37CD.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WAyyujWwSmzeZkTruYRkeVfrH2cYhuKsK', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WAyyujWwSmzeZkTruYRkeVfrH2cYhuKsK.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WHvksWqb9FarLL3mMW6ieN1qspqj9Tj7H', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WHvksWqb9FarLL3mMW6ieN1qspqj9Tj7H.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WJ1tKARGLmEhrUyLyTUjXdBfaEQQjyvkZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WJ1tKARGLmEhrUyLyTUjXdBfaEQQjyvkZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WLSwEjwQdBPJjYpTuyLmwduTZ6b4i8f8s', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WLSwEjwQdBPJjYpTuyLmwduTZ6b4i8f8s.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WQ6TM26S4hA3VAwvpj1b7D4Pd3jstYsxV', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WQ6TM26S4hA3VAwvpj1b7D4Pd3jstYsxV.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WRUe3csC1jiThN9KUtaji2bd412upfn1E', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WRUe3csC1jiThN9KUtaji2bd412upfn1E.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WZcuUfyVEVDZ44ya3DagEbHvvSvXDffPU', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WZcuUfyVEVDZ44ya3DagEbHvvSvXDffPU.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WeKKoDwdUqh9EXNxDVYJoJPP4wgQxrNYZ', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WeKKoDwdUqh9EXNxDVYJoJPP4wgQxrNYZ.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WhdeSqfRPyPPuaWWGA5JSykStj5D3W9ux', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WhdeSqfRPyPPuaWWGA5JSykStj5D3W9ux.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Wk94dW9z6Do1fw8AqJVjAx5Z5NYKwygTT', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Wk94dW9z6Do1fw8AqJVjAx5Z5NYKwygTT.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WsEyYJEGqDHoJrgvRtXCGSCTfCjPNFSpk', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WsEyYJEGqDHoJrgvRtXCGSCTfCjPNFSpk.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1WuSvfqsNczyQxJS8K8XPdpJsxqYCiEdke', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1WuSvfqsNczyQxJS8K8XPdpJsxqYCiEdke.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Wx3oHR3TQYbwHeS1HPw34tMeKSKRUeZu8', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Wx3oHR3TQYbwHeS1HPw34tMeKSKRUeZu8.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Wzfxze6PbchFxfw2Pxt8MY9My1hG8YRen', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Wzfxze6PbchFxfw2Pxt8MY9My1hG8YRen.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1X5Xk2kJHEfcRMRSpXVUzGgGcgKYGue5LT', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1X5Xk2kJHEfcRMRSpXVUzGgGcgKYGue5LT.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1X8CrV4ptJWFtKfKXZecxtbB7zuoydVqwt', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1X8CrV4ptJWFtKfKXZecxtbB7zuoydVqwt.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1X8DhjBCqCPQ8Ssht8awJQxcmseC2W3bYW', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1X8DhjBCqCPQ8Ssht8awJQxcmseC2W3bYW.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XCK3y54in6964u9UvhxxZgJ3Rhi8Jvmpm', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XCK3y54in6964u9UvhxxZgJ3Rhi8Jvmpm.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XCkUbuKiRdwqcdpTh4N6UbQaN6sLmsH2Q', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XCkUbuKiRdwqcdpTh4N6UbQaN6sLmsH2Q.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XFVGGeW6uj2x81GhpV74isogCyU8tYz3M', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XFVGGeW6uj2x81GhpV74isogCyU8tYz3M.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XGta53KtvSGfnDNaTcNxXygSgxUL9yNN1', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XGta53KtvSGfnDNaTcNxXygSgxUL9yNN1.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XHXXEdfxn2N67R3k52zS7HMfxJ2LjMTy4', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XHXXEdfxn2N67R3k52zS7HMfxJ2LjMTy4.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XRJqs3xVNk3pE7FmB8JP8d4HWwwXpwFhK', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XRJqs3xVNk3pE7FmB8JP8d4HWwwXpwFhK.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XTXBsEauzcv3uPvVXW92mVqrx99UGsb9T', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XTXBsEauzcv3uPvVXW92mVqrx99UGsb9T.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XVTxHSPejPUDF5PeX9PodYbsSSDr4juH7', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XVTxHSPejPUDF5PeX9PodYbsSSDr4juH7.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XYHyoewY5CMDdcYB5BjN7dQbWreV5cWgH', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XYHyoewY5CMDdcYB5BjN7dQbWreV5cWgH.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XcAi9gaL5MUkFWoQjD7zZAohRSRukra9D', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XcAi9gaL5MUkFWoQjD7zZAohRSRukra9D.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XcR6yEbjjPKbJpo4PoM8bEnpuPTXNKK3N', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XcR6yEbjjPKbJpo4PoM8bEnpuPTXNKK3N.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1Xf2Cwwwh67Ycu7E9yd3UhsABQC4YZPkab', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1Xf2Cwwwh67Ycu7E9yd3UhsABQC4YZPkab.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XfJfpgk6SPBfZhfYbeszwPnnepBTjfRNF', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XfJfpgk6SPBfZhfYbeszwPnnepBTjfRNF.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
  it('KT1XuvENs4d8EonCGf1PkKEi8p18r558Ntet', async () => {
    const stat = compile("../mainnet/mainnet_contracts_2021-04-01/tz/KT1XuvENs4d8EonCGf1PkKEi8p18r558Ntet.tz")
    assert(stat.status == 0, "Invalid status code, actual: " + stat.status + ", expected: 0")
  })
})
  