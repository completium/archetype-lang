import { Chest, Chest_key, Nat, Rational, Tez } from "@completium/archetype-ts-types";
import { configure_experiment, expect_to_fail, get_account, set_mockup, set_mockup_now, set_quiet } from "@completium/experiment-ts";
import assert from 'assert'

import { raffle } from '../../bindings/contracts/raffle/raffle'

const NOW = new Date();
const ONE_SECOND = 1000;
const ONE_MINUTE = 60 * ONE_SECOND;
const ONE_HOURS = 60 * ONE_MINUTE;


/* Contract parameters ----------------------------------------------------- */

const JACKPOT: Tez = new Tez(50)
const TICKET_PRICE: Tez = new Tez(5)

/* Initialise arguments ---------------------------------------------------- */
const OPEN_BUY = new Date(NOW.getTime() + 10 * ONE_MINUTE)
const CLOSE_BUY = new Date(NOW.getTime() + 10 * ONE_HOURS)
const CHEST_TIME = new Nat(10000000) // 20 seconds on standard computer
const REVEAL_FEE = new Rational(20, 100)

/* Partial raffle key 1 123456 --------------------------------------------- */
/**
 * $ octez-client hash data 123456 of type nat
 * ...
 * Raw packed data: 0x050080890f
 * ...
 *
 * $ timelock-utils --lock --data 050080890f --time 10000000
 * {
 *   "chest": "fbd3aaa288feb385b4e5e880b49291d6de99f3de9497db9db180f4d7c58d96a5eccfc0b889ffabe8bbbceaa6f1ef96e1e18f82f194b4a9cb91dffbdd86a4e683bd8adf81b280efe9d388c2d4f7c4b6a7de87dbd78be5aff2cba3ffabc7fbf6ddd0f9bcfd8d99bab5c882ef9bb79cf3fae5abf9f1ccc1a287918490a9cbddd591bdcae0d6c18bd5c39f9ffbc5f0a1edfb8ef5d5e4a4b2b186d49e9f93effddaffaa89a090dda4e0a6aca094fefd8599a7a6c2c6aee0b5e3f5a3c9b994d8af9d8df9b3f3a5abaaaaf2accdb3efcf92d49aa0f2dfe2fbb0a0e3d58adcabd0d7fa89cca8b9c2d2ad95ccc2f3d094f38983dfa385f8c4d0e0ada5d6c2bde1a1c6afc89a9c9cf4b3e2e2f88ab1cfce8ccc8cacd9e5cd8dc48ae79aa4bcc2c8e2dce79df3eb9c840545f99742620f19df2990e22bff98ea02856b7e9a3eb0c9a700000015e0ac8f0a64d17ec13a58c10f11086a0e8b8914b362",
 *   "key": "bef9acb2fec2eafe8fbdacc1cf94addaeea4afc193888ab286eae4a6c18bf1e2f0bb84c4d0e9f187babbbbe88afa9f86fdfec4faf0f5b1dfd98cfdff8dbcf3d4ea9189bddbe3f3b9f0b4ff958ba7ec81a8a893f983c8b5a5b3988486f8c3ac99eda6add8e6afd5fd8cfbbce3faeda1dff3dca7a9e3f5f79888bde3f4c0e682f4cfe9c8a9dcd989e2af9de1f284949a97c2f5b795eb9296e0b1f596a3f7fda69db2faa8f6fe829eb9b1a8e683a9c283eefe98c3f8e49789b4bab6bba988df9af79cd1bcf9efe1d1eaa3f6e5dda9f5afc7d6cfe0949880d1b3fca7fdc6ea9096fce2dcc6da87b888f69395cbb3aaa6caade9adb1bedf82e69cecda959ff8c9cb8bb1b3bc8ce6acc6c89ce7d69bb499b2d58eff84c8ccefc8dcfda1f7d6918bffb48bcba5a90ba488feae859887bbfbdfe083c3b2e48ca2e0bc8c8cbcdbe3fec9a8d7d4c6f8ddf2bcdada95edf1e489aed5e7bd8ba9f2dbf9a2de92a9eafc92db8184c8b1ede699cc92c0bf8fadb8f1c0a1c3a6d1ad8481a6a3c98fcdf9c0f0b1a2fd9985e8f1ce8a87a7b0dbe6faadbcb5b3c19097b5f198a795eff3b78ae3bba394d3f1c6b4d8e3d6b7cdb7ffdcbe94d0bfb485dcfeca9bb3d7d6d0a7f0aeacced6848f86efede9f0a6f99fb4abd7d285dbd6a0b5fb928ce29fc5fad397b6da8d94f187a78785edf088d3cde8cb8187cf99f4eaab90b5f8debcca83828cfccf80afa6d981c2f4d1eacdc6e8dbb2cecba6eaf5968cb69be4ccc78b9ac8809a83a39988bd85ba9dacabe5dfd383bcee8efbd8a5a78d84e3d3d2b4849ddbe4bceb8db9b687a5d5b0a2b0ba0586bf99a2b4a8eca1eae0fc82db87b9f58ca3c1c6d4f0b5b2f684c6aec2e5d2b1bbb297b2ceeefef8cb9581f9bcdfb8969aeda6f7b2d292f09dd893efaf81a7e4cbacd29ed6cee8fbfdd1f0a1bab18adaa1a5d5d9defbcdcfbc9bf184e7c9e4d8efbae2fade9a88f1e7a9ccd8e4e3b5bfd48ea6dcfbabbbfbb0c7c1e0a880f6cbacb9eab5a897e595e09ffcd19ce0f684add4f2e69b9ff892aefeaefe8cadd8ea80c2e18992b193a29dd0ceccc0e3dcb2f7a4d4d38083d9f4ea8b83edd7dbf78ecf9684efb2c2eb96a6b8bac7ac84c0b096e0a0b2f0bae5859fa1ec91d394e8a695a7f1b4f0f9bef3fee7eac1a0a7becdf7b1faec8082a7e7da888dbbf2a1cef2f4b3d1e0fdd8f99993a7c9e6d384ea94c19cddccebf4f288b5e0e3c489aaafd8a0ecc2cf028fa0ff8a92fa8584caaaf9a0e0b8f286d7ad01"
 * }
 *
 * alternative commands:
 *
 * $ octez-client timelock create for 10000000 with 0x050080890f in data/chest_alice
 * Timelock chest and chest_key computed.
 *
 * $ xxd -p data/chest_alice/time_chest_190195d94a3629e6ce3422bca11c281d943c5682034b05ccc40c6d73609ab7bd | tr -d '\n'; echo ""
 * a6e2f8acf3b3c088bcaf9fdbb7dcaf8fe1a39ddd9fd0fbf3ecbba9fdb19f96fcafe9d0d49df8b0bed3b7afdbb1b2dbcb9ca0c1a5e49ddd8be9d7e8cec5eac7f8f9bb9da1b9eaf3d1eef193eef3f891b1b0cdb5bccf95c3f18dc5f3f5b6838ba2fac9e09ae7cdf696df8d9eb8b8f0d6fb94a4dad0838ed0e8cffecd92f1d7f7efe2fce08986c6e788bceac5c4b1f8b48581aaf9b4c2f3e681fad392cdf1f6f1eaa684ccb5b2a5faaf98bb80d0d4ba9fc0b087c7cd828582c499e6a4e4f799dab6baf2bbd7add7b1aa8be7e49b87f1f6d2bc89a2fcb887e7df938ea9f4c2a488c4a4fef4869c8bd78aeddea0e3acc1a0c6918bf5b8e7da9ea6d0e2acd3efcfc29bd0abd19b98a1d292c9d3f9f3f2a4c9f7abc5b2b982c8cbbbe4b292bca7d2a4aaff88a4f8044a4e75bc6fa0e81e707239f2bd801698304851827819aa690000001caed068c4037b2eca5ba0d745d39d2a67e95764c6b2f1370365b06a25
 *
 * $ xxd -p data/chest_alice/time_key_create_190195d94a3629e6ce3422bca11c281d943c5682034b05ccc40c6d73609ab7bd | tr -d '\n'; echo ""
 * 8cbac2f2db93acd68dda82ae83c3badbe784a19ea58cfb8ba9a5daa0faf4cbaabfdcdbbedea9abccf38f83f682e3ecbfdb91aa93dcacb8b19ebcdd9bb8a3d3f5f7f0a888ccd4bcaa80e98494ffa1a2a7f8bead8990c681b5fbefeca1fa9fb3edc885d9bd92a485d4ae9e8ca5acc8b8f9b8d0d5918bcbc0d1c7a599ddef88acb9bcc39d96baf483a9b4b2f5b4a3afb4b1f6d0cabca7c4a4bbd6c0a6d985dfdffea293878de2e1fbe9ecd5c6b4cbbadd95d89cf3a7f0ac91d9d4b19ca3aad0be958a86eb97c78d92d4b8f7d8998697c0d4b3ea918096d7bba783fdcacdecfa8b8dec95eff184cfc0c8fab6cac5f1cdecf1d2a788bc92a5b987e9c0b8db8cc3d2db9696ad93b3e5bedfbeced6eaed8ec6c49ad0b5c9a5afc8fc9a81c8a0f6c4bf9fd5c9b2d304cf8fbd939d82aca2f6b1e5f2dbf7dec0cbd8b69acbb493d7bff99cf8918fad84a4ae93e6bdbcba9185ddd8bad7c5b1a9ebf093b2dea1e682eff2f8c2f5cfb3d5ada5e6aaa39fc6a48dbd97d9aae6a5e4beb89bf69be493f5bbc7b2a7affcc6c6f4cc9cbcae8abba8a4b494fedda7bad489e2c9a8d080e0f384cef1df8094de93c39a9aa487bd90d4eed19599a1a0e380a0f6aee786ddc0acfcd9ec93e6f3f1aed6b6da88c9e7dfed9ea5cdd9fcbef3f6b799f7aee7aca6a8ceade0ba82a5c6daf0c7a6eac7d7d5b2cc909eecc9c796a5b8cbd8f1b0beb5e8b6d2a4f98299a2b296b8ab9de9dea2ade8f4fce4b2d392b5d1b0a1cdc4d0a1f5c498d8bcf2aacbdecbacc8f6cbcfdffb8dcef1de88b3ffddf4a684f5e6cbdff3d8ce96e9e0a79cfcdca08eae0b93a0eee8c78fb1f59be9e9d8aaa3bde695c9d199e5c692d594ecadf4c8e597eed4b8b4b5faab99eeb7ff96cabe8aa398be85eae1e9f3a081909aadd6d481e685cf94cec2b2bcfbd9f995ff8fdac2aa8e87c29fecf880ab8ba3f8bebe9e8fd6d194e19ee3f9eea7aecdafab88c4da9ecea1ecffced790bd8cb38dedf6fbfae882d8fad6cefccdd5e0d4ea84feb6e3a2b499ad98a7f5a2ced4f8eabaffd6f5c18ce5b8d2a1b194eed4d39690eeb6d3d7aeb580d3d189a0d2f68abebdc2ef8a929cb6b1e6bbb98ec9ada584c3c3d58783eb8ce9b6ebf8fa9cefa0a9a8828bdae9d5e2faf8ffd8a483ec8d94c5ddc0f4eab6e2b781b6a0cac48bafdee2bfebaa8dc5b08399a783e98cebf1f0dcf68687d3d0c4b6e2e6e0b2cdd3eb84b3f4e28ae4f48cd4e76f868b82e5f286aa908d83c4b7f3c687cb94f701
 */

const LOCKED_RAFFLE_KEY_1 = new Chest("8ea1d7ddb0d8bb9482c8c5e4e9c0ac97c0caecf38bbefdeaa9e8fbbeec9be8888487d8e2dbb6bf8a8bb198d3a5bec3d7af85b7def8f4bad5a78fc1ff89e9bba1dfefa2d2bee18eaca993d3e5ffcea4fc8391b9e0c59cf5bdbed8d3bf859ec7aea1a7cebbc9e2fdf7ebca94ffa5d0bcffd4b9fac39cc8b2a48ae180d491cfc58afbfbf795deabdcce9baef6899c9ec7dc81eb91c0b7ce8dedb5daecdfa1b8cfb9a88ccfb4edc8aab7f8ab80fffba3f889e0efd2a7f8ffb38dfbacaeadb797adef80e9eeffc9a2a4a3f389c497e1c3c1bb978789cef98efe84c9b1ca9f8fb0c597c18eb3c7c4b2a8f6d0a788879089aec2a9bee3c0bfbea6ccf7f6a09e99a3ec92d8eceba1cadd83c6829ad1f3feb19ec18c96b9c48df7dcd3a39badf99eeb92bcd9a691fa0b2a9e2020f044ac99df7bb272e0e0ed3980a23283b3bd4e9e000000150c15e064cdaa17ba1ed5122c7b1718d32a1b978a62")
const CHEST_KEY_1 = new Chest_key("ea9dbfa59fbdb094c7cbdbaceda2c9a6a4e380d3d085bec0e18ad4ecc5da9bf9a3a98df6e48adee0b38be8cadcbaf49fbcefa1f4e893e1a99cb4a5fdb0b7eda5e3e8ccb2c68cc5979fdabbb6f7f58ccfd383b295d4e1bc9cdc8fd4b0e8eeacf4c9b3fe8998dbe6de9ccf83f3bee9a3ddd882a08adecdb4b686a0a3deb19780869ed9abf1b9b3d8b2b694fcf3a5c7bff18f908fd0bd9796f3c1ee99c78fbfa6b8f2d6a3d182dbe0dedbf496a7c5a189afc1e491c7e3dad9faad96e9d5b4f7d6d29c80aae2eef1baefac8487f1fec0c69888fde3bff2b1c594eefae592b0c286a3c792f2fa9ada8eb6e99ad2fcc1b5a5e3a18891989de2e58284e89ad4dbf1f6898baface584a6f6aaf9eab7c0ffd9c7b2f7c2dab9b081c8c0f194a4cea8b2ecc1b0cccfbf02a4ca87b3e1a4a9f5aea8d0e5c1e2afc386fedfb5f6e1c2ac9feceddcc18484c987978df78790efa7dcb9fc9886889285e1a2c0b4febdd2cbc484f287a4cdffb3a79eb8b3e4ccb48382a4a0dcd8b5a694a189dddc8fe1aec6cae2d2efd5d5f282879691c0e6b5f993b199ed919bf886ddb785b980c5b0a48eacc4dfedd4f384bd88eec38af4c6d6d9faf7d8aeaad1ccdff193cfc198fcd8fedfecdef3f7ed8088ccd6aea0dccc84e1aeddd98dfba085ebf79a92e7cec584ad84b480bdc7adc3bdf3c88dbdd19e83d884d6c6bcb884b0ba90d5c79cb6d79ae8c1819edfa8e8ca8af8f6f5dcdbebc5f1a58fc891a0b5c4ead5cfbabdc68df7a990b4e3dbb8d5bbeac0838cc4eeea8ab5b6db84b2e8fa8fcbb593ffd3edb3eae4ada4c284f39fa3b381e6eb24ee99d189fec09ee89089ee989093a38fd8e18aa08da895beabcaf1e3a0fc8bbb9b8eb7c5d3dc8bf7f9d999e2c4d894fda2efaad69c8783bfbbc398ddb6fdbfaf9cafc9ffd6aff5c4cfa4979ebcec8db897c2e38d92c381e39ac8aaef91feb4facdadfcb9e2a2add6e8a09d8a8e8281b3a0d7cefec8e09683dfabd8c5b2919e9ee9cad7baabf0f8b2e392a29dbdaf9caae99eeda9fca4acedb7b3a58585c7a4ebfba1f39d92aab08eb69487e6f7e4cce29ed4d9e9e1a2b9bbf6ddd0f1cd9aa6abab8f82c4eaa2eabee9dbef9492ed8ed4faedaab48997f3bdd8d6ed8dcb9be8b9d6c591dfa6d3a3c9db9b93df89cce8f1e081d1d3b5a28cc0adaaa2fbb7a8d6ddb8e5aed9f7a7dddd95dea7fd92b3eda3baf7d3a980b8c19af1d986f59fd8afffd9eeb7a406cae490c3f4db9c91e1edc2e3c0c9cba5fed002")
const INVALID_CHEST_KEY_1 = new Chest_key("8cf2b7b69bd4e18dc7a39c989ee3e8a4cfb191f485dd95ed8dabbff3fffdbabbceb5bbe09def8cc1a1a0809db6e3c6f6f2eca698d4e9c4cff1f1e3d2ecaaa58aa6d5dccbf1dfaefdbdfdecc4f38bbf97e9b5908daa89aae6ebab9ed9db919c8efba5ffb9bcb0f893dce2e7b390f0cd9c99f3d796d78dbd9cb8eb939385d9dfa0eda3b5d5adc4b58fe78fc891dea0c3c9a9f0d5d3c5feffdfdad78ee5ab91f4958d96f883dc8f8bb4a5d693f9bf8cc4fedbd99dfcf8a6afc8b9d2dbd19ca6edb5a8cd85e2c7cfde8d95fba6dd81a2d8a3e8a9a5939fc6cd9bc6f5f296eafb8dc5c1d2bfdfb8f3a582c0b5f6ceb899d4cbf6b7b48098d8bedff1a1e3c39df5aecad3a48ba5a5a1d9e9a9a7b8c094a4c8a7a3d7f2c7ebc9a7a789a587dd8abf948da4b4efd4028c98ab96ff9e97cca988bddbf4f8deafa798a091eb8d8df49afdb6fee5d486f881e4c5e8eee6bea5c7b783bbd9968080eaa1a8ab96cbb38480b1f786cd928297cc80bea9b992ab83e99c8295c79a8ca4e7dd84d5b1a3c9faf092dfacc9ddf1d9f7b4e1abe59accaab3cae5ecfcc8938e93f78993c4a0eb8af2a6b38ffabebfb0a5c2cee4b38bc8fee0e2d09d83f0b7e2d0c2d892e5e1b59fdbaaa8b6e587b493a6d6eba6aba9fdd3c0b9f1cad9fcd69ebe9ca9cdeed58bcfd8d5cfa4a0e2f9a697d4fee3edace0b4abe8c193ec94978ead8eace2d2b4b1cd84d5fa82c480929ea0b299b19dffb7feabcca8e29dc991dfa6ef8ea1eeb588e4dcbdaede82c0c7ca9583e6839be9d299cfe8cba2cd8ce29cc1dd81c0bac1d2fcabdaade8e8cec7c5d394d4c305a3c9bea9f3caf49495b2bd9fc7b9a0a5dce3d4bac09bbabb9bc3dc81dcba8493ffcfd299b8f5d0febe9af585dca3f6ed8c84b1fdecc3d5dee59faaf58190dbdd8dab8aab84d0cd87c3af9191e9eeb2b0939bbbc2f7dcdaf7aca8e1888ab88bf8a8a9ba9cb1f9db8fa6e3f1d3fed3d899cfa2f39cccf3e6c3a4fcade5fababbc1a0d883c2fad893a6d989fbd7b2d4b0c2f4f682c48989d4b3ad98ceb2b2b287b591efdf84f4c8f1b9a7dda2b1f195b1b8d8ecbef1fdf3c3c989d2f2b4aad8c28afcc581dc8a9e86e49ab2d8d9fad3f5b39db0fd8cf3efddfb94f4eabfd4ed9889dde2b9ceb7cdcbaeebb683d5fee99c8693ab82c59f8396c7eeb98cbedadd84aac7a5abefa8d197f6b0a0aeaf9f8fbcf1e8d5befaf786c69eb0bfed9fe5ff85e5818cdfb208dbf3ccb68cefa0def4f987edf5c5aeb6bc39")

/* Partial raffle key 2 234567 --------------------------------------------- */
/**
 * Raw packed data: 0x050087d11c
 * ...
 */
const LOCKED_RAFFLE_KEY_2 = new Chest("83ea84eeeac0fc9786c0cfd8f49db5f5bd90e398f38ae4899c819cfce08ea2eb96bd98e9a7d6a990eaeda8db8fd681b4f9fba89bf6d7e8dadbe2e7c1ba88fa8794aeb2cfb0aaffac95bbcab08795bec6b88b8bbe899ffda8dbcbd89faa86f4bfaeb195e6a3fb85f4cb9ecea8e091879aa894adee9e9793abeea7c9c48cdf85b581ccb0b6cbdfd081c3a8bbd8d1d5d3e4e3dd9ca7b0fa8df4f59caefdc7fa87e3e6ada8b0b9d9aae5bddc8c96cd80ea9aa6baf1f1f1c1a0dd83bf9fe186afe597ca9ef4b293a0839af6b9f6d4b0b1f2bedfa58db1c0aef5bedc83858ae08d81f1b9c3d5d293a2a08fe08286e9f49499b187ead2f7d4b283ecf5c0f9d3efd8f8c7fc828eca958eca9ff2b1c7b99a9f9897e8ada0b5c1e2968dcee6a9ac9adadaf3dcf088df02f12e79aaab6d33649843a90ef48abd17191d7ab96ff25da200000015cc2a50f7c7293164cc6b4d33d88f727f961d7508a8")
const CHEST_KEY_2 = new Chest_key("94f186e29080dbacc5d29da6b38cbff6a997b8e0ac8cebf095f1a7bbf8e3cd88b6f285e3cfdc8ccba993b2a3eb91a58f8dbcebd889fbf1e9d6b790bcf79ae69c97dce78bfb9289fdf1bdfcebe5b5efe8e4faa8cfd6b3c0aff7cdabb9fdaec2cba4efdaaaab98a7bdadcbb4ecbac6aacac2bf8491dbc2dae1cbafe191f2f6b1e0cfc294aaeaf696afc88c81fa9ab4f2b28eae97bdbbb1c785b8ea8eb1b7edf1ea81f1cacac9cec68fc2c2e2d8c7b5aa8cbfd9b5c0bcd28eababe996af88cc82b9d7f4ffc9e6edabeff29edec2b2cdc79efa9eeafca8f483caacc9e8f2f4b781debcfdafcf80bc96939f84b1e1fe83bff08ecad8ab9cd8aad3f481a0d0e9b7b2c2a392f1c0d1dc9ebdd6f3d5f9c4a0a685ff9bb4aec5ead681ac8387ffa7bdf9e1e386e39c09d0bcf4ded9a9f8daf9eb8fafc685c1fa87aac8c0888589d2fb8cbcf7b9ada597f19f95e6bec6aeaf918592f5df8addc19595b2cffde4d8c0bfeebcdbcabdcfd4ba9ad5d392db8aa2b0a7fda7f9bac9d0c3b49bd48688e88cbdfdf0f5f8fdec91e5d6b2e0a0afbc95cce985b6b8fc87d5ebbdd2b3f791c8cfb3abd285d2f8d7acc0e7ad9099859ec1ffe1e4c9d6acb4f6ebddf287f2ed92c8a0c6f4a7b58bb0e3ebcce892dca6e2dcecb5ad95d099aadd8ec8d0f9f0c3908d86d59bc9d7d1d6a1d3c09ee9b5b3a994d2c0f0fedac4d29bc6ec9394ace6beeecda3b6abd097c7e8e7f98299d191f6d6e3aaceb88e9fd6a8b7a0aaa1bcb789e0e0a1c199d8f8efb6d2a480828c8981f1edf7e79faabb999ea984ea9796a7b69d9c92fb9cf794d2ded996f3f603b7e485c2ca9d98a0cbddd7d2ada2fad890cebfbeaad1cbace0f7b7a49ad1b9f790f89d94efcade95e1cfbfcfcffce3e8e38796da9ce8c4babfe3879ebec9f1d996d4b6e0be8b8d88a2ed88f49ecdb1fb8cb09dd2b5e2fbd7fe93e8a2f29ef8aa92c5d38d95d7cbd787f18db3d5c5a2ab8dddaaf3bfb68ac1d596aab6bcc8e9bcc3eee39fa7e4c1bcc79cd6d4d1abaee7f3e6d09ae9c0d1a9b498d287a5a6c9eb9eacacf3fce5fbf8e2a5e9e9b492c6babae7beceecfaa3c3ffa6b7ffb99cf38eb09af1aaffcdeef8f0e89d819d99bfad83c08f85809fe2d4eea586f7f5d1c8e390fefcd0cee8ffb7d6dadf94e08cceda8dbdb3d086b3abafedca86f4bba7958683edc491dbb2e8b7a480eaada2c9dec3f39dc096af8990bc92e4828eaebccaf6abd7f95cebbb9fc0a7e28cefaadddb85cdf6f78fb3e101")

/**
 * @description Partial raffle key 3 is 345678; This chest has been generated with the wrong time value and the key by external process with right time
 *
 * Raw packed data: 0x05008e992a
 * ...
 * $ CHEST=`timelock-utils --lock --data 05008e992a --time 10000001 | jq -r '.chest'`
 * $ KEY=`timelock-utils --create-chest-key --chest $CHEST --time 10000000`
 *
 */
const LOCKED_RAFFLE_KEY_3 = new Chest("86bfb887dce1bcc899cb8adfbd8b8e9191f7a1c988e0c9cde6898d92b29ef496b68bf6dfc8949cb2e9fca990e7b1eece8c9fedaeddd3d4f3dab999c6faa489e3c1f1e799e7e6f5b982d0fc81d2ed80cca0fffaef949b92c2d9e2b6c9c0d1a999dfd9cfcc99c79bafa1cbeca49fed99eec8f599b2f5fde8c594bcc5d08ccd9182e2a2f5f5a09bc89597eacfdb94ea98a9d0de82bdd9aa9590abccc5f1bae9b8d1fac8f0f0b4f3a999dcb2dcb8c6c79ea0b1fa8bd593fba6b08891abc19bc5fe98d2b7b290e4ad9eadbed2e7aaaab88f9792ad95afdeacb98285c2a38e94fcc7b9b2afcbdb8fdf9ccfcda5c3fe94bef5aca29eeeb49fa1c1dead96fdc4828e9387ec83c8f0e6a7bdbcb2b592ede8c29ff38e80acec91d68ce0bff0e6a68396c6e998bca6fe017e643c770ae025eab1ae2cfc7f8b0c41bf2c5cade252c43000000015262671040a672bd29f65beb9175270eeee49b9478b")
const CHEST_KEY_3 = new Chest_key("86bfb887dce1bcc899cb8adfbd8b8e9191f7a1c988e0c9cde6898d92b29ef496b68bf6dfc8949cb2e9fca990e7b1eece8c9fedaeddd3d4f3dab999c6faa489e3c1f1e799e7e6f5b982d0fc81d2ed80cca0fffaef949b92c2d9e2b6c9c0d1a999dfd9cfcc99c79bafa1cbeca49fed99eec8f599b2f5fde8c594bcc5d08ccd9182e2a2f5f5a09bc89597eacfdb94ea98a9d0de82bdd9aa9590abccc5f1bae9b8d1fac8f0f0b4f3a999dcb2dcb8c6c79ea0b1fa8bd593fba6b08891abc19bc5fe98d2b7b290e4ad9eadbed2e7aaaab88f9792ad95afdeacb98285c2a38e94fcc7b9b2afcbdb8fdf9ccfcda5c3fe94bef5aca29eeeb49fa1c1dead96fdc4828e9387ec83c8f0e6a7bdbcb2b592ede8c29ff38e80acec91d68ce0bff0e6a68396c6e998bca6fe01fcd484c69cf9b1c195d289b298f8fd8cede1ccd3ecfe8bbbe586d2ffecc3bfc0c3b3f88384f2bbc08492f79594e2d980de8ea79fd9c7a1ced29cd8ec9f8ca5aa8ddad4f7b5bdaad8b2d1d78c87d4a689d0a694efac9ebca6ffd8dffa8783a5e284a7c1f3dcdee483cfbafae88fd9a992c6cf9ab69890c1a9fb99ddf8cac789f6e1ebdad495e38abff690c692b9feadf8aeb191a1adafbeacdafa9d899986b3bffeced8eef9bfa4a8c1eae7c9c4c592e7f5a0af879091f7f8bce08ffcd3baa9ecc2b6c486f0c6f08ad8ca93f59785e48dbc92f6d3a290cdcf8cafe9f495d080cdbac6faee9b93e580c0b3eefebaf3c0fc888b8c8ad5f4fcbba8e2a0f083fce1b6e2baccccd9b4c7fbe09ba1baa49bbbe4f1ccf3bfc9a6e4e7a0ed97a3a3d6efb0ddaa85a606a19cedacd4fe9ce9cbefe68499e1dffbe5e9a78fbf82d5b7d389ccf9fbab93ddd2e1d2f9958adfa49cabbdafcfccdd92b1e98480c883f28fc0a3cda6f5839ad2b580d9aaa1e9e1f8a7cfba80a48381f4d2c9a5fca59c81c3c5e9f68be3a7b794a1aecf958e949cfb9daccaa6d1df80d2a7c1eaf098ee87a7e4ab8199d4f79782aedc87e6bcfddbd8fdeda8b7e2d2dbe389b5a099d596b59dfbc09680d99295bfb7dad18f81bad0d9cbe2d1dcf6bdc1c3a4fb92b7a887dbcef297ffe8d9dcd2b5fc8efce18b9eb6affb929decb48bf3a0fcb6bfa5e3eee7cfc8e896fa8dddaad38aadc9c0de90e0d8dfd5d9eaacdfe2f1c4b38edceadb8b87eee79bb4c8f8a6dddeb281bddeb2c0f7b48e88e597fbc9dac99ae88bcb948e96c685adf6f093d7a283c0ccc80301")



/* Accounts ---------------------------------------------------------------- */

const owner = get_account('bootstrap1');
const alice = get_account('alice');
const jack = get_account('bootstrap2');
const bob = get_account('bootstrap3');


/* Initialisation ---------------------------------------------------------- */

describe('Initialisation', () => {
  it('Configure experiment', async () => {
    await configure_experiment({
      account: 'alice',
      endpoint: 'mockup',
      quiet: true,
    });
  });
  it('set_mockup', () => {
    set_mockup()
    // await mockup_init()
  });
  it('set_mockup_now', () => {
    set_mockup_now(new Date(Date.now()))
  });
})

/* Scenario ---------------------------------------------------------------- */

describe('[RAFFLE] Contract deployment', () => {
  it('Deploy raffle', async () => {
    await raffle.deploy(owner.get_address(), JACKPOT, TICKET_PRICE, { as: owner })
  });
})

describe("[RAFFLE] Open Raffle", () => {
  it("The unauthorized user Alice unsuccessfully calls 'initialise' entrypoint.", async () => {
    await expect_to_fail(async () => {
      await raffle.initialise(OPEN_BUY, CLOSE_BUY, CHEST_TIME, REVEAL_FEE, {as : alice})
    }, raffle.errors.INVALID_CALLER)
  });
  it("Owner unsuccessfully calls 'initialise' entrypoint with wrong 'close_buy'.", async () => {
    await expect_to_fail(async () => {
      await raffle.initialise(OPEN_BUY, OPEN_BUY, CHEST_TIME, REVEAL_FEE, {as : owner})
    }, raffle.errors.r0)
  });
  it("Owner unsuccessfully calls 'initialise' entrypoint with wrong 'reveal_fee'.", async () => {
    await expect_to_fail(async () => {
      await raffle.initialise(OPEN_BUY, CLOSE_BUY, CHEST_TIME, new Rational(20, 10), {as : owner})
    }, raffle.errors.r1)
  });
  it("Owner unsuccessfully calls 'initialise' entrypoint by sending not enough tez to the contract.", async () => {
    await expect_to_fail(async () => {
      await raffle.initialise(OPEN_BUY, CLOSE_BUY, CHEST_TIME, REVEAL_FEE, {as : owner})
    }, raffle.errors.r2)
  });
  it("Owner successfully calls 'initialise' entrypoint.", async () => {
    await raffle.initialise(OPEN_BUY, CLOSE_BUY, CHEST_TIME, REVEAL_FEE, { amount: JACKPOT, as: owner })
  });
  it("Owner unsuccessfully calls 'initialise' entrypoint because a raffle is already initialised.", async () => {
    await expect_to_fail(async () => {
      await raffle.initialise(OPEN_BUY, CLOSE_BUY, CHEST_TIME, REVEAL_FEE, {amount : JACKPOT, as : owner})
    }, raffle.errors.INVALID_STATE)
  });
})

describe("[RAFFLE] Test 'buy' entrypoint (at this point a raffle is open)", () => {
  it("Alice unsuccessfully calls 'buy' by sending a wrong amount of tez.", async () => {
    set_mockup_now(new Date(OPEN_BUY.getTime() + 10 * ONE_SECOND))
    await expect_to_fail(async () => {
      await raffle.buy(LOCKED_RAFFLE_KEY_1, {as : alice})
    }, raffle.errors.r3)
  });
  it("Alice unsuccessfully calls 'buy' entrypoint because raffle is closed.", async () => {
    set_mockup_now(new Date(CLOSE_BUY.getTime() + 10 * ONE_SECOND))
    await expect_to_fail(async () => {
      await raffle.buy(LOCKED_RAFFLE_KEY_1, {amount : TICKET_PRICE, as : alice})
    }, raffle.errors.r4)
    set_mockup_now(new Date(OPEN_BUY.getTime() + 10 * ONE_SECOND))
  });
  it("Alice successfully calls 'buy' entrypoint.", async () => {
    await raffle.buy(LOCKED_RAFFLE_KEY_1, { amount: TICKET_PRICE, as: alice })
  });
  it("Alice unsuccessfully calls 'buy' entrypoint because she has already bought one.", async () => {
    await expect_to_fail(async () => {
      await raffle.buy(LOCKED_RAFFLE_KEY_1, {amount : TICKET_PRICE, as : alice})
    }, {prim: "Pair", args: [{string: "KEY_EXISTS"}, {string: "player"}]})
  });
  it("Jack successfully calls 'buy' entrypoint.", async () => {
    await raffle.buy(LOCKED_RAFFLE_KEY_2, { amount: TICKET_PRICE, as: jack })
  });
  it("Bob successfully calls 'buy' entrypoint.", async () => {
    await raffle.buy(LOCKED_RAFFLE_KEY_3, { amount: TICKET_PRICE, as: bob })
  });
})

describe("[RAFFLE] Players reveal their raffle key (at this point a raffle is open and two players participated)", () => {
  it("Alice unsuccessfully calls 'reveal' entrypoint because it is before the 'close_date'.", async () => {
    await expect_to_fail(async () => {
      await raffle.reveal(alice.get_address(), CHEST_KEY_1, { as: alice })
    }, raffle.errors.r5)
  });
  it("set mockup time after close buy date", () => {
    set_mockup_now(new Date(CLOSE_BUY.getTime() + 10 * ONE_SECOND));
  })
  it("Alice unsuccessfully calls 'reveal' entrypoint because of an invalid chest key.", async () => {
    await expect_to_fail(async () => {
      await raffle.reveal(alice.get_address(), INVALID_CHEST_KEY_1, {as : alice})
    }, raffle.errors.INVALID_CHEST_KEY)
  });
  it("Alice successfully calls 'reveal' entrypoint and gets the reveal fee.", async () => {
    const owner_balance_before = await owner.get_balance()
    const alice_balance_before = await alice.get_balance()
    const jack_balance_before = await jack.get_balance()
    const bob_balance_before = await bob.get_balance()

    await raffle.reveal(alice.get_address(), CHEST_KEY_1, { as: alice })

    const owner_balance_after = await owner.get_balance()
    const alice_balance_after = await alice.get_balance()
    const jack_balance_after = await jack.get_balance()
    const bob_balance_after = await bob.get_balance()

    assert(owner_balance_after.equals(owner_balance_before))
    assert(alice_balance_after.to_big_number().isGreaterThan(alice_balance_before.plus(new Tez(0.99)).to_big_number()))
    assert(jack_balance_after.equals(jack_balance_before))
    assert(bob_balance_after.equals(bob_balance_before))
  });
  it("Alice unsuccessfully calls 'reveal' entrypoint because her raffle key is already revealed.", async () => {
    await expect_to_fail(async () => {
      await raffle.reveal(alice.get_address(), CHEST_KEY_1, { as: alice })
    }, raffle.errors.r6)
  });
  it("Jack successfully calls 'reveal' entrypoint and gets the reveal fee.", async () => {
    const owner_balance_before = await owner.get_balance()
    const alice_balance_before = await alice.get_balance()
    const jack_balance_before = await jack.get_balance()
    const bob_balance_before = await bob.get_balance()

    await raffle.reveal(jack.get_address(), CHEST_KEY_2, { as: jack })

    const owner_balance_after = await owner.get_balance()
    const alice_balance_after = await alice.get_balance()
    const jack_balance_after = await jack.get_balance()
    const bob_balance_after = await bob.get_balance()

    assert(owner_balance_after.equals(owner_balance_before))
    assert(alice_balance_after.equals(alice_balance_before))
    assert(jack_balance_after.to_big_number().isGreaterThan(jack_balance_before.plus(new Tez(0.99)).to_big_number()))
    assert(bob_balance_after.equals(bob_balance_before))
  });
})

describe("[RAFFLE] Test 'transfer' entrypoint", () => {
  it("Owner unsucessfully calls 'transfer' entrypoint because Bob is not revealed.", async () => {
    await expect_to_fail(async () => {
      await raffle.transfer({ as: owner })
    }, raffle.errors.r7)
  });
  it("Owner sucessfully calls 'reveal' entrypoint to remove Bob's chest, and gets the unlock reward.", async () => {
    const owner_balance_before = await owner.get_balance()
    const alice_balance_before = await alice.get_balance()
    const jack_balance_before = await jack.get_balance()
    const bob_balance_before = await bob.get_balance()

    await raffle.reveal(bob.get_address(), CHEST_KEY_3, { as: owner })

    const owner_balance_after = await owner.get_balance()
    const alice_balance_after = await alice.get_balance()
    const jack_balance_after = await jack.get_balance()
    const bob_balance_after = await bob.get_balance()

    assert(owner_balance_after.to_big_number().isGreaterThan(owner_balance_before.plus(new Tez(0.99)).to_big_number()))
    assert(alice_balance_after.equals(alice_balance_before))
    assert(jack_balance_after.equals(jack_balance_before))
    assert(bob_balance_after.equals(bob_balance_before))
  });
  it("Owner sucessfully calls 'transfer' entrypoint to send the jackpot to Jack.", async () => {
    const alice_balance_before = await alice.get_balance()
    const jack_balance_before = await jack.get_balance()
    const bob_balance_before = await bob.get_balance()

    await raffle.transfer( { as: owner })

    const alice_balance_after = await alice.get_balance()
    const jack_balance_after = await jack.get_balance()
    const bob_balance_after = await bob.get_balance()

    assert(alice_balance_before.equals(alice_balance_after))
    assert(jack_balance_before.plus(new Tez(62)).equals(jack_balance_after))
    assert(bob_balance_before.equals(bob_balance_after))
  })
})
