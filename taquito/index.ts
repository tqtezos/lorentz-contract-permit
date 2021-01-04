#!/usr/bin/env ts-node

export {};

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner, importKey } from '@taquito/signer';
import { Parser, emitMicheline } from '@taquito/michel-codec';

const fs = require("fs");
const bob_address = 'tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm';
const { email, password, mnemonic, secret } =
  JSON.parse(
    fs.readFileSync(require('os').homedir() + '/Downloads/' + bob_address + '.json').toString()
);

const Tezos = new TezosToolkit('https://api.tez.ie/rpc/delphinet');

Tezos.setProvider({ signer: InMemorySigner.fromFundraiser(email, password, mnemonic.join(' ')) });

importKey(
  Tezos,
  email,
  password,
  mnemonic.join(' '),
  secret
).catch((e) => console.error(e));


const errors_to_missigned_bytes = errors => {
  const errors_with = errors.map(x => x.with).filter(x => x !== undefined);
  if (errors_with.length != 1){
    throw ['errors_to_missigned_bytes: expected one error to fail "with" michelson, but found:', errors_with]
  } else {
    const error_with = errors_with[0];
    const p = new Parser();
    const michelsonCode = p.parseJSON(error_with);
    if (error_with.prim !== 'Pair'){
      throw ['errors_to_missigned_bytes: expected a "Pair", but found:', error_with.prim]
    } else {
      const error_with_args = error_with.args;
      if (error_with_args.length !== 2){
        throw ['errors_to_missigned_bytes: expected two arguments to "Pair", but found:', error_with_args]
      } else {
        if (error_with_args[0].string !== 'missigned'){
          throw ['errors_to_missigned_bytes: expected a "missigned" annotation, but found:', error_with_args[0]]
        } else {
          if (typeof error_with_args[1].bytes !== 'string'){
            throw ['errors_to_missigned_bytes: expected bytes, but found:', error_with_args[1].bytes]
          } else {
            return error_with_args[1].bytes

          }
        }
      }
    }
  }
}


const permit_examples = async () => {
  console.log('inside: permit_examples');

  const permit_address = 'KT1TDmx9JMdYqpFnD3XBrtZbN6nud1GsQnzU';
  const permit_contract = await Tezos.contract.at(permit_address);

  // Check whether bob is actually the admin
  const storage = await permit_contract.storage();
  console.log('bob is admin:', storage['2'] === bob_address);

  const signer_key = await Tezos.signer.publicKey().catch(e => console.error(e));
  const dummy_sig = "edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm";
  const parameter_bytes = "0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c";

  const bytes_to_sign = await permit_contract.methods.permit(signer_key, dummy_sig, parameter_bytes).send().catch((e) => errors_to_missigned_bytes(e.errors));
  console.log('bytes_to_sign', bytes_to_sign);
  const parameter_sig = await Tezos.signer.sign(bytes_to_sign).then(s => s.prefixSig);
  console.log('permit package:', [signer_key, parameter_sig, parameter_bytes]);
  const permit_op = await permit_contract.methods.permit(signer_key, parameter_sig, parameter_bytes).send();
  await permit_op.confirmation().then(() => console.log('permit_op hash:', permit_op.hash));

  console.log('ending: permit_examples');
};

permit_examples();

