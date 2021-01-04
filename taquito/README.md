# permit-taquito

Everyting is inside of [`index.ts`](index.ts)

## Originating the contract

See the top-level directory for more detail.

```bash
❯❯❯ tezos-client --wait none originate contract PermitAdmin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/permit_admin_42.tz | tr -d '\n')" \
  --init 'Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")' \
  --burn-cap 0.781 --force

Waiting for the node to be bootstrapped before injection...
Current head: BMbR1Sudgmz5 (timestamp: 2021-01-04T18:44:40-00:00, validation: 2021-01-04T18:44:58-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 3921.891 units (will add 100 for safety)
Estimated storage: 781 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooPYWQHqoczU39kU2Bc17GoCwv3EE4ZHJWwqzaZWCNvtTtQ741Q'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooPYWQHqoczU39kU2Bc17GoCwv3EE4ZHJWwqzaZWCNvtTtQ741Q to be included --confirmations 30 --branch BMbR1Sudgmz5pZ8n73A9odzeJyZDcWkTHVB53UvnFbec5nyDN2k
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001138
    Expected counter: 526602
    Gas limit: 4022
    Storage limit: 801 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001138
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,162) ... +ꜩ0.001138
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (pair %permit key (pair signature bytes)) (nat %wrapped)) ;
        ...
                     PAIR } } }
        Initial storage:
          (Pair {} (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1TDmx9JMdYqpFnD3XBrtZbN6nud1GsQnzU
        Storage size: 524 bytes
        Updated big_maps:
          New map(38096) of type (big_map (pair bytes address) unit)
        Paid storage size diff: 524 bytes
        Consumed gas: 3921.891
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.131
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425

New contract KT1TDmx9JMdYqpFnD3XBrtZbN6nud1GsQnzU originated.
Contract memorized as PermitAdmin42.
```

[The originated contract](https://better-call.dev/delphinet/KT1TDmx9JMdYqpFnD3XBrtZbN6nud1GsQnzU/operations)


## Examples

The examples are defined in `permit_examples`

`npm test` output:

```bash
inside: permit_examples
bob is admin: true
bytes_to_sign 05070707070a00000004a83650210a0000001601cc71fa0ddd7113f936438158e407160675706ae800070700010a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c
permit package: [ 'edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb',
  'edsigtnykzDLqjpufz2WuJZrfTYD3xKEwysn89K3EohARGcbRWHVbTuuJoTuQPybRCKE3rx3S33Y7R5QuG7rfJCswMxzFY2P1LV',
  '0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c' ]
permit_op hash: opa48muX5dwyYEVNgwMra9tJtmXpxJ3BW6fM28D6qtC14PKdq2m
ending: permit_examples
```

