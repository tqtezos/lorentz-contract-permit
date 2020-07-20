
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

You can find the following example on carthagenet [here](https://better-call.dev/carthagenet/KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD/operations).

## Setting Up
### Requirements
#### Tezos-client
To set up the tezos-client, follow the instructions in the [Setup Tezos Client](https://assets.tqtezos.com/setup/1-tezos-client) tutorial

### Installing Dependencies
`stack build` Note that this will take some time. 

## Originating

### Print the `permitAdmin42Contract`

Required Haskell imports:

```haskell
import qualified Lorentz.Contracts.Permit.Admin42 as A
import Tezos.Crypto.Orphans ()
import Text.Read
```

(Re)generate the contract:

```haskell
A.printPermitAdmin42 (Just "contracts/permit_admin_42.tz") False
```

### Generate the initial storage

```haskell
printInitPermitAdmin42 (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")

Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
```

### Originate the contract

```bash
$ tezos-client --wait none originate contract PermitAdmin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/permit_admin_42.tz | tr -d '\n')" \
  --init 'Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")' \
  --burn-cap 0.909

Waiting for the node to be bootstrapped before injection...
Current head: BMMeeGpuCgAa (timestamp: 2020-07-20T19:45:56-00:00, validation: 2020-07-20T19:46:15-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 24164 units (will add 100 for safety)
Estimated storage: 818 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooxZr3hhxaFdwtoF29uVzCe1JNyySr1dBizYrnZJt1J7sTPDDBq'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooxZr3hhxaFdwtoF29uVzCe1JNyySr1dBizYrnZJt1J7sTPDDBq to be included --confirmations 30 --branch BMMeeGpuCgAaW7nAfr7wAvLzdjPZWJejbF2TUvWA3Kw4yJiLd7x
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.0032
    Expected counter: 623997
    Gas limit: 24264
    Storage limit: 838 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.0032
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,284) ... +ꜩ0.0032
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (pair %permitParam key (pair signature bytes)) (nat %wrappedParam)) ;
          storage (pair (big_map (pair bytes address) unit) (pair nat address)) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DIP { DIP { DUP ; CDR ; CAR } ;
                           DUP ;
                           CAR ;
                           DIP { CDR ;
                                 DUP ;
                                 DIG 2 ;
                                 PAIR ;
                                 SELF ;
                                 ADDRESS ;
                                 CHAIN_ID ;
                                 PAIR ;
                                 PAIR ;
                                 PACK } } ;
                     DUP ;
                     DIP { DIP { DIP { DUP } } ;
                           CHECK_SIGNATURE ;
                           IF { DROP } { PUSH string "missigned" ; PAIR ; FAILWITH } } ;
                     HASH_KEY ;
                     IMPLICIT_ACCOUNT ;
                     ADDRESS ;
                     SWAP ;
                     PAIR ;
                     DIP { DUP ; CAR ; DIP { CDR } } ;
                     DIP { UNIT ; SOME } ;
                     UPDATE ;
                     DIP { DUP ; CAR ; DIP { CDR } ; PUSH nat 1 ; ADD ; PAIR } ;
                     PAIR ;
                     NIL operation ;
                     PAIR }
                   { DUP ;
                     PUSH nat 42 ;
                     COMPARE ;
                     EQ ;
                     IF {} { PUSH string "not 42" ; FAILWITH } ;
                     PACK ;
                     DIP { DUP ; CAR ; DIP { CDR ; DUP ; CAR ; DIP { CDR } ; SWAP ; DUP } } ;
                     SWAP ;
                     DIP { BLAKE2B ; PAIR } ;
                     DUP ;
                     DIG 2 ;
                     DUP ;
                     DIP { MEM ; IF {} { PUSH string "no permit" ; FAILWITH } ; NONE unit } ;
                     UPDATE ;
                     DIP { SWAP ; PAIR } ;
                     PAIR ;
                     NIL operation ;
                     PAIR } } }
        Initial storage:
          (Pair {} (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT19uax1n4xA3Rs8aJ3Qkzp99C4BjJz2g7G6
        Storage size: 561 bytes
        Updated big_maps:
          New map(10500) of type (big_map (pair bytes address) unit)
        Paid storage size diff: 561 bytes
        Consumed gas: 24164
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.561
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT19uax1n4xA3Rs8aJ3Qkzp99C4BjJz2g7G6 originated.
Contract memorized as PermitAdmin42.
```

Make a bash variable for the contract:

```bash
PERMIT_ADMIN_42="KT19uax1n4xA3Rs8aJ3Qkzp99C4BjJz2g7G6"
```

Get the Chain ID:

```bash
$ tezos-client rpc get /chains/main/chain_id

"NetXjD3HPJJjmcd"
```

Generate an incorrect signature (you can use this one):

```bash
$ alpha-client sign bytes "0x$(dd if=/dev/urandom bs=4096 count=1 | xxd -p | tr -d '\n')" for bob
1+0 records in
1+0 records out
4096 bytes (4.1 kB, 4.0 KiB) copied, 5.9e-05 s, 69.4 MB/s

Signature: edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM

$ BOB_RAND_SIG="edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM"
```

Generate a `permitParam` parameter using the `BOB_RAND_SIG`

```haskell
> A.printPermitAdmin42Param (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM")

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
```

Make a bash variable for it:

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)'
```

Use a `dry-run` to find the expected bytes for the parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint permitParam --arg $PERMIT_PARAM --dry-run

Waiting for the node to be bootstrapped before injection...
Current head: BLQUN7V8bbL1 (timestamp: 2020-07-20T19:49:50-00:00, validation: 2020-07-20T19:50:31-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0
    Expected counter: 623967
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT19uax1n4xA3Rs8aJ3Qkzp99C4BjJz2g7G6
      Entrypoint: permitParam
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This operation FAILED.

Runtime error in contract KT19uax1n4xA3Rs8aJ3Qkzp99C4BjJz2g7G6:
  01: { parameter (or (pair %permitParam key (pair signature bytes)) (nat %wrappedParam)) ;
  02:   storage (pair (big_map (pair bytes address) unit) (pair nat address)) ;
  03:   code { DUP ;
  04:          CAR ;
  05:          DIP { CDR } ;
  06:          IF_LEFT
  07:            { DUP ;
  08:              CAR ;
  09:              DIP { CDR } ;
  10:              DIP { DIP { DUP ; CDR ; CAR } ;
  11:                    DUP ;
  12:                    CAR ;
  13:                    DIP { CDR ;
  14:                          DUP ;
  15:                          DIG 2 ;
  16:                          PAIR ;
  17:                          SELF ;
  18:                          ADDRESS ;
  19:                          CHAIN_ID ;
  20:                          PAIR ;
  21:                          PAIR ;
  22:                          PACK } } ;
  23:              DUP ;
  24:              DIP { DIP { DIP { DUP } } ;
  25:                    CHECK_SIGNATURE ;
  26:                    IF { DROP } { PUSH string "missigned" ; PAIR ; FAILWITH } } ;
  27:              HASH_KEY ;
  28:              IMPLICIT_ACCOUNT ;
  29:              ADDRESS ;
  30:              SWAP ;
  31:              PAIR ;
  32:              DIP { DUP ; CAR ; DIP { CDR } } ;
  33:              DIP { UNIT ; SOME } ;
  34:              UPDATE ;
  35:              DIP { DUP ; CAR ; DIP { CDR } ; PUSH nat 1 ; ADD ; PAIR } ;
  36:              PAIR ;
  37:              NIL operation ;
  38:              PAIR }
  39:            { DUP ;
  40:              PUSH nat 42 ;
  41:              COMPARE ;
  42:              EQ ;
  43:              IF {} { PUSH string "not 42" ; FAILWITH } ;
  44:              PACK ;
  45:              DIP { DUP ; CAR ; DIP { CDR ; DUP ; CAR ; DIP { CDR } ; SWAP ; DUP } } ;
  46:              SWAP ;
  47:              DIP { BLAKE2B ; PAIR } ;
  48:              DUP ;
  49:              DIG 2 ;
  50:              DUP ;
  51:              DIP { MEM ; IF {} { PUSH string "no permit" ; FAILWITH } ; NONE unit } ;
  52:              UPDATE ;
  53:              DIP { SWAP ; PAIR } ;
  54:              PAIR ;
  55:              NIL operation ;
  56:              PAIR } } }
At line 26 characters 66 to 74,
script reached FAILWITH instruction
with
  (Pair "missigned"
        0x05070707070a000000049caecab90a00000016010e872abc26c86c2ecd2f1d4b031da948e86e330f00070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
Fatal error:
```

Store the `"missigned"` bytes from the error in a bash variable:

```bash
PERMIT_BYTES="0x05070707070a000000049caecab90a00000016010e872abc26c86c2ecd2f1d4b031da948e86e330f00070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c"
```

Sign the bytes:

```bash
$ tezos-client sign bytes $PERMIT_BYTES for bob

Signature: edsigte3ht7ktrXaDNQ9A1xWTez8zgRAjAVXeDYE8vGqhgsW3sX6cnt3Qou1UF16zz6Rms7DDDqJpstQ56xhPvfiwQH7i9aGWe8
```

Generate the parameter:

```haskell
A.printPermitAdmin42Param (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigte3ht7ktrXaDNQ9A1xWTez8zgRAjAVXeDYE8vGqhgsW3sX6cnt3Qou1UF16zz6Rms7DDDqJpstQ56xhPvfiwQH7i9aGWe8")

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigte3ht7ktrXaDNQ9A1xWTez8zgRAjAVXeDYE8vGqhgsW3sX6cnt3Qou1UF16zz6Rms7DDDqJpstQ56xhPvfiwQH7i9aGWe8" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
```

Store it in a bash variabe:

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigte3ht7ktrXaDNQ9A1xWTez8zgRAjAVXeDYE8vGqhgsW3sX6cnt3Qou1UF16zz6Rms7DDDqJpstQ56xhPvfiwQH7i9aGWe8" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)'
```

Anyone can submit the signed, hashed parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint permitParam --arg $PERMIT_PARAM --burn-cap 0.067

Waiting for the node to be bootstrapped before injection...
Current head: BLnPZe1tWCdL (timestamp: 2020-07-20T19:53:50-00:00, validation: 2020-07-20T19:53:53-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 24840 units (will add 100 for safety)
Estimated storage: 67 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oo26yUhiN7eYx6CX9qjrYM2KZSoPfMPkWccVaYukYocN9thgYmw'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oo26yUhiN7eYx6CX9qjrYM2KZSoPfMPkWccVaYukYocN9thgYmw to be included --confirmations 30 --branch BLnPZe1tWCdLzRxeWJGe2cy4xbw353HHYBkRLchryBab9UfQYCb
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.002966
    Expected counter: 623967
    Gas limit: 24940
    Storage limit: 87 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.002966
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,284) ... +ꜩ0.002966
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT19uax1n4xA3Rs8aJ3Qkzp99C4BjJz2g7G6
      Entrypoint: permitParam
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigte3ht7ktrXaDNQ9A1xWTez8zgRAjAVXeDYE8vGqhgsW3sX6cnt3Qou1UF16zz6Rms7DDDqJpstQ56xhPvfiwQH7i9aGWe8"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This transaction was successfully applied
      Updated storage:
        (Pair 10500 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Set map(10500)[(Pair 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c
              0x0000aad02222472cdf9892a3011c01caf6407f027081)] to Unit
      Storage size: 628 bytes
      Paid storage size diff: 67 bytes
      Consumed gas: 24840
      Balance updates:
        tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ0.067
```

Then, anyone can submit `42` in place of Bob:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint wrappedParam --arg 42 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLo4THYkvac2 (timestamp: 2020-07-20T19:54:50-00:00, validation: 2020-07-20T19:55:12-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 24799 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'oo1z3vD896cCiferDQh5Jn3DwnfkCARiWoFDxZqoNqX6gTLJtH2'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oo1z3vD896cCiferDQh5Jn3DwnfkCARiWoFDxZqoNqX6gTLJtH2 to be included --confirmations 30 --branch BLo4THYkvac2Wkw8aND9seTmioCH3duDvudiQwGWkzGdXWhwnpf
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.002761
    Expected counter: 623968
    Gas limit: 24899
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.002761
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,284) ... +ꜩ0.002761
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT19uax1n4xA3Rs8aJ3Qkzp99C4BjJz2g7G6
      Entrypoint: wrappedParam
      Parameter: 42
      This transaction was successfully applied
      Updated storage:
        (Pair 10500 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Unset map(10500)[(Pair 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c
              0x0000aad02222472cdf9892a3011c01caf6407f027081)]
      Storage size: 561 bytes
      Consumed gas: 24799
```


# Control Group

[Here](https://better-call.dev/carthagenet/KT1MMCMQ5hQRgJDyTVSrQpBm35L2XFgRax2b/operations)'s 
an origination and call of `Admin42` without permit.

(Re)generate the contract:

```haskell
A.printAdmin42 (Just "contracts/admin_42.tz") False
```

### Originate the contract

```bash
$ tezos-client --wait none originate contract Admin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/admin_42.tz | tr -d '\n')" \
  --init "\"$BOB_ADDRESS\"" \
  --burn-cap 0.41

Waiting for the node to be bootstrapped before injection...
Current head: BLrZ4vKztWVT (timestamp: 2020-07-15T21:59:56-00:00, validation: 2020-07-15T22:00:14-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13498 units (will add 100 for safety)
Estimated storage: 410 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oo35Z5cRyBoTgrwzscq9RZsvrqk167eRTqCBByWiUAAT7b4Etiq'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oo35Z5cRyBoTgrwzscq9RZsvrqk167eRTqCBByWiUAAT7b4Etiq to be included --confirmations 30 --branch BLrZ4vKztWVTQMjq2LfaPA9h5tEdAQvQZcRKDwN7ijLYoGJddCd
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001756
    Expected counter: 623995
    Gas limit: 13598
    Storage limit: 430 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001756
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,278) ... +ꜩ0.001756
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter nat ;
          storage address ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 PUSH nat 42 ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "not 42" ; FAILWITH } ;
                 DUP ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF {} { PUSH string "not admin" ; FAILWITH } ;
                 NIL operation ;
                 PAIR } }
        Initial storage: "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1MMCMQ5hQRgJDyTVSrQpBm35L2XFgRax2b
        Storage size: 153 bytes
        Paid storage size diff: 153 bytes
        Consumed gas: 13498
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.153
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1MMCMQ5hQRgJDyTVSrQpBm35L2XFgRax2b originated.
Contract memorized as Admin42.
```

Make a bash variable for the contract:

```bash
ADMIN_42="KT1MMCMQ5hQRgJDyTVSrQpBm35L2XFgRax2b"
```

```bash
$ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $ADMIN_42 \
  --arg 42 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLQCfWxj1iZ7 (timestamp: 2020-07-15T22:01:26-00:00, validation: 2020-07-15T22:01:46-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13742 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'oobexiHC9EauQGNfNxdig25YP6TzN4qJktW5TvobLkCEcUAB9Jp'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oobexiHC9EauQGNfNxdig25YP6TzN4qJktW5TvobLkCEcUAB9Jp to be included --confirmations 30 --branch BLQCfWxj1iZ7Q2UPvp8VyvCq6QWCMooUtm3RzamDuv8Yms8D5kC
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001642
    Expected counter: 623996
    Gas limit: 13842
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001642
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,278) ... +ꜩ0.001642
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1MMCMQ5hQRgJDyTVSrQpBm35L2XFgRax2b
      Parameter: 42
      This transaction was successfully applied
      Updated storage: 0x0000aad02222472cdf9892a3011c01caf6407f027081
      Storage size: 153 bytes
      Consumed gas: 13742
```

# Misc

Currently not working:

```haskell
A.printPermitAdmin42Bytes (Tezos.Core.ChainIdUnsafe "NetXjD3HPJJjmcd") (read "KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD") 0

"0x05070707070a0000000f4e6574586a443348504a4a6a6d63640a00000016018cf9ea3e34a7a02673d4501b483984e4349a245f00070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c"
```
