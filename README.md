
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
Current head: BLNUVR9KLn5V (timestamp: 2020-07-15T19:09:56-00:00, validation: 2020-07-15T19:10:21-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 26689 units (will add 100 for safety)
Estimated storage: 909 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opW54qeddwNaCLY67YmaoYEYh2iDRLCupMmWqkyHbvQYgWp1YEJ'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opW54qeddwNaCLY67YmaoYEYh2iDRLCupMmWqkyHbvQYgWp1YEJ to be included --confirmations 30 --branch BLNUVR9KLn5VmNxaxVMoEBWtseGJFoy1hWuGy4s2MQmVA3LkMPj
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003543
    Expected counter: 623994
    Gas limit: 26789
    Storage limit: 929 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003543
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,278) ... +ꜩ0.003543
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
                   { LAMBDA
                       (pair (big_map (pair bytes address) unit) (pair bytes address))
                       (big_map (pair bytes address) unit)
                       { DUP ;
                         CAR ;
                         DIP { CDR } ;
                         DIP { DUP ; CAR ; DIP { CDR } ; BLAKE2B ; PAIR } ;
                         DUP ;
                         DIG 2 ;
                         DUP ;
                         DIP { MEM ; IF {} { PUSH string "no permit" ; FAILWITH } ; NONE unit } ;
                         UPDATE } ;
                     DIP { DUP ;
                           PUSH nat 42 ;
                           COMPARE ;
                           EQ ;
                           IF {} { PUSH string "not 42" ; FAILWITH } ;
                           PACK ;
                           DIP { DUP ; CAR ; DIP { CDR ; DUP ; CAR ; DIP { CDR } ; SWAP ; DUP } } ;
                           SWAP ;
                           DIP { PAIR } ;
                           PAIR } ;
                     SWAP ;
                     EXEC ;
                     DIP { SWAP ; PAIR } ;
                     PAIR ;
                     NIL operation ;
                     PAIR } } }
        Initial storage:
          (Pair {} (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD
        Storage size: 652 bytes
        Updated big_maps:
          New map(10100) of type (big_map (pair bytes address) unit)
        Paid storage size diff: 652 bytes
        Consumed gas: 26689
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.652
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD originated.
Contract memorized as PermitAdmin42.
```

Make a bash variable for the contract:

```bash
PERMIT_ADMIN_42="KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD"
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
A.printPermitAdmin42Param (Tezos.Core.ChainIdUnsafe "NetXjD3HPJJjmcd") (read "KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD") 0 (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM")

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
Current head: BLeMGzrrKgqR (timestamp: 2020-07-15T21:49:46-00:00, validation: 2020-07-15T21:49:56-00:00)
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
      To: KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD
      Entrypoint: permitParam
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This operation FAILED.

Runtime error in contract KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD:
  01: { parameter (or (pair %permitParam key (pair signature bytes)) (nat %wrappedParam)) ;
  ..
  66:              PAIR } } }
At line 26 characters 66 to 74,
script reached FAILWITH instruction
with
  (Pair "missigned"
        0x05070707070a000000049caecab90a00000016018cf9ea3e34a7a02673d4501b483984e4349a245f00070700010a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
Fatal error:
  transfer simulation failed
```

Store the `"missigned"` bytes from the error in a bash variable:

```bash
PERMIT_BYTES="0x05070707070a000000049caecab90a00000016018cf9ea3e34a7a02673d4501b483984e4349a245f00070700010a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c"
```

Sign the bytes:

```bash
$ tezos-client sign bytes $PERMIT_BYTES for bob

Signature: edsigu4arMPRfN9gMJjE7LTFjgraGC2Qx1Tx4pd613MQdqyaaPQgXWAAHNLzvg5by5fLHZcrCAGq28sCAJoBxzBh86QfYAV5FnK
```

Generate the parameter:

```haskell
A.printPermitAdmin42Param (Tezos.Core.ChainIdUnsafe "NetXjD3HPJJjmcd") (read "KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD") 0 (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigu4arMPRfN9gMJjE7LTFjgraGC2Qx1Tx4pd613MQdqyaaPQgXWAAHNLzvg5by5fLHZcrCAGq28sCAJoBxzBh86QfYAV5FnK")

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu4arMPRfN9gMJjE7LTFjgraGC2Qx1Tx4pd613MQdqyaaPQgXWAAHNLzvg5by5fLHZcrCAGq28sCAJoBxzBh86QfYAV5FnK" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
```

Store it in a bash variabe:

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu4arMPRfN9gMJjE7LTFjgraGC2Qx1Tx4pd613MQdqyaaPQgXWAAHNLzvg5by5fLHZcrCAGq28sCAJoBxzBh86QfYAV5FnK" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)'
```

Anyone can submit the signed, hashed parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint permitParam --arg $PERMIT_PARAM --burn-cap 0.067

Waiting for the node to be bootstrapped before injection...
Current head: BMLL73nzfHkH (timestamp: 2020-07-15T21:35:36-00:00, validation: 2020-07-15T21:35:50-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 26910 units (will add 100 for safety)
Estimated storage: 67 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooixURMhJcf9irFEobyPSBDsznaA7EyvuJT3D2v6SRAKPDieZfR'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooixURMhJcf9irFEobyPSBDsznaA7EyvuJT3D2v6SRAKPDieZfR to be included --confirmations 30 --branch BMLL73nzfHkHtLsXeZfs9VdUBh3xog5e6Houm1ywo5EW8BxxYXR
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.003173
    Expected counter: 623965
    Gas limit: 27010
    Storage limit: 87 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.003173
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,278) ... +ꜩ0.003173
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD
      Entrypoint: permitParam
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigu4arMPRfN9gMJjE7LTFjgraGC2Qx1Tx4pd613MQdqyaaPQgXWAAHNLzvg5by5fLHZcrCAGq28sCAJoBxzBh86QfYAV5FnK"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This transaction was successfully applied
      Updated storage:
        (Pair 10100 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Set map(10100)[(Pair 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c
              0x0000aad02222472cdf9892a3011c01caf6407f027081)] to Unit
      Storage size: 719 bytes
      Paid storage size diff: 67 bytes
      Consumed gas: 26910
      Balance updates:
        tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ0.067
```

Then, anyone can submit `42` in place of Bob:

```bash
$ alpha-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint wrappedParam --arg 42 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLNPu6WmtuQJ (timestamp: 2020-07-15T21:37:06-00:00, validation: 2020-07-15T21:37:12-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 26876 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooAbgSw7m34SfV88KYZukkxQPTKNLxLhpNDKu8LtbeS7ez6F9G1'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooAbgSw7m34SfV88KYZukkxQPTKNLxLhpNDKu8LtbeS7ez6F9G1 to be included --confirmations 30 --branch BLNPu6WmtuQJMrWpHiuEXMBUjbd5PjPsUJJs2MFHfv4CNWXAt2R
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.002969
    Expected counter: 623966
    Gas limit: 26976
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.002969
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,278) ... +ꜩ0.002969
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD
      Entrypoint: wrappedParam
      Parameter: 42
      This transaction was successfully applied
      Updated storage:
        (Pair 10100 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Unset map(10100)[(Pair 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c
              0x0000aad02222472cdf9892a3011c01caf6407f027081)]
      Storage size: 652 bytes
      Consumed gas: 26876
```


# Misc

Currently not working:

```haskell
A.printPermitAdmin42Bytes (Tezos.Core.ChainIdUnsafe "NetXjD3HPJJjmcd") (read "KT1MSBZ2BDUL3vMgFhouRWJrbbgxZAG4XQFD") 0

"0x05070707070a0000000f4e6574586a443348504a4a6a6d63640a00000016018cf9ea3e34a7a02673d4501b483984e4349a245f00070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c"
```
