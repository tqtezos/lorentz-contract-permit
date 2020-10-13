
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

You can find the following example on carthagenet [here](https://better-call.dev/carthagenet/KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR/operations).

## Setting Up
### Requirements
#### Tezos-client
To set up the tezos-client, follow the instructions in the [Setup Tezos Client](https://assets.tqtezos.com/setup/1-tezos-client) tutorial

### Installing Dependencies
`stack build` Note that this will take some time. 

## Originating

### Print the `permitAdmin42SetContract`

Required Haskell imports:

```haskell
import qualified Lorentz.Contracts.Permit.Admin42.Set as A
import Tezos.Crypto.Orphans ()
import Text.Read
```

(Re)generate the contract:

```haskell
A.printPermitAdmin42Set (Just "contracts/permit_admin_42_set.tz") False
```

### Generate the initial storage

```haskell
A.printInitPermitAdmin42Set (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")

Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
```

### Originate the contract

```bash
$ tezos-client --wait none originate contract PermitAdmin42Set \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/permit_admin_42_set.tz | tr -d '\n')" \
  --init 'Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")' \
  --burn-cap 0.885

Waiting for the node to be bootstrapped before injection...
Current head: BLazcJ2281nA (timestamp: 2020-07-22T20:01:20-00:00, validation: 2020-07-22T20:01:51-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 26717 units (will add 100 for safety)
Estimated storage: 885 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opVxf8UtvTqVtuhmLY6qh8gUcGaweNP4ZhssSPMYqQfdY8kSjaJ'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opVxf8UtvTqVtuhmLY6qh8gUcGaweNP4ZhssSPMYqQfdY8kSjaJ to be included --confirmations 30 --branch BLazcJ2281nAqjDp7HZMBcPH8y67WhXkgxARdZN1h2BdPfwRZqR
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003522
    Expected counter: 624002
    Gas limit: 26817
    Storage limit: 905 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003522
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,286) ... +ꜩ0.003522
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (pair %permit key (pair signature bytes)) (nat %wrapped)) ;
          storage (pair (big_map address (set bytes)) (pair nat address)) ;
          code { DUP ;
                 CAR ;
                 IF_LEFT
                   { DUP ;
                     CAR ;
                     DUP ;
                     DIP { SWAP ;
                           CDR ;
                           DUP ;
                           CAR ;
                           DIP { CDR ;
                                 DUP ;
                                 DIP { DIP { HASH_KEY ; IMPLICIT_ACCOUNT ; ADDRESS } ; PAIR ; SWAP } ;
                                 SWAP ;
                                 CDR ;
                                 DUP ;
                                 CDR ;
                                 CAR ;
                                 DIP { SWAP } ;
                                 PAIR ;
                                 SELF ;
                                 ADDRESS ;
                                 CHAIN_ID ;
                                 PAIR ;
                                 PAIR ;
                                 PACK } } ;
                     DIP { DIP { DUP } } ;
                     CHECK_SIGNATURE ;
                     IF { DROP } { PUSH string "missigned" ; PAIR ; FAILWITH } ;
                     SWAP ;
                     DUP ;
                     CAR ;
                     DIP { DIP { DUP ;
                                 CDR ;
                                 DUP ;
                                 CAR ;
                                 PUSH nat 1 ;
                                 ADD ;
                                 DIP { CDR } ;
                                 PAIR ;
                                 SWAP ;
                                 CAR ;
                                 DUP } ;
                           CDR ;
                           DUP ;
                           DIP { SWAP } ;
                           GET ;
                           IF_NONE { EMPTY_SET bytes } {} ;
                           PUSH bool True } ;
                     UPDATE ;
                     SOME ;
                     SWAP ;
                     UPDATE ;
                     PAIR ;
                     NIL operation ;
                     PAIR }
                   { DUP ;
                     PUSH nat 42 ;
                     COMPARE ;
                     EQ ;
                     IF {} { PUSH string "not 42" ; FAILWITH } ;
                     PACK ;
                     BLAKE2B ;
                     DIP { CDR ; DUP ; CAR ; DIP { CDR ; DUP ; CDR } } ;
                     SWAP ;
                     DIP { PAIR } ;
                     SWAP ;
                     PAIR ;
                     DUP ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DUP ;
                     CDR ;
                     DIP { SWAP } ;
                     GET ;
                     IF_NONE
                       { FAILWITH }
                       { SWAP ;
                         DIP { DUP } ;
                         CAR ;
                         MEM ;
                         IF { DIP { DUP ; CAR ; DIP { CDR } ; DUP ; CAR } ;
                              SWAP ;
                              DIP { PUSH bool False } ;
                              UPDATE ;
                              SOME ;
                              SWAP ;
                              CDR ;
                              UPDATE }
                            { FAILWITH } } ;
                     PAIR ;
                     NIL operation ;
                     PAIR } } }
        Initial storage:
          (Pair {} (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1Uj1FA9zW6erGo2wR6HGxjhdTuLQWpTghA
        Storage size: 628 bytes
        Updated big_maps:
          New map(10833) of type (big_map address (set bytes))
        Paid storage size diff: 628 bytes
        Consumed gas: 26717
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.628
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1Uj1FA9zW6erGo2wR6HGxjhdTuLQWpTghA originated.
Contract memorized as PermitAdmin42Set.
```

Make a bash variable for the contract:

```bash
PERMIT_ADMIN_42_SET="KT1Uj1FA9zW6erGo2wR6HGxjhdTuLQWpTghA"
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

Generate a `permit` parameter using the `BOB_RAND_SIG`

```haskell
> A.printPermitAdmin42SetParam (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM")

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
```

Make a bash variable for it:

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)'
```

Use a `dry-run` to find the expected bytes for the parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42_SET \
  --entrypoint permit --arg $PERMIT_PARAM --dry-run

Waiting for the node to be bootstrapped before injection...
Current head: BM1Y2s2bdNeZ (timestamp: 2020-07-22T20:03:00-00:00, validation: 2020-07-22T20:03:35-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0
    Expected counter: 623973
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1Uj1FA9zW6erGo2wR6HGxjhdTuLQWpTghA
      Entrypoint: permit
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This operation FAILED.

Runtime error in contract KT1Uj1FA9zW6erGo2wR6HGxjhdTuLQWpTghA:
  01: { parameter (or (pair %permit key (pair signature bytes)) (nat %wrapped)) ;
  02:   storage (pair (big_map address (set bytes)) (pair nat address)) ;
  03:   code { DUP ;
  04:          CAR ;
  05:          IF_LEFT
  06:            { DUP ;
  07:              CAR ;
  08:              DUP ;
  09:              DIP { SWAP ;
  10:                    CDR ;
  11:                    DUP ;
  12:                    CAR ;
  13:                    DIP { CDR ;
  14:                          DUP ;
  15:                          DIP { DIP { HASH_KEY ; IMPLICIT_ACCOUNT ; ADDRESS } ; PAIR ; SWAP } ;
  16:                          SWAP ;
  17:                          CDR ;
  18:                          DUP ;
  19:                          CDR ;
  20:                          CAR ;
  21:                          DIP { SWAP } ;
  22:                          PAIR ;
  23:                          SELF ;
  24:                          ADDRESS ;
  25:                          CHAIN_ID ;
  26:                          PAIR ;
  27:                          PAIR ;
  28:                          PACK } } ;
  29:              DIP { DIP { DUP } } ;
  30:              CHECK_SIGNATURE ;
  31:              IF { DROP } { PUSH string "missigned" ; PAIR ; FAILWITH } ;
  32:              SWAP ;
  33:              DUP ;
  34:              CAR ;
  35:              DIP { DIP { DUP ;
  36:                          CDR ;
  37:                          DUP ;
  38:                          CAR ;
  39:                          PUSH nat 1 ;
  40:                          ADD ;
  41:                          DIP { CDR } ;
  42:                          PAIR ;
  43:                          SWAP ;
  44:                          CAR ;
  45:                          DUP } ;
  46:                    CDR ;
  47:                    DUP ;
  48:                    DIP { SWAP } ;
  49:                    GET ;
  50:                    IF_NONE { EMPTY_SET bytes } {} ;
  51:                    PUSH bool True } ;
  52:              UPDATE ;
  53:              SOME ;
  54:              SWAP ;
  55:              UPDATE ;
  56:              PAIR ;
  57:              NIL operation ;
  58:              PAIR }
  59:            { DUP ;
  60:              PUSH nat 42 ;
  61:              COMPARE ;
  62:              EQ ;
  63:              IF {} { PUSH string "not 42" ; FAILWITH } ;
  64:              PACK ;
  65:              BLAKE2B ;
  66:              DIP { CDR ; DUP ; CAR ; DIP { CDR ; DUP ; CDR } } ;
  67:              SWAP ;
  68:              DIP { PAIR } ;
  69:              SWAP ;
  70:              PAIR ;
  71:              DUP ;
  72:              DUP ;
  73:              CAR ;
  74:              DIP { CDR } ;
  75:              DUP ;
  76:              CDR ;
  77:              DIP { SWAP } ;
  78:              GET ;
  79:              IF_NONE
  80:                { FAILWITH }
  81:                { SWAP ;
  82:                  DIP { DUP } ;
  83:                  CAR ;
  84:                  MEM ;
  85:                  IF { DIP { DUP ; CAR ; DIP { CDR } ; DUP ; CAR } ;
  86:                       SWAP ;
  87:                       DIP { PUSH bool False } ;
  88:                       UPDATE ;
  89:                       SOME ;
  90:                       SWAP ;
  91:                       CDR ;
  92:                       UPDATE }
  93:                     { FAILWITH } } ;
  94:              PAIR ;
  95:              NIL operation ;
  96:              PAIR } } }
At line 31 characters 60 to 68,
script reached FAILWITH instruction
with
  (Pair "missigned"
        0x05070707070a000000049caecab90a0000001601dcf1431e9fa9c4fc3b0859e3ea91bbfecfbb725200070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
Fatal error:
  transfer simulation failed
```

Store the `"missigned"` bytes from the error in a bash variable:

```bash
PERMIT_BYTES="0x05070707070a000000049caecab90a0000001601dcf1431e9fa9c4fc3b0859e3ea91bbfecfbb725200070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c"
```

Sign the bytes:

```bash
$ tezos-client sign bytes $PERMIT_BYTES for bob

Signature: edsigu5hhPSqYUbfgdZFXvpjMnArH1GS9BqQfM3ddhFyBEG2rQSKozQ85yt16X63e8KRrWNFYpCTwRZuQUUaL59B1C5Y1pJAbr6
```

Generate the parameter:

```haskell
A.printPermitAdmin42SetParam (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigu5hhPSqYUbfgdZFXvpjMnArH1GS9BqQfM3ddhFyBEG2rQSKozQ85yt16X63e8KRrWNFYpCTwRZuQUUaL59B1C5Y1pJAbr6")

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu5hhPSqYUbfgdZFXvpjMnArH1GS9BqQfM3ddhFyBEG2rQSKozQ85yt16X63e8KRrWNFYpCTwRZuQUUaL59B1C5Y1pJAbr6" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
```

Store it in a bash variabe:

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu5hhPSqYUbfgdZFXvpjMnArH1GS9BqQfM3ddhFyBEG2rQSKozQ85yt16X63e8KRrWNFYpCTwRZuQUUaL59B1C5Y1pJAbr6" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)'
```

Anyone can submit the signed, hashed parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42_SET \
  --entrypoint permit --arg $PERMIT_PARAM --burn-cap 0.0001

Waiting for the node to be bootstrapped before injection...
Current head: BLiAcPnbqAE1 (timestamp: 2020-07-22T20:05:10-00:00, validation: 2020-07-22T20:05:30-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 27705 units (will add 100 for safety)
Estimated storage: 107 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oovSjnAQqaHtL89CbN4juThiEBFJJ93m4zbm1uzhB3mpaRVJ1qk'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oovSjnAQqaHtL89CbN4juThiEBFJJ93m4zbm1uzhB3mpaRVJ1qk to be included --confirmations 30 --branch BLiAcPnbqAE1JQHLPEx3dkDEoPBWiFG3edGNforFyaoZK39htZ3
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.003248
    Expected counter: 623973
    Gas limit: 27805
    Storage limit: 127 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.003248
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,286) ... +ꜩ0.003248
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1Uj1FA9zW6erGo2wR6HGxjhdTuLQWpTghA
      Entrypoint: permit
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigu5hhPSqYUbfgdZFXvpjMnArH1GS9BqQfM3ddhFyBEG2rQSKozQ85yt16X63e8KRrWNFYpCTwRZuQUUaL59B1C5Y1pJAbr6"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This transaction was successfully applied
      Updated storage:
        (Pair 10833 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Set map(10833)[0x0000aad02222472cdf9892a3011c01caf6407f027081] to { 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c }
      Storage size: 735 bytes
      Paid storage size diff: 107 bytes
      Consumed gas: 27705
      Balance updates:
        tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ0.107
```

On the second run for a user we have:

```
  Gas limit: 27968
  Storage limit: 0 bytes
```

I.e., the storage can be completely reused.

Then, anyone can submit `42` in place of Bob:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42_SET \
  --entrypoint wrapped --arg 42 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BM4vFkHgsFcs (timestamp: 2020-07-22T20:05:40-00:00, validation: 2020-07-22T20:05:59-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 26759 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooHNPmA3tZrjLHzwC7rHuapNgzgNFsAWJHEYy2KEcczqjKoJY5X'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooHNPmA3tZrjLHzwC7rHuapNgzgNFsAWJHEYy2KEcczqjKoJY5X to be included --confirmations 30 --branch BM4vFkHgsFcsMfd3ukGrk8KmxNAZirVmT7J9E77L5BPNuiPa3tD
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.002952
    Expected counter: 623974
    Gas limit: 26859
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.002952
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,286) ... +ꜩ0.002952
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1Uj1FA9zW6erGo2wR6HGxjhdTuLQWpTghA
      Entrypoint: wrapped
      Parameter: 42
      This transaction was successfully applied
      Updated storage:
        (Pair 10833 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Set map(10833)[0x0000aad02222472cdf9892a3011c01caf6407f027081] to {}
      Storage size: 698 bytes
      Consumed gas: 26759
```

Submissions have a similar cost for subsequent runs.


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
