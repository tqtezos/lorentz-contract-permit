
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
  --burn-cap 0.781

Waiting for the node to be bootstrapped before injection...
Current head: BMCTSg2nXiga (timestamp: 2020-07-20T22:05:16-00:00, validation: 2020-07-20T22:05:35-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 23500 units (will add 100 for safety)
Estimated storage: 781 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooZt41yBBVWeLALpGZgjVed1FwNYTnKaMcLQxRbAWnBUt23bZAC'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooZt41yBBVWeLALpGZgjVed1FwNYTnKaMcLQxRbAWnBUt23bZAC to be included --confirmations 30 --branch BMCTSg2nXigao2Tcy6UXeMGctXTATjUHuhf49kRBek916y8vmrm
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003096
    Expected counter: 624000
    Gas limit: 23600
    Storage limit: 801 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003096
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,284) ... +ꜩ0.003096
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (pair %permit key (pair signature bytes)) (nat %wrapped)) ;
          storage (pair (big_map (pair bytes address) unit) (pair nat address)) ;
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
                     DIP { DUP ;
                           CDR ;
                           DUP ;
                           CAR ;
                           PUSH nat 1 ;
                           ADD ;
                           DIP { CDR } ;
                           PAIR ;
                           SWAP ;
                           CAR ;
                           UNIT ;
                           SOME } ;
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
                     MEM ;
                     IF { DUP ; CAR ; DIP { CDR ; NONE unit } ; UPDATE }
                        { PUSH string "no permit" ; FAILWITH } ;
                     PAIR ;
                     NIL operation ;
                     PAIR } } }
        Initial storage:
          (Pair {} (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR
        Storage size: 524 bytes
        Updated big_maps:
          New map(10509) of type (big_map (pair bytes address) unit)
        Paid storage size diff: 524 bytes
        Consumed gas: 23500
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.524
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR originated.
Contract memorized as PermitAdmin42.
```

Make a bash variable for the contract:

```bash
PERMIT_ADMIN_42="KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR"
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
  --entrypoint permit --arg $PERMIT_PARAM --dry-run

Waiting for the node to be bootstrapped before injection...
Current head: BL8oRMHVPFof (timestamp: 2020-07-20T22:06:16-00:00, validation: 2020-07-20T22:06:32-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0
    Expected counter: 623971
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR
      Entrypoint: permit
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This operation FAILED.

Runtime error in contract KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR:
  01: { parameter (or (pair %permit key (pair signature bytes)) (nat %wrapped)) ;
  02:   storage (pair (big_map (pair bytes address) unit) (pair nat address)) ;
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
  33:              DIP { DUP ;
  34:                    CDR ;
  35:                    DUP ;
  36:                    CAR ;
  37:                    PUSH nat 1 ;
  38:                    ADD ;
  39:                    DIP { CDR } ;
  40:                    PAIR ;
  41:                    SWAP ;
  42:                    CAR ;
  43:                    UNIT ;
  44:                    SOME } ;
  45:              UPDATE ;
  46:              PAIR ;
  47:              NIL operation ;
  48:              PAIR }
  49:            { DUP ;
  50:              PUSH nat 42 ;
  51:              COMPARE ;
  52:              EQ ;
  53:              IF {} { PUSH string "not 42" ; FAILWITH } ;
  54:              PACK ;
  55:              BLAKE2B ;
  56:              DIP { CDR ; DUP ; CAR ; DIP { CDR ; DUP ; CDR } } ;
  57:              SWAP ;
  58:              DIP { PAIR } ;
  59:              SWAP ;
  60:              PAIR ;
  61:              DUP ;
  62:              DUP ;
  63:              CAR ;
  64:              DIP { CDR } ;
  65:              MEM ;
  66:              IF { DUP ; CAR ; DIP { CDR ; NONE unit } ; UPDATE }
  67:                 { PUSH string "no permit" ; FAILWITH } ;
  68:              PAIR ;
  69:              NIL operation ;
  70:              PAIR } } }
At line 31 characters 60 to 68,
script reached FAILWITH instruction
with
  (Pair "missigned"
        0x05070707070a000000049caecab90a000000160182c415155ce8ec47d60dfe579065addc370a4e8400070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
Fatal error:
  transfer simulation failed
```

Store the `"missigned"` bytes from the error in a bash variable:

```bash
PERMIT_BYTES="0x05070707070a000000049caecab90a000000160182c415155ce8ec47d60dfe579065addc370a4e8400070700000a000000200f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c"
```

Sign the bytes:

```bash
$ tezos-client sign bytes $PERMIT_BYTES for bob

Signature: edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm
```

Generate the parameter:

```haskell
A.printPermitAdmin42Param (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm")

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)
```

Store it in a bash variabe:

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm" 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c)'
```

Anyone can submit the signed, hashed parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint permit --arg $PERMIT_PARAM --burn-cap 0.067

Waiting for the node to be bootstrapped before injection...
Current head: BL6wcDXfo5tc (timestamp: 2020-07-20T22:07:26-00:00, validation: 2020-07-20T22:08:02-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 24360 units (will add 100 for safety)
Estimated storage: 67 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooF1y4wBedmk7JE7HQLZo9Gn6VBD9guJSsHVKP852HgeSMNx6bM'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooF1y4wBedmk7JE7HQLZo9Gn6VBD9guJSsHVKP852HgeSMNx6bM to be included --confirmations 30 --branch BL6wcDXfo5tcB3MWmxWPzGQREheiWVXUpDqpdY7G57CKPZ86hCP
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.002913
    Expected counter: 623971
    Gas limit: 24460
    Storage limit: 87 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.002913
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,284) ... +ꜩ0.002913
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR
      Entrypoint: permit
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigu5scrvoY2AB7cnHzUd7x7ZvXEMYkArKeehN5ZXNkmfUSkyApHcW5vPcjbuTrnHUMt8mJkWmo8WScNgKL3vu9akFLAXvHxm"
                             0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c))
      This transaction was successfully applied
      Updated storage:
        (Pair 10509 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Set map(10509)[(Pair 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c
              0x0000aad02222472cdf9892a3011c01caf6407f027081)] to Unit
      Storage size: 591 bytes
      Paid storage size diff: 67 bytes
      Consumed gas: 24360
      Balance updates:
        tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ0.067
```

Then, anyone can submit `42` in place of Bob:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint wrapped --arg 42 --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLZ1TWybYM5z (timestamp: 2020-07-20T22:08:38-00:00, validation: 2020-07-20T22:08:57-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 24319 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'onsnsPLXKyFW1kDrGZftdbepkzqqp8RkcYCQPX9yMLbwtdi1ewU'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onsnsPLXKyFW1kDrGZftdbepkzqqp8RkcYCQPX9yMLbwtdi1ewU to be included --confirmations 30 --branch BLZ1TWybYM5zpQNEPxKUDdVQTc5nJY6ViDQSzGPDnarF4VT2ojm
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.002708
    Expected counter: 623972
    Gas limit: 24419
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.002708
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,284) ... +ꜩ0.002708
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR
      Entrypoint: wrapped
      Parameter: 42
      This transaction was successfully applied
      Updated storage:
        (Pair 10509 (Pair 1 0x0000aad02222472cdf9892a3011c01caf6407f027081))
      Updated big_maps:
        Unset map(10509)[(Pair 0x0f0db0ce6f057a8835adb6a2c617fd8a136b8028fac90aab7b4766def688ea0c
              0x0000aad02222472cdf9892a3011c01caf6407f027081)]
      Storage size: 524 bytes
      Consumed gas: 24319
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
