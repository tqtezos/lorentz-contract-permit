
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

## Setting Up
### Requirements
#### Tezos-client
To set up the tezos-client, follow the instructions in the [Setup Tezos Client](https://assets.tqtezos.com/setup/1-tezos-client) tutorial

### Installing Dependencies
`stack build` Note that this will take some time. 

## Originating

### Print the `permitAdmin42Contract`

```haskell
printPermitAdmin42 (Just "contracts/permit_admin_42.tz") False
```

```haskell
import Tezos.Crypto.Orphans ()
printInitPermitAdmin42 (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")

Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
```


```bash
$ tezos-client --wait none originate contract PermitAdmin42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/permit_admin_42.tz | tr -d '\n')" \
  --init 'Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")' \
  --burn-cap 0.889

Waiting for the node to be bootstrapped before injection...
Current head: BLzGD3c8dzFT (timestamp: 2020-07-13T20:52:38-00:00, validation: 2020-07-13T20:52:48-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 26288 units (will add 100 for safety)
Estimated storage: 889 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opAD75SiuGtv2MPXwvBwGTcZRGfhYXGhaGug1RijAs63pxbVt5v'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opAD75SiuGtv2MPXwvBwGTcZRGfhYXGhaGug1RijAs63pxbVt5v to be included --confirmations 30 --branch BLzGD3c8dzFT3MRwSJzgAyQnf5Z3TiUwX7FyeunqdgPqdXg5BAk
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003483
    Expected counter: 623992
    Gas limit: 26388
    Storage limit: 909 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003483
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,275) ... +ꜩ0.003483
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { ... }
        Initial storage:
          (Pair {} (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1N6tDY9WKsWWvUT1gy6coZTLo96sYuDfcR
        Storage size: 632 bytes
        Updated big_maps:
          New map(9904) of type (big_map (pair bytes address) unit)
        Paid storage size diff: 632 bytes
        Consumed gas: 26288
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.632
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1N6tDY9WKsWWvUT1gy6coZTLo96sYuDfcR originated.
Contract memorized as PermitAdmin42.
```

```bash
PERMIT_ADMIN_42="KT1N6tDY9WKsWWvUT1gy6coZTLo96sYuDfcR"
```

```bash
$ tezos-client rpc get /chains/main/chain_id

"NetXjD3HPJJjmcd"
```

```haskell
A.printPermitAdmin42Bytes (Tezos.Core.ChainIdUnsafe "NetXjD3HPJJjmcd") (read "KT1N6tDY9WKsWWvUT1gy6coZTLo96sYuDfcR") 0

"0xacf4569de467cdea7ff9f4ce28a563505d54c95b5b4a88fbcb4d9cdb0be3a47b"
```

```bash
PERMIT_BYTES="0xacf4569de467cdea7ff9f4ce28a563505d54c95b5b4a88fbcb4d9cdb0be3a47b"
```

```bash
$ tezos-client sign bytes $PERMIT_BYTES for bob

Signature: edsigtbGUJ8hGaCcnJzVNhbzMxEjMreWj9hLQ787HzwC7gdHN81CjAxwA7F3s2r2gvQMdd1nKrP7EUZ5LSEmECeB5Rqg3uURwsE
```

```haskell
A.printPermitAdmin42Param (Tezos.Core.ChainIdUnsafe "NetXjD3HPJJjmcd") (read "KT1N6tDY9WKsWWvUT1gy6coZTLo96sYuDfcR") 0 (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigtbGUJ8hGaCcnJzVNhbzMxEjMreWj9hLQ787HzwC7gdHN81CjAxwA7F3s2r2gvQMdd1nKrP7EUZ5LSEmECeB5Rqg3uURwsE")

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtbGUJ8hGaCcnJzVNhbzMxEjMreWj9hLQ787HzwC7gdHN81CjAxwA7F3s2r2gvQMdd1nKrP7EUZ5LSEmECeB5Rqg3uURwsE" 0x05070707070a0000000f4e6574586a443348504a4a6a6d63640a0000001601944baa8dc22081f0bfd380ed34dbb6a93f83ecb60007070000002a)
```

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtbGUJ8hGaCcnJzVNhbzMxEjMreWj9hLQ787HzwC7gdHN81CjAxwA7F3s2r2gvQMdd1nKrP7EUZ5LSEmECeB5Rqg3uURwsE" 0x05070707070a0000000f4e6574586a443348504a4a6a6d63640a0000001601944baa8dc22081f0bfd380ed34dbb6a93f83ecb60007070000002a)'
```

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_ADMIN_42 \
  --entrypoint permit --arg $PERMIT_PARAM --burn-cap 0.000001

Waiting for the node to be bootstrapped before injection...
Current head: BLFpB4G1cxLe (timestamp: 2020-07-13T21:21:38-00:00, validation: 2020-07-13T21:22:05-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0
    Expected counter: 623965
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1N6tDY9WKsWWvUT1gy6coZTLo96sYuDfcR
      Entrypoint: permit
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigtbGUJ8hGaCcnJzVNhbzMxEjMreWj9hLQ787HzwC7gdHN81CjAxwA7F3s2r2gvQMdd1nKrP7EUZ5LSEmECeB5Rqg3uURwsE"
                             0x05070707070a0000000f4e6574586a443348504a4a6a6d63640a0000001601944baa8dc22081f0bfd380ed34dbb6a93f83ecb60007070000002a))
      This operation FAILED.

Runtime error in contract KT1N6tDY9WKsWWvUT1gy6coZTLo96sYuDfcR:
  01: { parameter (or (pair %permit key (pair signature bytes)) (nat %wrappedParam)) ;
  02:   storage (pair (big_map (pair bytes address) unit) (pair nat address)) ;
  03:   code { DUP ;
  04:          CAR ;
  05:          DIP { CDR } ;
  06:          IF_LEFT
  07:            { DUP ;
  08:              CAR ;
  09:              DIP { CDR } ;
  10:              DUP ;
  11:              DIP { DIP { DUP ;
  12:                          CAR ;
  13:                          DIP { CDR ;
  14:                                DUP ;
  15:                                DIG 2 ;
  16:                                DUP ;
  17:                                CAR ;
  18:                                DIP { CDR } ;
  19:                                SWAP ;
  20:                                DUP ;
  21:                                CAR ;
  22:                                DIP { CDR } ;
  23:                                DUP ;
  24:                                DUG 5 ;
  25:                                DIP { DIG 3 } ;
  26:                                PAIR ;
  27:                                SELF ;
  28:                                ADDRESS ;
  29:                                CHAIN_ID ;
  30:                                PAIR ;
  31:                                PAIR ;
  32:                                PACK } } ;
  33:                    CHECK_SIGNATURE ;
  34:                    IF {} { PUSH string "missigned" ; FAILWITH } } ;
  35:              HASH_KEY ;
  36:              IMPLICIT_ACCOUNT ;
  37:              ADDRESS ;
  38:              DIG 3 ;
  39:              PAIR ;
  40:              DIP { SWAP ; UNIT ; SOME } ;
  41:              UPDATE ;
  42:              DIP { SWAP ; PUSH nat 1 ; ADD ; PAIR } ;
  43:              PAIR ;
  44:              NIL operation ;
  45:              PAIR }
  46:            { LAMBDA
  47:                (pair (big_map (pair bytes address) unit) (pair bytes address))
  48:                (big_map (pair bytes address) unit)
  49:                { DUP ;
  50:                  CAR ;
  51:                  DIP { CDR } ;
  52:                  DIP { DUP ; CAR ; DIP { CDR } ; BLAKE2B ; PAIR } ;
  53:                  DUP ;
  54:                  DIG 2 ;
  55:                  DUP ;
  56:                  DIP { MEM ; IF {} { PUSH string "no permit" ; FAILWITH } ; NONE unit } ;
  57:                  UPDATE } ;
  58:              DIP { DUP ;
  59:                    PUSH nat 42 ;
  60:                    COMPARE ;
  61:                    EQ ;
  62:                    IF {} { PUSH string "not 42" ; FAILWITH } ;
  63:                    PACK ;
  64:                    DIP { DUP ; CAR ; DIP { CDR ; DUP ; CAR ; DIP { CDR } ; SWAP ; DUP } } ;
  65:                    SWAP ;
  66:                    DIP { PAIR } ;
  67:                    PAIR } ;
  68:              SWAP ;
  69:              EXEC ;
  70:              DIP { SWAP ; PAIR } ;
  71:              PAIR ;
  72:              NIL operation ;
  73:              PAIR } } }
At line 34 characters 53 to 61,
script reached FAILWITH instruction
with "missigned"
Fatal error:
  transfer simulation failed
```

