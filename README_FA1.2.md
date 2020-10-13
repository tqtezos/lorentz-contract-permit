
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

You can find the following example on carthagenet [here](https://better-call.dev/carthagenet/KT1LWCJk8NDgMjAJ8TXR6XrKjjMmc31nLHNR/operations).

## Setting Up
### Requirements
#### Tezos-client
To set up the tezos-client, follow the instructions in the [Setup Tezos Client](https://assets.tqtezos.com/setup/1-tezos-client) tutorial

### Installing Dependencies
`stack build` Note that this will take some time. 

## Originating

### Print the `permittableManagedLedgerSetContract`

Required Haskell imports:

```haskell
import qualified Lorentz.Contracts.ManagedLedger.Permit.Set as S
import Text.Read
```

(Re)generate the contract:

```haskell
S.printPermittableManagedLedgerSetContract (Just "contracts/permit_fa1.2_set.tz") False
```

### Generate the initial storage

`$BOB_ADDRESS` will be the admin address:

```bash
❯❯❯ echo $BOB_ADDRESS
tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
```

```haskell
*Main S Text.Read> S.initPermittableManagedLedgerSetContract (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") True
Pair { } (Pair { } (Pair (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" False) (Pair 0 0)))
```

### Originate the contract

```bash
❯❯❯ tezos-client --wait none originate contract PermitFA12Set \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/permit_fa1.2_set.tz | tr -d '\n')" \
  --init 'Pair { } (Pair { } (Pair (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" False) (Pair 0 0)))' \
  --burn-cap 3.933

Waiting for the node to be bootstrapped before injection...
Current head: BLb2pm558VxR (timestamp: 2020-08-04T20:27:44-00:00, validation: 2020-08-04T20:28:00-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 111206 units (will add 100 for safety)
Estimated storage: 3933 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oodGoanJtvPQMHKaRwSshDTJ3MDfn3SMzeTgGrt3Xzs8sss6rLH'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oodGoanJtvPQMHKaRwSshDTJ3MDfn3SMzeTgGrt3Xzs8sss6rLH to be included --confirmations 30 --branch BLb2pm558VxRcUvNAnn9SA81uwUmHMt2WkFimUZNJv2MnGYxsQb
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.014987
    Expected counter: 624013
    Gas limit: 111306
    Storage limit: 3953 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.014987
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,303) ... +ꜩ0.014987
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %permit key (pair signature bytes))
                (or (or (or (pair %transfer (address :from) (pair (address :to) (nat :value)))
                            (pair %getBalance (address :owner) (contract nat)))
                        (or (pair %getTotalSupply unit (contract nat)) (bool %setPause)))
                    (or (or (address %setAdministrator) (pair %getAdministrator unit (contract address)))
                        (or (pair %mint (address :to) (nat :value)) (pair %burn (address :from) (nat :value)))))) ;
          storage
            (pair (big_map address nat)
                  (pair (big_map address (set bytes)) (pair (pair address bool) (pair nat nat)))) ;
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
                                 CDR ;
                                 CDR ;
                                 CDR ;
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
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 PUSH nat 1 ;
                                 ADD ;
                                 DIP { DUP ; CDR ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR } ;
                           CDR ;
                           DUP ;
                           DIP { DIP { DUP } ;
                                 DIP { CDR ; CAR } ;
                                 GET ;
                                 IF_NONE { EMPTY_SET bytes } {} } ;
                           SWAP ;
                           PUSH bool True } ;
                     UPDATE ;
                     SOME ;
                     SWAP ;
                     DIP { DIP { DUP ; CDR ; CAR } } ;
                     UPDATE ;
                     DIP { DUP ; DIP { CAR } ; CDR } ;
                     DIP { DUP ; DIP { CDR } ; CAR } ;
                     SWAP ;
                     DROP ;
                     PAIR ;
                     SWAP ;
                     PAIR ;
                     NIL operation ;
                     PAIR }
                   { DIP { CDR } ;
                     PAIR ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     IF_LEFT
                       { IF_LEFT
                           { IF_LEFT
                               { DIP { DUP ;
                                       CDR ;
                                       CDR ;
                                       CAR ;
                                       CDR ;
                                       IF { PUSH string "tokenOperationsArePaused" ; FAILWITH } {} } ;
                                 DUP ;
                                 CAR ;
                                 SENDER ;
                                 COMPARE ;
                                 EQ ;
                                 IF {}
                                    { DUP ;
                                      CAR ;
                                      SWAP ;
                                      DUP ;
                                      DIP { PACK ;
                                            BLAKE2B ;
                                            PAIR ;
                                            PAIR ;
                                            DUP ;
                                            DUP ;
                                            CAR ;
                                            DIP { CDR } ;
                                            DUP ;
                                            CDR ;
                                            DIP { SWAP } ;
                                            DIP { CDR ; CAR } ;
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
                                                     DIP { DIP { DUP ; CDR ; CAR } } ;
                                                     UPDATE ;
                                                     DIP { DUP ; DIP { CAR } ; CDR } ;
                                                     DIP { DUP ; DIP { CDR } ; CAR } ;
                                                     SWAP ;
                                                     DROP ;
                                                     PAIR ;
                                                     SWAP ;
                                                     PAIR }
                                                   { FAILWITH } } } } ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 CAR ;
                                 DIP { CAR } ;
                                 GET ;
                                 IF_NONE
                                   { CDR ;
                                     CDR ;
                                     PUSH nat 0 ;
                                     SWAP ;
                                     PAIR ;
                                     PUSH string "NotEnoughBalance" ;
                                     PAIR ;
                                     FAILWITH }
                                   {} ;
                                 DUP ;
                                 DIP 2 { DUP } ;
                                 DIG 2 ;
                                 CDR ;
                                 CDR ;
                                 SWAP ;
                                 SUB ;
                                 ISNAT ;
                                 IF_NONE
                                   { DIP { DUP } ;
                                     SWAP ;
                                     CDR ;
                                     CDR ;
                                     PAIR ;
                                     PUSH string "NotEnoughBalance" ;
                                     PAIR ;
                                     FAILWITH }
                                   {} ;
                                 SWAP ;
                                 DROP ;
                                 DUP ;
                                 INT ;
                                 EQ ;
                                 IF { DROP ; NONE nat } { SOME } ;
                                 SWAP ;
                                 DUP ;
                                 DIP { CAR ;
                                       DIP { DIP { DUP ; CAR } } ;
                                       UPDATE ;
                                       DIP { DUP ; DIP { CDR } ; CAR } ;
                                       SWAP ;
                                       DROP ;
                                       PAIR } ;
                                 DUP ;
                                 DIP { CDR ;
                                       CDR ;
                                       NEG ;
                                       DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
                                       ADD ;
                                       ISNAT ;
                                       IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
                                       DIP { DUP ; CDR ; CDR } ;
                                       DIP { DUP ; DIP { CAR } ; CDR } ;
                                       DIP { DUP ; DIP { CDR } ; CAR } ;
                                       SWAP ;
                                       DROP ;
                                       PAIR ;
                                       SWAP ;
                                       PAIR ;
                                       DIP { DUP ; DIP { CAR } ; CDR } ;
                                       DIP { DUP ; DIP { CAR } ; CDR } ;
                                       SWAP ;
                                       DROP ;
                                       SWAP ;
                                       PAIR ;
                                       SWAP ;
                                       PAIR } ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 CDR ;
                                 CAR ;
                                 DIP { CAR } ;
                                 GET ;
                                 IF_NONE
                                   { DUP ;
                                     CDR ;
                                     CDR ;
                                     INT ;
                                     EQ ;
                                     IF { NONE nat } { DUP ; CDR ; CDR ; SOME } }
                                   { DIP { DUP } ; SWAP ; CDR ; CDR ; ADD ; SOME } ;
                                 SWAP ;
                                 DUP ;
                                 DIP { CDR ;
                                       CAR ;
                                       DIP { DIP { DUP ; CAR } } ;
                                       UPDATE ;
                                       DIP { DUP ; DIP { CDR } ; CAR } ;
                                       SWAP ;
                                       DROP ;
                                       PAIR } ;
                                 CDR ;
                                 CDR ;
                                 INT ;
                                 DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
                                 ADD ;
                                 ISNAT ;
                                 IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
                                 DIP { DUP ; CDR ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CDR } ; CAR } ;
                                 SWAP ;
                                 DROP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 NIL operation ;
                                 PAIR }
                               { DUP ;
                                 CAR ;
                                 DIP { CDR ; DIP { DUP } ; SWAP } ;
                                 DIP { CAR } ;
                                 GET ;
                                 IF_NONE { PUSH nat 0 } {} ;
                                 DIP { AMOUNT } ;
                                 TRANSFER_TOKENS ;
                                 NIL operation ;
                                 SWAP ;
                                 CONS ;
                                 PAIR } }
                           { IF_LEFT
                               { DUP ;
                                 CAR ;
                                 DIP { CDR ; DIP { DUP } ; SWAP } ;
                                 PAIR ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 DIP { AMOUNT } ;
                                 TRANSFER_TOKENS ;
                                 NIL operation ;
                                 SWAP ;
                                 CONS ;
                                 PAIR }
                               { DIP { DUP ;
                                       CDR ;
                                       CDR ;
                                       CAR ;
                                       CAR ;
                                       SENDER ;
                                       COMPARE ;
                                       EQ ;
                                       IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
                                 DIP { DUP ; CDR ; CDR } ;
                                 DIP { DUP ; DIP { CDR } ; CAR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 PAIR ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 NIL operation ;
                                 PAIR } } }
                       { IF_LEFT
                           { IF_LEFT
                               { DIP { DUP ;
                                       CDR ;
                                       CDR ;
                                       CAR ;
                                       CAR ;
                                       SENDER ;
                                       COMPARE ;
                                       EQ ;
                                       IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
                                 DIP { DUP ; CDR ; CDR } ;
                                 DIP { DUP ; DIP { CDR } ; CAR } ;
                                 DIP { DUP ; DIP { CDR } ; CAR } ;
                                 SWAP ;
                                 DROP ;
                                 PAIR ;
                                 PAIR ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 NIL operation ;
                                 PAIR }
                               { DUP ;
                                 CAR ;
                                 DIP { CDR ; DIP { DUP } ; SWAP } ;
                                 PAIR ;
                                 CDR ;
                                 CDR ;
                                 CDR ;
                                 CAR ;
                                 CAR ;
                                 DIP { AMOUNT } ;
                                 TRANSFER_TOKENS ;
                                 NIL operation ;
                                 SWAP ;
                                 CONS ;
                                 PAIR } }
                           { IF_LEFT
                               { DIP { DUP ;
                                       CDR ;
                                       CDR ;
                                       CAR ;
                                       CAR ;
                                       SENDER ;
                                       COMPARE ;
                                       EQ ;
                                       IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 CAR ;
                                 DIP { CAR } ;
                                 GET ;
                                 IF_NONE
                                   { DUP ; CDR ; INT ; EQ ; IF { NONE nat } { DUP ; CDR ; SOME } }
                                   { DIP { DUP } ; SWAP ; CDR ; ADD ; SOME } ;
                                 SWAP ;
                                 DUP ;
                                 DIP { CAR ;
                                       DIP { DIP { DUP ; CAR } } ;
                                       UPDATE ;
                                       DIP { DUP ; DIP { CDR } ; CAR } ;
                                       SWAP ;
                                       DROP ;
                                       PAIR } ;
                                 CDR ;
                                 INT ;
                                 DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
                                 ADD ;
                                 ISNAT ;
                                 IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
                                 DIP { DUP ; CDR ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CDR } ; CAR } ;
                                 SWAP ;
                                 DROP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 NIL operation ;
                                 PAIR }
                               { DIP { DUP ;
                                       CDR ;
                                       CDR ;
                                       CAR ;
                                       CAR ;
                                       SENDER ;
                                       COMPARE ;
                                       EQ ;
                                       IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 DIP { DUP } ;
                                 SWAP ;
                                 CAR ;
                                 DIP { CAR } ;
                                 GET ;
                                 IF_NONE
                                   { CDR ;
                                     PUSH nat 0 ;
                                     SWAP ;
                                     PAIR ;
                                     PUSH string "NotEnoughBalance" ;
                                     PAIR ;
                                     FAILWITH }
                                   {} ;
                                 DUP ;
                                 DIP 2 { DUP } ;
                                 DIG 2 ;
                                 CDR ;
                                 SWAP ;
                                 SUB ;
                                 ISNAT ;
                                 IF_NONE
                                   { DIP { DUP } ;
                                     SWAP ;
                                     CDR ;
                                     PAIR ;
                                     PUSH string "NotEnoughBalance" ;
                                     PAIR ;
                                     FAILWITH }
                                   {} ;
                                 SWAP ;
                                 DROP ;
                                 DUP ;
                                 INT ;
                                 EQ ;
                                 IF { DROP ; NONE nat } { SOME } ;
                                 SWAP ;
                                 DUP ;
                                 DIP { CAR ;
                                       DIP { DIP { DUP ; CAR } } ;
                                       UPDATE ;
                                       DIP { DUP ; DIP { CDR } ; CAR } ;
                                       SWAP ;
                                       DROP ;
                                       PAIR } ;
                                 CDR ;
                                 NEG ;
                                 DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
                                 ADD ;
                                 ISNAT ;
                                 IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
                                 DIP { DUP ; CDR ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CDR } ; CAR } ;
                                 SWAP ;
                                 DROP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 DIP { DUP ; DIP { CAR } ; CDR } ;
                                 SWAP ;
                                 DROP ;
                                 SWAP ;
                                 PAIR ;
                                 SWAP ;
                                 PAIR ;
                                 NIL operation ;
                                 PAIR } } } } } }
        Initial storage:
          (Pair {}
                (Pair {} (Pair (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" False) (Pair 0 0))))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7
        Storage size: 3676 bytes
        Updated big_maps:
          New map(12269) of type (big_map address (set bytes))
          New map(12268) of type (big_map address nat)
        Paid storage size diff: 3676 bytes
        Consumed gas: 111206
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ3.676
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7 originated.
Contract memorized as PermitFA12Set.
```

Make a bash variable for the contract:

```bash
PERMIT_FA12="KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7"
```

Mint `10` tokens to Bob:

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $PERMIT_FA12 \
  --entrypoint mint --arg "Pair \"$BOB_ADDRESS\" 10" \
  --burn-cap 0.067

Waiting for the node to be bootstrapped before injection...
Current head: BKyZ3bwCLzUf (timestamp: 2020-08-04T20:29:14-00:00, validation: 2020-08-04T20:29:42-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 95851 units (will add 100 for safety)
Estimated storage: 67 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'onoq1mhNZ8mXWNvryJe6gaa2bWpwykrthWfr1mxEz1XPUfSs7yS'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onoq1mhNZ8mXWNvryJe6gaa2bWpwykrthWfr1mxEz1XPUfSs7yS to be included --confirmations 30 --branch BMC7eGB3XsxzVkW5rv2W2D2rxC4fDzmzBquNxihCUtPA9AEETpx
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.009902
    Expected counter: 624014
    Gas limit: 95951
    Storage limit: 87 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.009902
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,303) ... +ꜩ0.009902
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7
      Entrypoint: mint
      Parameter: (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" 10)
      This transaction was successfully applied
      Updated storage:
        (Pair 12268
              (Pair 12269
                    (Pair (Pair 0x0000aad02222472cdf9892a3011c01caf6407f027081 False) (Pair 10 0))))
      Updated big_maps:
        Set map(12268)[0x0000aad02222472cdf9892a3011c01caf6407f027081] to 10
      Storage size: 3743 bytes
      Paid storage size diff: 67 bytes
      Consumed gas: 95851
      Balance updates:
        tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.067
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

- `from` is `$BOB_ADDRESS`
- `to` is `$ALICE_ADDRESS`
- `value` is `5`

```haskell
> S.printPermittableManagedLedgerSetParam (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM") (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (read "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr") 5

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM" 0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568)
```

Make a bash variable for it:

```bash
MISSIGNED_PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM" 0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568)'
```

Use a `dry-run` to find the expected bytes for the parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_FA12 \
  --entrypoint permit --arg $MISSIGNED_PERMIT_PARAM --dry-run

Waiting for the node to be bootstrapped before injection...
Current head: BMBNC1kNHbEV (timestamp: 2020-08-04T20:30:14-00:00, validation: 2020-08-04T20:30:36-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0
    Expected counter: 623979
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7
      Entrypoint: permit
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM"
                             0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568))
      This operation FAILED.

Runtime error in contract KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7:
  001: { parameter
  002:     (or (pair %permit key (pair signature bytes))
  003:         (or (or (or (pair %transfer (address :from) (pair (address :to) (nat :value)))
  004:                     (pair %getBalance (address :owner) (contract nat)))
  005:                 (or (pair %getTotalSupply unit (contract nat)) (bool %setPause)))
  006:             (or (or (address %setAdministrator) (pair %getAdministrator unit (contract address)))
  007:                 (or (pair %mint (address :to) (nat :value)) (pair %burn (address :from) (nat :value)))))) ;
  008:   storage
  009:     (pair (big_map address nat)
  010:           (pair (big_map address (set bytes)) (pair (pair address bool) (pair nat nat)))) ;
  011:   code { DUP ;
  012:          CAR ;
  013:          IF_LEFT
  014:            { DUP ;
  015:              CAR ;
  016:              DUP ;
  017:              DIP { SWAP ;
  018:                    CDR ;
  019:                    DUP ;
  020:                    CAR ;
  021:                    DIP { CDR ;
  022:                          DUP ;
  023:                          DIP { DIP { HASH_KEY ; IMPLICIT_ACCOUNT ; ADDRESS } ; PAIR ; SWAP } ;
  024:                          SWAP ;
  025:                          CDR ;
  026:                          DUP ;
  027:                          CDR ;
  028:                          CDR ;
  029:                          CDR ;
  030:                          CDR ;
  031:                          DIP { SWAP } ;
  032:                          PAIR ;
  033:                          SELF ;
  034:                          ADDRESS ;
  035:                          CHAIN_ID ;
  036:                          PAIR ;
  037:                          PAIR ;
  038:                          PACK } } ;
  039:              DIP { DIP { DUP } } ;
  040:              CHECK_SIGNATURE ;
  041:              IF { DROP } { PUSH string "missigned" ; PAIR ; FAILWITH } ;
  042:              SWAP ;
  043:              DUP ;
  044:              CAR ;
  045:              DIP { DIP { DUP ;
  046:                          CDR ;
  047:                          CDR ;
  048:                          CDR ;
  049:                          CDR ;
  050:                          PUSH nat 1 ;
  051:                          ADD ;
  052:                          DIP { DUP ; CDR ; CDR } ;
  053:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  054:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  055:                          SWAP ;
  056:                          DROP ;
  057:                          SWAP ;
  058:                          PAIR ;
  059:                          SWAP ;
  060:                          PAIR ;
  061:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  062:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  063:                          SWAP ;
  064:                          DROP ;
  065:                          SWAP ;
  066:                          PAIR ;
  067:                          SWAP ;
  068:                          PAIR } ;
  069:                    CDR ;
  070:                    DUP ;
  071:                    DIP { DIP { DUP } ;
  072:                          DIP { CDR ; CAR } ;
  073:                          GET ;
  074:                          IF_NONE { EMPTY_SET bytes } {} } ;
  075:                    SWAP ;
  076:                    PUSH bool True } ;
  077:              UPDATE ;
  078:              SOME ;
  079:              SWAP ;
  080:              DIP { DIP { DUP ; CDR ; CAR } } ;
  081:              UPDATE ;
  082:              DIP { DUP ; DIP { CAR } ; CDR } ;
  083:              DIP { DUP ; DIP { CDR } ; CAR } ;
  084:              SWAP ;
  085:              DROP ;
  086:              PAIR ;
  087:              SWAP ;
  088:              PAIR ;
  089:              NIL operation ;
  090:              PAIR }
  091:            { DIP { CDR } ;
  092:              PAIR ;
  093:              DUP ;
  094:              CAR ;
  095:              DIP { CDR } ;
  096:              IF_LEFT
  097:                { IF_LEFT
  098:                    { IF_LEFT
  099:                        { DIP { DUP ;
  100:                                CDR ;
  101:                                CDR ;
  102:                                CAR ;
  103:                                CDR ;
  104:                                IF { PUSH string "tokenOperationsArePaused" ; FAILWITH } {} } ;
  105:                          DUP ;
  106:                          CAR ;
  107:                          SENDER ;
  108:                          COMPARE ;
  109:                          EQ ;
  110:                          IF {}
  111:                             { DUP ;
  112:                               CAR ;
  113:                               SWAP ;
  114:                               DUP ;
  115:                               DIP { PACK ;
  116:                                     BLAKE2B ;
  117:                                     PAIR ;
  118:                                     PAIR ;
  119:                                     DUP ;
  120:                                     DUP ;
  121:                                     CAR ;
  122:                                     DIP { CDR } ;
  123:                                     DUP ;
  124:                                     CDR ;
  125:                                     DIP { SWAP } ;
  126:                                     DIP { CDR ; CAR } ;
  127:                                     GET ;
  128:                                     IF_NONE
  129:                                       { FAILWITH }
  130:                                       { SWAP ;
  131:                                         DIP { DUP } ;
  132:                                         CAR ;
  133:                                         MEM ;
  134:                                         IF { DIP { DUP ; CAR ; DIP { CDR } ; DUP ; CAR } ;
  135:                                              SWAP ;
  136:                                              DIP { PUSH bool False } ;
  137:                                              UPDATE ;
  138:                                              SOME ;
  139:                                              SWAP ;
  140:                                              CDR ;
  141:                                              DIP { DIP { DUP ; CDR ; CAR } } ;
  142:                                              UPDATE ;
  143:                                              DIP { DUP ; DIP { CAR } ; CDR } ;
  144:                                              DIP { DUP ; DIP { CDR } ; CAR } ;
  145:                                              SWAP ;
  146:                                              DROP ;
  147:                                              PAIR ;
  148:                                              SWAP ;
  149:                                              PAIR }
  150:                                            { FAILWITH } } } } ;
  151:                          DIP { DUP } ;
  152:                          SWAP ;
  153:                          DIP { DUP } ;
  154:                          SWAP ;
  155:                          CAR ;
  156:                          DIP { CAR } ;
  157:                          GET ;
  158:                          IF_NONE
  159:                            { CDR ;
  160:                              CDR ;
  161:                              PUSH nat 0 ;
  162:                              SWAP ;
  163:                              PAIR ;
  164:                              PUSH string "NotEnoughBalance" ;
  165:                              PAIR ;
  166:                              FAILWITH }
  167:                            {} ;
  168:                          DUP ;
  169:                          DIP 2 { DUP } ;
  170:                          DIG 2 ;
  171:                          CDR ;
  172:                          CDR ;
  173:                          SWAP ;
  174:                          SUB ;
  175:                          ISNAT ;
  176:                          IF_NONE
  177:                            { DIP { DUP } ;
  178:                              SWAP ;
  179:                              CDR ;
  180:                              CDR ;
  181:                              PAIR ;
  182:                              PUSH string "NotEnoughBalance" ;
  183:                              PAIR ;
  184:                              FAILWITH }
  185:                            {} ;
  186:                          SWAP ;
  187:                          DROP ;
  188:                          DUP ;
  189:                          INT ;
  190:                          EQ ;
  191:                          IF { DROP ; NONE nat } { SOME } ;
  192:                          SWAP ;
  193:                          DUP ;
  194:                          DIP { CAR ;
  195:                                DIP { DIP { DUP ; CAR } } ;
  196:                                UPDATE ;
  197:                                DIP { DUP ; DIP { CDR } ; CAR } ;
  198:                                SWAP ;
  199:                                DROP ;
  200:                                PAIR } ;
  201:                          DUP ;
  202:                          DIP { CDR ;
  203:                                CDR ;
  204:                                NEG ;
  205:                                DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
  206:                                ADD ;
  207:                                ISNAT ;
  208:                                IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
  209:                                DIP { DUP ; CDR ; CDR } ;
  210:                                DIP { DUP ; DIP { CAR } ; CDR } ;
  211:                                DIP { DUP ; DIP { CDR } ; CAR } ;
  212:                                SWAP ;
  213:                                DROP ;
  214:                                PAIR ;
  215:                                SWAP ;
  216:                                PAIR ;
  217:                                DIP { DUP ; DIP { CAR } ; CDR } ;
  218:                                DIP { DUP ; DIP { CAR } ; CDR } ;
  219:                                SWAP ;
  220:                                DROP ;
  221:                                SWAP ;
  222:                                PAIR ;
  223:                                SWAP ;
  224:                                PAIR } ;
  225:                          DIP { DUP } ;
  226:                          SWAP ;
  227:                          DIP { DUP } ;
  228:                          SWAP ;
  229:                          CDR ;
  230:                          CAR ;
  231:                          DIP { CAR } ;
  232:                          GET ;
  233:                          IF_NONE
  234:                            { DUP ;
  235:                              CDR ;
  236:                              CDR ;
  237:                              INT ;
  238:                              EQ ;
  239:                              IF { NONE nat } { DUP ; CDR ; CDR ; SOME } }
  240:                            { DIP { DUP } ; SWAP ; CDR ; CDR ; ADD ; SOME } ;
  241:                          SWAP ;
  242:                          DUP ;
  243:                          DIP { CDR ;
  244:                                CAR ;
  245:                                DIP { DIP { DUP ; CAR } } ;
  246:                                UPDATE ;
  247:                                DIP { DUP ; DIP { CDR } ; CAR } ;
  248:                                SWAP ;
  249:                                DROP ;
  250:                                PAIR } ;
  251:                          CDR ;
  252:                          CDR ;
  253:                          INT ;
  254:                          DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
  255:                          ADD ;
  256:                          ISNAT ;
  257:                          IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
  258:                          DIP { DUP ; CDR ; CDR } ;
  259:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  260:                          DIP { DUP ; DIP { CDR } ; CAR } ;
  261:                          SWAP ;
  262:                          DROP ;
  263:                          PAIR ;
  264:                          SWAP ;
  265:                          PAIR ;
  266:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  267:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  268:                          SWAP ;
  269:                          DROP ;
  270:                          SWAP ;
  271:                          PAIR ;
  272:                          SWAP ;
  273:                          PAIR ;
  274:                          NIL operation ;
  275:                          PAIR }
  276:                        { DUP ;
  277:                          CAR ;
  278:                          DIP { CDR ; DIP { DUP } ; SWAP } ;
  279:                          DIP { CAR } ;
  280:                          GET ;
  281:                          IF_NONE { PUSH nat 0 } {} ;
  282:                          DIP { AMOUNT } ;
  283:                          TRANSFER_TOKENS ;
  284:                          NIL operation ;
  285:                          SWAP ;
  286:                          CONS ;
  287:                          PAIR } }
  288:                    { IF_LEFT
  289:                        { DUP ;
  290:                          CAR ;
  291:                          DIP { CDR ; DIP { DUP } ; SWAP } ;
  292:                          PAIR ;
  293:                          CDR ;
  294:                          CDR ;
  295:                          CDR ;
  296:                          CDR ;
  297:                          CAR ;
  298:                          DIP { AMOUNT } ;
  299:                          TRANSFER_TOKENS ;
  300:                          NIL operation ;
  301:                          SWAP ;
  302:                          CONS ;
  303:                          PAIR }
  304:                        { DIP { DUP ;
  305:                                CDR ;
  306:                                CDR ;
  307:                                CAR ;
  308:                                CAR ;
  309:                                SENDER ;
  310:                                COMPARE ;
  311:                                EQ ;
  312:                                IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
  313:                          DIP { DUP ; CDR ; CDR } ;
  314:                          DIP { DUP ; DIP { CDR } ; CAR } ;
  315:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  316:                          SWAP ;
  317:                          DROP ;
  318:                          SWAP ;
  319:                          PAIR ;
  320:                          PAIR ;
  321:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  322:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  323:                          SWAP ;
  324:                          DROP ;
  325:                          SWAP ;
  326:                          PAIR ;
  327:                          SWAP ;
  328:                          PAIR ;
  329:                          NIL operation ;
  330:                          PAIR } } }
  331:                { IF_LEFT
  332:                    { IF_LEFT
  333:                        { DIP { DUP ;
  334:                                CDR ;
  335:                                CDR ;
  336:                                CAR ;
  337:                                CAR ;
  338:                                SENDER ;
  339:                                COMPARE ;
  340:                                EQ ;
  341:                                IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
  342:                          DIP { DUP ; CDR ; CDR } ;
  343:                          DIP { DUP ; DIP { CDR } ; CAR } ;
  344:                          DIP { DUP ; DIP { CDR } ; CAR } ;
  345:                          SWAP ;
  346:                          DROP ;
  347:                          PAIR ;
  348:                          PAIR ;
  349:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  350:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  351:                          SWAP ;
  352:                          DROP ;
  353:                          SWAP ;
  354:                          PAIR ;
  355:                          SWAP ;
  356:                          PAIR ;
  357:                          NIL operation ;
  358:                          PAIR }
  359:                        { DUP ;
  360:                          CAR ;
  361:                          DIP { CDR ; DIP { DUP } ; SWAP } ;
  362:                          PAIR ;
  363:                          CDR ;
  364:                          CDR ;
  365:                          CDR ;
  366:                          CAR ;
  367:                          CAR ;
  368:                          DIP { AMOUNT } ;
  369:                          TRANSFER_TOKENS ;
  370:                          NIL operation ;
  371:                          SWAP ;
  372:                          CONS ;
  373:                          PAIR } }
  374:                    { IF_LEFT
  375:                        { DIP { DUP ;
  376:                                CDR ;
  377:                                CDR ;
  378:                                CAR ;
  379:                                CAR ;
  380:                                SENDER ;
  381:                                COMPARE ;
  382:                                EQ ;
  383:                                IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
  384:                          DIP { DUP } ;
  385:                          SWAP ;
  386:                          DIP { DUP } ;
  387:                          SWAP ;
  388:                          CAR ;
  389:                          DIP { CAR } ;
  390:                          GET ;
  391:                          IF_NONE
  392:                            { DUP ; CDR ; INT ; EQ ; IF { NONE nat } { DUP ; CDR ; SOME } }
  393:                            { DIP { DUP } ; SWAP ; CDR ; ADD ; SOME } ;
  394:                          SWAP ;
  395:                          DUP ;
  396:                          DIP { CAR ;
  397:                                DIP { DIP { DUP ; CAR } } ;
  398:                                UPDATE ;
  399:                                DIP { DUP ; DIP { CDR } ; CAR } ;
  400:                                SWAP ;
  401:                                DROP ;
  402:                                PAIR } ;
  403:                          CDR ;
  404:                          INT ;
  405:                          DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
  406:                          ADD ;
  407:                          ISNAT ;
  408:                          IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
  409:                          DIP { DUP ; CDR ; CDR } ;
  410:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  411:                          DIP { DUP ; DIP { CDR } ; CAR } ;
  412:                          SWAP ;
  413:                          DROP ;
  414:                          PAIR ;
  415:                          SWAP ;
  416:                          PAIR ;
  417:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  418:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  419:                          SWAP ;
  420:                          DROP ;
  421:                          SWAP ;
  422:                          PAIR ;
  423:                          SWAP ;
  424:                          PAIR ;
  425:                          NIL operation ;
  426:                          PAIR }
  427:                        { DIP { DUP ;
  428:                                CDR ;
  429:                                CDR ;
  430:                                CAR ;
  431:                                CAR ;
  432:                                SENDER ;
  433:                                COMPARE ;
  434:                                EQ ;
  435:                                IF {} { UNIT ; PUSH string "SenderIsNotAdmin" ; PAIR ; FAILWITH } } ;
  436:                          DIP { DUP } ;
  437:                          SWAP ;
  438:                          DIP { DUP } ;
  439:                          SWAP ;
  440:                          CAR ;
  441:                          DIP { CAR } ;
  442:                          GET ;
  443:                          IF_NONE
  444:                            { CDR ;
  445:                              PUSH nat 0 ;
  446:                              SWAP ;
  447:                              PAIR ;
  448:                              PUSH string "NotEnoughBalance" ;
  449:                              PAIR ;
  450:                              FAILWITH }
  451:                            {} ;
  452:                          DUP ;
  453:                          DIP 2 { DUP } ;
  454:                          DIG 2 ;
  455:                          CDR ;
  456:                          SWAP ;
  457:                          SUB ;
  458:                          ISNAT ;
  459:                          IF_NONE
  460:                            { DIP { DUP } ;
  461:                              SWAP ;
  462:                              CDR ;
  463:                              PAIR ;
  464:                              PUSH string "NotEnoughBalance" ;
  465:                              PAIR ;
  466:                              FAILWITH }
  467:                            {} ;
  468:                          SWAP ;
  469:                          DROP ;
  470:                          DUP ;
  471:                          INT ;
  472:                          EQ ;
  473:                          IF { DROP ; NONE nat } { SOME } ;
  474:                          SWAP ;
  475:                          DUP ;
  476:                          DIP { CAR ;
  477:                                DIP { DIP { DUP ; CAR } } ;
  478:                                UPDATE ;
  479:                                DIP { DUP ; DIP { CDR } ; CAR } ;
  480:                                SWAP ;
  481:                                DROP ;
  482:                                PAIR } ;
  483:                          CDR ;
  484:                          NEG ;
  485:                          DIP { DUP ; CDR ; CDR ; CDR ; CAR } ;
  486:                          ADD ;
  487:                          ISNAT ;
  488:                          IF_NONE { PUSH string "Unexpected: Negative total supply" ; FAILWITH } {} ;
  489:                          DIP { DUP ; CDR ; CDR } ;
  490:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  491:                          DIP { DUP ; DIP { CDR } ; CAR } ;
  492:                          SWAP ;
  493:                          DROP ;
  494:                          PAIR ;
  495:                          SWAP ;
  496:                          PAIR ;
  497:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  498:                          DIP { DUP ; DIP { CAR } ; CDR } ;
  499:                          SWAP ;
  500:                          DROP ;
  501:                          SWAP ;
  502:                          PAIR ;
  503:                          SWAP ;
  504:                          PAIR ;
  505:                          NIL operation ;
  506:                          PAIR } } } } } }
At line 41 characters 60 to 68,
script reached FAILWITH instruction
with
  (Pair "missigned"
        0x05070707070a000000049caecab90a00000016014230a3b5b9223b2320a565e3e8740116e687804500070700000a000000203865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568)
Fatal error:
  transfer simulation failed
```

Store the `"missigned"` bytes from the error in a bash variable:

```bash
PERMIT_BYTES="0x05070707070a000000049caecab90a00000016014230a3b5b9223b2320a565e3e8740116e687804500070700000a000000203865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568"
```

Sign the bytes:

```bash
$ tezos-client sign bytes $PERMIT_BYTES for bob

Signature: edsigu1hcpLmXujkFowr7dVF8fNUMJU3KjgkhmTvVEQdhBaWLKgLSPE1So5LjxXFYbpSEbsgTrRHqqcY7zzznuUpdnghHukW1jL
```

Generate the parameter:

```haskell
> S.printPermittableManagedLedgerSetParam (read "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb") (read "edsigu1hcpLmXujkFowr7dVF8fNUMJU3KjgkhmTvVEQdhBaWLKgLSPE1So5LjxXFYbpSEbsgTrRHqqcY7zzznuUpdnghHukW1jL") (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (read "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr") 5

Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu1hcpLmXujkFowr7dVF8fNUMJU3KjgkhmTvVEQdhBaWLKgLSPE1So5LjxXFYbpSEbsgTrRHqqcY7zzznuUpdnghHukW1jL" 0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568)
```

Store it in a bash variabe:

```bash
PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigu1hcpLmXujkFowr7dVF8fNUMJU3KjgkhmTvVEQdhBaWLKgLSPE1So5LjxXFYbpSEbsgTrRHqqcY7zzznuUpdnghHukW1jL" 0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568)'
```

Anyone can submit the signed, hashed parameter:

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_FA12 \
  --entrypoint permit --arg $PERMIT_PARAM --burn-cap 0.107

Waiting for the node to be bootstrapped before injection...
Current head: BLd13oGnio56 (timestamp: 2020-08-04T20:34:14-00:00, validation: 2020-08-04T20:34:33-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 97465 units (will add 100 for safety)
Estimated storage: 107 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opLMN9nseZnfApeSmvWLCK9VtMfo9cpoa62DRkUuQdyuNuH7uee'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opLMN9nseZnfApeSmvWLCK9VtMfo9cpoa62DRkUuQdyuNuH7uee to be included --confirmations 30 --branch BLd13oGnio56qRBhxuqKzKBGPNJTTrN1EV9K2xyDunMdTNh9q3K
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.010224
    Expected counter: 623979
    Gas limit: 97565
    Storage limit: 127 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.010224
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,303) ... +ꜩ0.010224
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7
      Entrypoint: permit
      Parameter: (Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb"
                       (Pair "edsigu1hcpLmXujkFowr7dVF8fNUMJU3KjgkhmTvVEQdhBaWLKgLSPE1So5LjxXFYbpSEbsgTrRHqqcY7zzznuUpdnghHukW1jL"
                             0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568))
      This transaction was successfully applied
      Updated storage:
        (Pair 12268
              (Pair 12269
                    (Pair (Pair 0x0000aad02222472cdf9892a3011c01caf6407f027081 False) (Pair 10 1))))
      Updated big_maps:
        Set map(12269)[0x0000aad02222472cdf9892a3011c01caf6407f027081] to { 0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568 }
      Storage size: 3850 bytes
      Paid storage size diff: 107 bytes
      Consumed gas: 97465
      Balance updates:
        tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ0.107
```

Then, anyone can submit the transfer in place of Bob:

- `from` is `$BOB_ADDRESS`
- `to` is `$ALICE_ADDRESS`
- `value` is `5`

The Michelson parameter for the transfer is:
`Pair \"$BOB_ADDRESS\" (Pair \"$ALICE_ADDRESS\" 5)`

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_FA12 \
  --entrypoint transfer \
  --arg "Pair \"$BOB_ADDRESS\" (Pair \"$ALICE_ADDRESS\" 5)" \
  --burn-cap 0.03

Waiting for the node to be bootstrapped before injection...
Current head: BLvBtXL1oMtR (timestamp: 2020-08-04T20:37:14-00:00, validation: 2020-08-04T20:37:17-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 98955 units (will add 100 for safety)
Estimated storage: 30 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooaZuDaCR7LjSoUwJBAJYJmYGPL3y19R1xfuhzPUMtYhWHPugPa'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooaZuDaCR7LjSoUwJBAJYJmYGPL3y19R1xfuhzPUMtYhWHPugPa to be included --confirmations 30 --branch BLvBtXL1oMtRTppEDicDBH3qb1YCkSAShhehXiFcNJCkhYp3ip2
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.010259
    Expected counter: 623980
    Gas limit: 99055
    Storage limit: 50 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.010259
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,303) ... +ꜩ0.010259
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1EckRxUjwDhk12WT8soLs3XwkkFbxqyWZ7
      Entrypoint: transfer
      Parameter: (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"
                       (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" 5))
      This transaction was successfully applied
      Updated storage:
        (Pair 12268
              (Pair 12269
                    (Pair (Pair 0x0000aad02222472cdf9892a3011c01caf6407f027081 False) (Pair 10 1))))
      Updated big_maps:
        Set map(12269)[0x0000aad02222472cdf9892a3011c01caf6407f027081] to {}
        Set map(12268)[0x00003b5d4596c032347b72fb51f688c45200d0cb50db] to 5
        Set map(12268)[0x0000aad02222472cdf9892a3011c01caf6407f027081] to 5
      Storage size: 3880 bytes
      Paid storage size diff: 30 bytes
      Consumed gas: 98955
      Balance updates:
        tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ0.03
```


