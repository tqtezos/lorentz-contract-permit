
```haskell
import qualified Lorentz.Contracts.ManagedLedger.Permit.Set as S
import Text.Read
```

```haskell
S.printPermittableManagedLedgerSetContract (Just "contracts/permit_fa1.2_set.tz") False
```

```bash
❯❯❯ echo $BOB_ADDRESS
tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
```

```haskell
*Main S Text.Read> S.initPermittableManagedLedgerSetContract (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") True
Pair { } (Pair { } (Pair (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" False) (Pair 0 0)))
```

```bash
❯❯❯ tezos-client --wait none originate contract PermitFA12Set \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/permit_fa1.2_set.tz | tr -d '\n')" \
  --init 'Pair { } (Pair { } (Pair (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" False) (Pair 0 0)))' \
  --burn-cap 3.44

Waiting for the node to be bootstrapped before injection...
Current head: BLBgisKyVNhV (timestamp: 2020-08-04T17:58:04-00:00, validation: 2020-08-04T17:58:16-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 96378 units (will add 100 for safety)
Estimated storage: 3440 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'op8PrjXk6dsDjST3ahUnq26ZRfjtsvihCCBBYoQ9ortgRrQ2XU8'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for op8PrjXk6dsDjST3ahUnq26ZRfjtsvihCCBBYoQ9ortgRrQ2XU8 to be included --confirmations 30 --branch BLBgisKyVNhVeh5F2NS8H52u23K99TsauofFMPX8b3qcCAWMomD
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.013011
    Expected counter: 624009
    Gas limit: 96478
    Storage limit: 3460 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.013011
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,303) ... +ꜩ0.013011
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter
            (or (or (or (pair %transfer (address :from) (pair (address :to) (nat :value)))
                        (pair %getBalance (address :owner) (contract nat)))
                    (or (pair %getTotalSupply unit (contract nat)) (bool %setPause)))
                (or (or (address %setAdministrator) (pair %getAdministrator unit (contract address)))
                    (or (pair %mint (address :to) (nat :value)) (pair %burn (address :from) (nat :value))))) ;
          storage
            (pair (big_map address nat)
                  (pair (big_map address (set bytes)) (pair (pair address bool) (pair nat nat)))) ;
          code { DUP ;
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
                             PAIR } } } } }
        Initial storage:
          (Pair {}
                (Pair {} (Pair (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" False) (Pair 0 0))))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1ASfLiKHWpFvrb7DU8z4LZJGsgMXLWBK4G
        Storage size: 3183 bytes
        Updated big_maps:
          New map(12265) of type (big_map address (set bytes))
          New map(12264) of type (big_map address nat)
        Paid storage size diff: 3183 bytes
        Consumed gas: 96378
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ3.183
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1ASfLiKHWpFvrb7DU8z4LZJGsgMXLWBK4G originated.
Contract memorized as PermitFA12Set.
```

```bash
PERMIT_FA12="KT1ASfLiKHWpFvrb7DU8z4LZJGsgMXLWBK4G"
```

Mint `10` tokens to Bob:

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $PERMIT_FA12 \
  --entrypoint mint --arg "Pair \"$BOB_ADDRESS\" 10" \
  --burn-cap 0.067

Waiting for the node to be bootstrapped before injection...
Current head: BLVmC5VFFmJT (timestamp: 2020-08-04T18:56:34-00:00, validation: 2020-08-04T18:57:03-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 83483 units (will add 100 for safety)
Estimated storage: 67 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'op2T5hzyr3d8PiumnvZKH98UodPAPxoeaUsyZw86Gq387HwRoPt'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for op2T5hzyr3d8PiumnvZKH98UodPAPxoeaUsyZw86Gq387HwRoPt to be included --confirmations 30 --branch BLVmC5VFFmJTkBnG16Ph7bEsMJ2YNJXkrqHbLSQF1PVwV1ZqvzN
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.008665
    Expected counter: 624011
    Gas limit: 83583
    Storage limit: 87 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.008665
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,303) ... +ꜩ0.008665
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1ASfLiKHWpFvrb7DU8z4LZJGsgMXLWBK4G
      Entrypoint: mint
      Parameter: (Pair "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm" 10)
      This transaction was successfully applied
      Updated storage:
        (Pair 12264
              (Pair 12265
                    (Pair (Pair 0x0000aad02222472cdf9892a3011c01caf6407f027081 False) (Pair 20 0))))
      Updated big_maps:
        Set map(12264)[0x0000aad02222472cdf9892a3011c01caf6407f027081] to 10
      Storage size: 3317 bytes
      Paid storage size diff: 67 bytes
      Consumed gas: 83483
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

```bash
MISSIGNED_PERMIT_PARAM='Pair "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" (Pair "edsigtfkWys7vyeQy1PnHcBuac1dgj2aJ8Jv3fvoDE5XRtxTMRgJBwVgMTzvhAzBQyjH48ux9KE8jRZBSk4Rv2bfphsfpKP3ggM" 0x3865bc582f4140e94fafb65fab8025f0616dd3038def17d50123014b39121568)'
```

```bash
$ tezos-client --wait none transfer 0 from $FRED_ADDRESS to $PERMIT_FA12 \
  --entrypoint permit --arg $MISSIGNED_PERMIT_PARAM --dry-run



```








