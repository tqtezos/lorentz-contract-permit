
You can find an originated example [here](https://better-call.dev/carthagenet/KT1U3XFFSxVDbxHvdVLRWUQ3xPwz9wS5KZpS/operations).

```bash
❯❯❯ alpha-client --wait none originate contract PermitAdmin42Expiry \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/permit_admin_42_expiry.tz | tr -d '\n')" \
  --init 'Pair 300 (Pair (Pair { } (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")) "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")' \
  --burn-cap 2.096

Waiting for the node to be bootstrapped before injection...
Current head: BLscCGuNVXjT (timestamp: 2020-09-01T19:52:42-00:00, validation: 2020-09-01T19:52:57-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 66413 units (will add 100 for safety)
Estimated storage: 2096 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'onnh5zMKu5wXemadWcCwqHyPWoJcJHyQ94rYRKTsPwgv68w8wnh'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onnh5zMKu5wXemadWcCwqHyPWoJcJHyQ94rYRKTsPwgv68w8wnh to be included --confirmations 30 --branch BLscCGuNVXjTQLMaXVcomE7U8arBxFRvfBYDZN5JdEkLNzathF1
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.008717
    Expected counter: 624016
    Gas limit: 66513
    Storage limit: 2116 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.008717
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,336) ... +ꜩ0.008717
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter .. }
        Initial storage:
          (Pair 300
                (Pair (Pair {} (Pair 0 "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
                      "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1U3XFFSxVDbxHvdVLRWUQ3xPwz9wS5KZpS
        Storage size: 1839 bytes
        Updated big_maps:
          New map(16053) of type (big_map address (pair (option int) (map bytes (pair timestamp (option int)))))
        Paid storage size diff: 1839 bytes
        Consumed gas: 66413
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ1.839
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1U3XFFSxVDbxHvdVLRWUQ3xPwz9wS5KZpS originated.
Contract memorized as PermitAdmin42Expiry.
```

