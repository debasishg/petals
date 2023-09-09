## Exercise 9.8

Using redundant binary representation we have `Zero`, `One` and `Two`. Here `Zero` and `Two` are considered unsafe, while `One` is safe. Hence we allocate the debit invariant as `1` debit for `One` and `0` debits for `Zero` or `Two`

Assume we begin with `k` continuous `Two`s. `inc` changes the `k` `Two`s to `k` `One`s. Allocate a new debit for each of these steps and discharge `1` debit for the `One` generated just after the `Two`s if any. This restores the invariant.

Assume we begin with k continuous `Zero`s. `dec` changes the `k` `Zero`s to `k` `One`s. Allocate a new debit for each of these steps and discharge `1` debit for the `One` generated just after the `Zero`s if any. This restores the invariant.

Thus the debit invariant holds for `inc` and `dec` and they run in O(1) amortized.