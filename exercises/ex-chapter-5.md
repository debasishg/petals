## Exercise 5.2

Assume initial credits is the number of trees in the heap, say `t`. Now a call to `insert` takes `k + 1` steps and we have `k` `link`s. Since we had `t` trees before `insert`, after `insert` we have number of trees = `t - k + 1`, which is also the number of credits. Change in credit = `(t - k + 1) - t` = `1 - k`. Hence amortized cost = actual cost + change in credit = `(k + 1) + (1 - k)` = `2` = O(1)

## Exercise 5.3

**Show `merge` is O(log n) amortized:**

Assume for a binomial heap the initial potential = number of trees in the heap. Now, say we are merging 2 heaps of potentials `m` and `n`. Hence initial total potential = `m + n`.

Now during merge we can have at most `m + n` recursive calls to `merge` and let's assume we have `k` calls to `link`. Each of the `k` calls to `link` reduces the potential by `1` - hence at the end of merge the potential is `(m + n - k)`. So change of potential = `(m + n - k) - (m + n) = -k`. 

So amortized cost  
= actual cost + change in potential
= `(m + n + k)` (for `m + n` recursive calls to `merge` and `k` calls to `link`) `+ (-k)`
= `(m + n)`, which will be O(log n) since in a binomial heap of size n we can have log n trees.

**Show `deleteMin` is O(log n) amortized:**

Assume we have an initial heap with `n` trees, and hence initial potential = `n`. We find the minimum binary tree of rank (say) `r`, for which the actual cost will be at most `n`. This leaves us with a heap of potential `(n - 1)`.

For the minimum binary tree of rank `r` we do the following:
* reverse the `r` children 
* make a heap of potential `r`

We need to merge this new heap of potential `r` with the original one of potential `(n - 1)`. This takes `(n - 1 + r + k)` actual steps, where `k` is the number of `link`s. And the potential of this merged heap is `(n - 1 + r - k)`.

Hence change in potential = `(n - 1 + r - k) - n = (r - k - 1)`
And the amortized cost 
= actual cost + change in potential 
= `(n - 1 + r + k) + (r - k - 1)`
= `(n + 2r - 2)`, which is O(log n)
