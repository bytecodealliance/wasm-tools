During a merge, when `from` contributes a **new** interface that doesn't exist in `into`, that interface gets appended to the **end** of the arena. If an existing interface in `into` now depends on that newly-appended interface (because extra types/functions referencing it were also added), the ordering invariant is violated — the dependency comes *after* its dependent.

The `topo-sort-needed` test sets up exactly this scenario:

- **`from`** has two interfaces: `dep` (defines `type t = u32`) and `user` (has `use dep.{t}`, shared function `get`, and extra function `get-dep` that returns `t`).
- **`into`** has only `user` with just the `get` function — no `dep` at all.

When merging `from` into `into`:

1. `user` is matched between both sides (same name, same package). The shared function `get` checks out structurally.
2. `dep` has no counterpart in `into`, so it's **appended to the end** of the arena — after `user`.
3. The extra function `get-dep` and the `use dep.{t}` type from `from`'s `user` are added to `into`'s `user`, creating a dependency from `user` → `dep`.

Now the arena has `[user, dep]` but the dependency goes `user → dep` — `dep` should come first. Without the topological sort, `assert_topologically_sorted` panics with `assertion failed: other_interface_pos <= my_interface_pos`. With the sort, the arena is reordered to `[dep, user]` and the invariant holds.