---
layout: post
---

When using ExUnit for testing in elixir, you can use the `:skip` tag to
identify tests which do not need to be run.

``` elixir
@tag :skip
test "the truth" do
    assert 1 + 2 == 3
end
```

This test will not be run when using `mix test` and will show up as skipped in
the final statisics.

```
...

Finished in 1.8 seconds
3 tests, 2 failures, 1 skipped

Randomized with seed 936117

---
```

This was only [introduced](https://github.com/elixir-lang/elixir/commit/65f81054aa53b31e16ffb439dd6dfbf67265708d)
in late 2015, so a lot of blog posts recommend using a custom tag and
configuring the run-time accordingly. But now it is built in so you get this
for free.
