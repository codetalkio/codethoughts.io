+++
title = "Briefly on the purpose of Functors, Applicatives and Monads"
date = 2015-11-28
[taxonomies]
tags = ["haskell"]
+++

In a recent thread on [/r/haskell](https://www.reddit.com/r/haskell/) about how
to [motivate the AMP proposal in teaching](https://www.reddit.com/r/haskell/comments/3tpom7/amp_how_do_you_motivate_this_in_teaching/), I read a comment that finally helped me understand the purpose of `Functor`s, `Applicative`s and `Monad`s.

<div></div><!-- more -->

{{ toc() }}

## Wait, what is AMP?

For the reader that hasn't followed this debacle, the AMP proposal is basically
about making `Applicative` a superclass of `Monad`. That this hasn't been so
is often considered one of the historical errors Haskell carries around, since
a `Monad` is always an `Applicative`.

The hierachy of `Functor`, `Applicative` and `Monad` is thus changed to,

<!-- <div class="snippet-title">Typeclass hierachy</div> -->
```haskell,linenos
class Functor f where
    -- ...

class Functor f => Applicative f where
    -- ...

class Applicative m => Monad m where
    -- ...
```

which makes more sense than the arbitrary split before (i.e. `Monad` was just
"on its own").

The concerns surrounding this change has mostly been about
beginner-friendliness, since a `Monad` now needs an instance of both
`Applicative` and `Functor`.

There are various other considerations, which the interested reader can find out
more about on the [GHC 7.10 migration page](https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10) or the [Haskell wiki entry about AMP](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal).


## Back to Functor, Applicative and Monad

I'm mostly going to paraphrase [/u/ForTheFunctionGod's](https://www.reddit.com/user/ForTheFunctionGod) comment (which you can find [here](https://www.reddit.com/r/haskell/comments/3tpom7/amp_how_do_you_motivate_this_in_teaching/cx8an8b)), while trying to expand a bit on it with my own understanding.

One can naturally extend the intuition of a `Functor` to an `Applicative` and
from there to a `Monad`.

**An explanation of the first two might go as follows:**


> Let's say I have a list of numbers and want to add `2` to each of these
> numbers. You can write `fmap (+2) [1..10]` (using `Functor`).
>
> But what if I had two lists of numbers and wanted to add each number from the
> first list to each number from the second list? With `Applicative` you can do
> just that - it's like `fmap` only it can take multiple arguments. You can write
> `(+) <$> [1,5,10] <*> [11..13]`.[^1]


**From here, one can motivate `Monad` by asking:**

> What if I wanted to abort midway through - say, if I encountered a problem? You can then write:[^1]


```haskell,linenos
add xs ys = do
    x <- xs
    if x < 0 then []
    else do
        y <- ys
        if y < 0 then []
        else return (x+y)
```

Thus we have the natural hierachy:

1. `Functor`: apply a function to a container.
2. `Applicative`: apply a multi-argument function to multiple containers.
3. `Monad`: like `Applicative` but I can decide what to do next after each
   step.


## Examples of Functor, Applicative and Monad

While the above may help a bit in the _"why?"_, there is still the question of
how to use them. I won't go into much detail since there exists a wealth of
information on this topic already (you can quickly google them), but just give
some brief examples of what happens when using them.

__`Functor` is the simplest__, and can be thought of as a much more general `map`. Wherever you use `map` you can always replace it with `fmap`, but not the other way around.

```haskell
> fmap (*2) [1..10]
[2,4,6,8,10,12,14,16,18,20]
```

`fmap` simply takes every element of the list and applies the function to it.

__`Applicative` on the other hand__ is a bit more tricky to understand. The
best starting point is probably to show where `fmap` is not enough.

Imagine we want to apply the function `*` (multiplication) to a list, we could
try `fmap (*) [1..3]` but that would give us a bunch of partially applied
functions back, like `[(*1), (*2), (*3)]`.

Now, what can we do with this? We can for example map a value onto the list of
partially applied functions, which would look something like this using `fmap`,

```haskell
> let a = fmap (*) [1..3]
> fmap (\f -> f 9) a
[9,18,27]
```

which can be seen as applying a function that takes a function as argument an
applies `9` to that function, to every function in the list `a`. The real problem
comes when we want to apply a `Functor` function to `Functor` values, but I won't
get into much detail on that (you can read more [here](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors)).

Luckily, instead of writing the above, we can use `<*>` which is a part of
`Applicative`. We can instead write,

```haskell
> fmap (*) [1..3] <*> [9]
[9,18,27]
```

Note that the reason we put `9` inside a context (here a list), is because
`Applicative` expects everything to be a `Functor`. Since the first part is so
common, `Control.Applicative` actually exports `<$>`, so we can do the
following instead, replacing `fmap`,

```haskell
> (*) <$> [1..3] <*> [9]
[9,18,27]
```

Admittedly, it's a bit more interesting when we want to apply the multiple
functions to multiple arguments, as such,

```haskell
> (*) <$> [1,5,10] <*> [11..13]
[11,12,13,55,60,65,110,120,130]
> (*) <$> Just 3 <*> Just 5
```

or perhaps inside contexts, such as `Maybe`,

```haskell
> (*) <$> Just 3 <*> Just 5
Just 15
```

but I won't go into more detail about that, since that isn't the purpose of this post.

__`Monad` is the last__, but perhaps most tricky. I'll try to be as brief as
possible though. Too see the _"what happens next, based on what happened before"_
part, we will look into the `Maybe` monad used with `do` notation. It will not
explain `Monad` usage in general, but should be enough to get the gist that
_"something"_ happens between the steps.

For example (note that I use ; instead of linebreaks because we are executing
in the GHCi REPL),

```haskell
> do Just 6; Nothing; Just 9
Nothing
```

You may be aware that a `do` block returns the last line executed in it - so
why did it here return `Nothing` when the last line was `Just 9`? That is
because on each step the bind function `>>=` (or `=<<` for the other direction) is applied. The `Maybe` `Monad` defines that if it encounters a `Nothing` then every subsequent actions also return a `Nothing`.

To understand this better, let's take a look at the `Maybe` instance,

<!-- <div class="snippet-title">Maybe Monad typeclass instance</div> -->
```haskell,linenos
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f  = f x
    fail _ = Nothing
```

Notably here is the line that says `Nothing >>= f = Nothing`. It throws away
whatever future action it gets and just returns `Nothing`. This is in
contrast to `Just x >>= f = f x` which can be seen as unpacking `x` from `Just
x` and then applying the future action, `f`, to that `x`.


## Wrap up

Hopefully this should have helped understand the difference of `Functor`,
`Applicative` and `Monad` a bit, and how they work together. To understand
these concepts more in depth, I recommend reading a bit up on them, and
especially for `Monad`, try reading about the `Writer` and `State` `Monad`s,
how they are used and how they are implemented. One resource for that is the
[chapter on a few more monads](http://learnyouahaskell.com/for-a-few-monads-more) in LYAH.

If you are confused about _"What exactly is a Functor, is it a class or
interface or whatever?"_, then I gave my take on it [here](https://www.reddit.com/r/haskell/comments/3uebjq/grasping_haskell_functors_applicatives_and_monads/cxekkcn). Other than that, I encourage that you read a bit on Type Class'.

Finally, I can deeply recommend the book [Haskell Programming - from first
principle](http://haskellbook.com). It is still in development as of this date,
but already features 700 pages of quality content. Simply the best Haskell book
I have ever read (or, I'm still reading it as of writing).

[^1]: https://www.reddit.com/r/haskell/comments/3tpom7/amp_how_do_you_motivate_this_in_teaching/cx8an8b
