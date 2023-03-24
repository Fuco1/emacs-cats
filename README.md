# Category theory in Emacs

This package implements various abstractions from Category Theory
highly inspired by Haskell.

The complete (Haskell) documentation can be found on the
[hackage](https://hackage.haskell.org/package/base-4.18.0.0).

# Usage

Because Emacs runtime does not possess any abilities of type
inference, we must explicitly pass the pure and return constructors to
some functions where it is impossible to get from context.  Other than
this, the usage and interface is the same as the Haskell libraries.

# Methods

Implemented methods:

## Functors

Minimal implementation of Functor is:

- `cats-fmap`

Instances for built-in Elisp types:

- `list`
- `cons`
- `vector`

Instances for types provided by Cats:

- `cats-data-maybe`
- `cats-data-state`

## Applicative

Minimal implementation of Applicative is:

- `cats-pure`
- `cats-apply`

Additional methods:

- `cats-lift-a2`

Instances for built-in Elisp types:

- `list`
- `vector`

Instances for types provided by Cats:

- `cats-data-maybe`
- `cats-data-state`

## Monad

Minimal implementation of Monad is:

- `cats-return`
- `cats-bind`

Additional methods:

- `cats-seq`

Instances for built-in Elisp types:

- `list`
- `vector`

Instances for types provided by Cats:

- `cats-data-maybe`
- `cats-data-state`

## Monoid

Minimal implementation of Monoid is:

- `cats-mempty`
- `cats-mappend`

Additional methods:

- `cats-mconcat`

Instances for built-in Elisp types:

- `list`
- `vector`

Instances for types provided by Cats:

- `cats-data-endo` - Monoid of endomorphisms under composition

## Foldable

Minimal implementation of Foldable is:

- `cats-fold-map` or `cats-foldr`

Instances for built-in Elisp types:

- `list`
- `vector`

Instances for types provided by Cats:

- `cats-data-maybe`

## Traversible
