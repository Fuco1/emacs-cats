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

A type `f` is a `Functor` if it provides a function `cats-fmap` which,
given any types `a` and `b` lets you apply any function from `(a ->
b)` to turn an `f a` into an `f b`, preserving the structure of
`f`.

Example: a list is a functor with `cats-fmap` equal to `mapcar`.

Minimal implementation of Functor is:

- `cats-fmap`

The following laws must be satisfied:

``` emacs-lisp
;; Identity
  (cats-fmap #'identity a)
= (identity a)

;; Composition
  (cats-fmap (lambda (x) (funcall f (funcall g x))) structure)
= (cats-fmap f (cats-fmap g structure))
```

Instances for built-in Elisp types:

- `list`
- `cons`
- `vector`

Instances for types provided by Cats:

- `cats-data-maybe`
- `cats-data-ziplist`
- `cats-data-state`

## Applicative

A functor with application, providing operations to embed pure
expressions (`cats-pure`), and sequence computations and combine their
results (`cats-apply`).  Unlike Monads, Applicative application can
not decide what to do next based on the result of previous action.

Each Applicative must also implement **Functor**.

Minimal implementation of Applicative is:

- `cats-pure`
- `cats-apply`

Additional methods:

- `cats-lift-a2`

Instances for built-in Elisp types:

- `list` (non-deterministic)
- `cons` (zipper)
- `vector` (non-deterministic)

Instances for types provided by Cats:

- `cats-data-ziplist` is a list wrapper with zipping apply
- `cats-data-maybe`
- `cats-data-state`

## Monad

The Monad class defines the basic operations over a monad, an abstract
datatype of actions.  Unlike Applicative, Monad binding can decide
what to do next based on the result of previous action.

Each Monad must also implement **Applicative**.

Minimal implementation of Monad is:

- `cats-return`
- `cats-bind`

Additional methods:

- `cats-seq`

The following laws must be satisfied:

``` emacs-lisp
;; Left identity
  (cats-bind (cats-return monad-type a) fn)
= (funcall fn a)

;; Right identity
  (cats-bind m (lambda (x) (cats-return monad-type x)))
= m

;; Associativity
  (cats-bind m (lambda (x) (cats-bind (funcall k x) h)))
= (cats-bind (cats-bind m k) h)
```

Instances for built-in Elisp types:

- `list` (non-deterministic)
- `vector` (non-deterministic)

Instances for types provided by Cats:

- `cats-data-maybe`
- `cats-data-state`

## Monoid

The class of monoids (types with an associative binary operation that
has an identity).

Minimal implementation of Monoid is:

- `cats-mempty`
- `cats-mappend`

Additional methods:

- `cats-mconcat`

The following laws must be satisfied:

``` emacs-lisp
;; Right Identity
(cats-mappend a (cats-mempty a)) = a

;; Left Identity
(cats-mappend (cats-mempty a) a) = a

;; Associativity (semigroup)
  (cats-mappend a (cats-mappend b c))
= (cats-mappend (cats-mappend a b) c)

;; Concatenation
  (cats-mconcat list-of-a)
= (cats-foldr #'cats-mappend (cats-mempty item-of-a) list-of-a)
```

Instances for built-in Elisp types:

- `list`
- `vector`
- `number` (with sum)
- `string` (with concat)

Instances for types provided by Cats:

- `cats-data-endo` - Monoid of endomorphisms under composition

## Foldable

The Foldable class represents data structures that can be reduced to a
summary value one element at a time.

Minimal implementation of Foldable is:

- `cats-fold-map` or `cats-foldr`

Instances for built-in Elisp types:

- `list`
- `vector`

Instances for types provided by Cats:

- `cats-data-maybe`

## Traversable

Functors representing data structures that can be transformed to
structures of the same shape by performing an Applicative (or,
therefore, Monad) action on each element from left to right.

Each Traversable must also implement **Functor** and **Foldable**.

Minimal implementation of Traversable is:

- `cats-traverse` or `cats-sequence-a`

Instances for built-in Elisp types:

- `list`

Instances for types provided by Cats:

- `cats-data-maybe`
