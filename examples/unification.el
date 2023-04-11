;; -*- lexical-binding: t -*-
;;
;; Unification of numbers, strings and lists with variables
;; represented by keywords.  You can try to extend the example to
;; hash-tables, vectors, conses or anything else!
;;
;; The substitution alist is kept in the state monad.
;;
;; Examples:
;;
;; (cats-run-unif (list :a) (list 1))
;; => "((1) . {:a -> 1})"
;;
;; (cats-run-unif (list 1 2 :a :w) (list :x 2 (list 9 10) :y))
;; => "((1 2 (9 10) :y) . {:x -> 1, :a -> (9 10), :w -> :y})"

(defun cats-unif-return (a)
  "Return A in the state monad."
  (cats-return (cats-data-state) a))

(cl-defgeneric cats-substitute (a)
  "Substitute all the variables in A according to current state.

Variables are represented by keywords."
  (cats-do
   ;; get the state from the monad
   (:= subs (cats-state-get))
   ;; action which performs the substitution
   (cats-unif-return
    (cl-reduce
     (lambda (struct sub)
       (cl-subst (cdr sub) (car sub) struct))
     subs
     :initial-value a))))

(cl-defmethod cats-substitute ((a cats-data-ziplist))
  "Unboxing for substitution inside ziplists."
  ;; Map cats-ziplist on the result of cats-substitute (a monadic
  ;; action).  A map on a monadic action returns a new action which
  ;; calls the mapping function on the result of the original.
  (cats-fmap #'cats-ziplist (cats-substitute (oref a list))))

(defun cats-unif-add (from to)
  "Add substitution from FROM to TO to the state."
  (cats-do
   ;; modify the current state with a callback
   (cats-state-modify
    (lambda (state)
      (if (assoc from state)
          state
        (cons (cons from to) state))))
   ;; return the new TO value.
   (cats-unif-return to)))

(cl-defgeneric cats-unif (this other)
  "Unify THIS with OTHER."
  (cats-do
   ;; apply the existing substitutions
   (:= sub-this (cats-substitute this))
   (:= sub-other (cats-substitute other))
   ;; Sequence takes an applicative shape of actions and returns an
   ;; action with result of the same applicative shape.
   ;; (applicative (state a s)) => (state (applicative a) s)
   ;; For example (list (cats-unif 1 2) (cats-unif :a 2)) becomes
   ;; (state (list 1 2) (:a 1)), where the (list 1 2) corresponds to
   ;; the "applicative of actions" and (:a 1) is the state.
   (cats-sequence
    ;; the lift takes X x Y -> (state a s) and and two applicatives of
    ;; same shape with X and Y and produces the result (applicative
    ;; (state a s)) of the same shape.
    (cats-lift-a2 #'cats-unif sub-this sub-other)
    (cats-data-state))))

(cl-defmethod cats-unif ((this number) (other number))
  "Unify two numbers."
  (cats-unif-return
   (if (eq this other)
       other
     (list 'error (format "This %s is not equal to other %s" this other)))))

(cl-defmethod cats-unif ((this string) (other string))
  "Unify two strings."
  (cats-unif-return
   (if (eq this other)
       other
     (list 'error (format "This %s is not equal to other %s" this other)))))

(cl-defmethod cats-unif ((this list) (other list))
  "Unify two lists.

We wrap the lists with `cats-ziplist' to provide pairwise
unification instead of the default nondeterminism interpretation
of list monad."
  (cats-do
   ;; re is a ziplist
   (:= re
    (cats-unif
     ;; use ziplist to process lists "pairwise" instead of each-to-each
     ;; combination.
     (cats-ziplist this)
     (cats-ziplist other)))
   ;; extract list from ziplist and return it
   (cats-unif-return (oref re list))))

(cl-defmethod cats-unif ((this symbol) (other symbol))
  "Unify two symbols.

If both THIS and OTHER are variables, replace THIS with OTHER,
otherwise run recursive unification."
  (cats-do
   (:= sub-this (cats-substitute this))
   (:= sub-other (cats-substitute other))
   (if (and (keywordp sub-this)
            (keywordp sub-other))
       (cats-unif-add sub-this sub-other)
     (cats-unif sub-this sub-other))))

(cl-defmethod cats-unif ((this symbol) other)
  "Unify THIS symbol with OTHER.

If THIS is a keyword, replace it everywhere with OTHER."
  (cats-do
   (:= sub-this (cats-substitute this))
   (:= sub-other (cats-substitute other))
   (if (keywordp sub-this)
       ;; replace this keyword with other
       (cats-unif-add sub-this sub-other)
     (cats-unif sub-this sub-other))))

(cl-defmethod cats-unif (this (other symbol))
  "Unify THIS with OTHER symbol.

If OTHER is a keyword, replace it everywhere with THIS."
  (cats-do
   (:= sub-this (cats-substitute this))
   (:= sub-other (cats-substitute other))
   (if (keywordp sub-other)
       ;; replace other keyword with this
       (cats-unif-add sub-other sub-this)
     (cats-unif sub-this sub-other))))

(defun cats-run-unif (term-a term-b)
  "Wrapper to unify TERM-A and TERM-B."
  (let ((re
         ;; cats-run-state returns a cons of return value (the unified
         ;; term) and the state, which is the alist of used
         ;; substitutions.
         (cats-run-state
             (cats-unif term-a term-b)
             ;; The initial state of the monad is the initial
             ;; substitutions alist, which is empty.
             nil)))
    ;; Format the result for debugging.  In real-life you would
    ;; probably want to return the values instead.
    (format
     "(%S . {%s})"
     (car re)
     (mapconcat
      (lambda (sub)
        (format "%s -> %S" (car sub) (cdr sub)))
      (nreverse (cdr re))
      ", "))))
