;; -*- lexical-binding: t -*-
(require 'buttercup)

(require 'cats-data-maybe)
(require 'cats-data-applicative)

(describe "cats-data-applicative"

  (describe "methods"

    (it "should lift binary function with lift-a2"
      (expect (cats-lift-a2 #'+ (cats-just 1) (cats-just 2)) :to-equal (cats-just 3))))

  (describe "list instance"

    (it "should embed a value in a pure context"
      (expect (cats-pure nil 1) :to-equal (list 1)))

    (it "should implement non-deterministic effect for list instance"
      (expect (cats-apply
               (cats-apply (cats-pure nil (lambda (x) (lambda (y) (+ x y))))
                           (list 1 2))
               (list 3 4))
              :to-equal (list 4 5 5 6))))

  (describe "vector instance"

    (it "should embed a value in a pure context"
      (expect (cats-pure [] 1) :to-equal [1]))

    (it "should implement non-deterministic effect for vector instance"
      (expect (cats-apply
               (cats-apply (cats-pure [] (lambda (x) (lambda (y) (+ x y))))
                           (vector 1 2))
               (vector 3 4))
              :to-equal [4 5 5 6])))

  (describe "maybe instance"

    (it "should embed a value in a pure context"
      (expect (cats-pure (cats-nothing) 1) :to-equal (cats-just 1)))

    (it "should apply function on two just instances"
      (expect (cats-apply
               (cats-apply (cats-pure (cats-nothing) (lambda (x) (lambda (y) (+ x y))))
                           (cats-just 1))
               (cats-just 2))
              :to-equal (cats-just 3)))

    (it "should implement short-circuit effect for maybe instance (just nothing)"
      (expect (cats-apply
               (cats-apply (cats-pure (cats-just 1) (lambda (x) (lambda (y) (+ x y))))
                           (cats-just 1))
               (cats-nothing))
              :to-equal (cats-nothing)))

    (it "should implement short-circuit effect for maybe instance (nothing just)"
      (expect (cats-apply
               (cats-apply (cats-pure (cats-just 1) (lambda (x) (lambda (y) (+ x y))))
                           (cats-nothing))
               (cats-just 2))
              :to-equal (cats-nothing)))

    (it "should implement short-circuit effect for maybe instance (nothing nothing)"
      (expect (cats-apply
               (cats-apply (cats-pure (cats-just 1) (lambda (x) (lambda (y) (+ x y))))
                           (cats-nothing))
               (cats-nothing))
              :to-equal (cats-nothing)))))
