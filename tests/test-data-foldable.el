;; -*- lexical-binding: t -*-
(require 'buttercup)

(require 'cats-data-foldable)

(describe "cats-data-foldable"

  (describe "list instance"

    (it "should fold with foldr"
      (expect (cats-foldr (lambda (x y) (+ x y)) 0 '(1 2 3))
              :to-equal 6))

    (it "should fold with fold-map"
      (expect (cats-fold-map (lambda (x) (list x x)) '(1 2 3) nil)
              :to-equal '(1 1 2 2 3 3)))

    (it "should get length with length"
      (expect (cats-length '(1 2 3)) :to-equal 3))

    (it "should convert to list with to-list"
      (expect (cats-to-list '(1 2 3)) :to-equal '(1 2 3)))

    (it "should find just an element with find"
      (expect (cats-find (lambda (x) (= x 2)) '(1 2 3))
              :to-equal (cats-just 2)))

    (it "should return nothing if it can't find an element"
      (expect (cats-find (lambda (x) (= x 4)) '(1 2 3))
              :to-equal (cats-nothing))))

  (describe "vector instance"

    (it "should fold with foldr"
      (expect (cats-foldr (lambda (x y) (+ x y)) 0 [1 2 3])
              :to-equal 6))

    (it "should fold with fold-map"
      (expect (cats-fold-map (lambda (x) (list x x)) [1 2 3] nil)
              :to-equal '(1 1 2 2 3 3)))

    (it "should get length with length"
      (expect (cats-length [1 2 3]) :to-equal 3))

    (it "should convert to list with to-list"
      (expect (cats-to-list [1 2 3]) :to-equal '(1 2 3)))

    (it "should find just an element with find"
      (expect (cats-find (lambda (x) (= x 2)) [1 2 3])
              :to-equal (cats-just 2)))

    (it "should return nothing if it can't find an element"
      (expect (cats-find (lambda (x) (= x 4)) [1 2 3])
              :to-equal (cats-nothing))))

  (describe "maybe instance"

    (it "should fold with foldr"
      (expect (cats-foldr (lambda (x y) (+ x y)) 0 (cats-just 1))
              :to-equal 1))

    (it "should fold with fold-map"
      (expect (cats-fold-map (lambda (x) (list x x)) (cats-just 1) nil)
              :to-equal '(1 1)))

    (it "should get length with length"
      (expect (cats-length (cats-just 1)) :to-equal 1))

    (it "should convert to list with to-list"
      (expect (cats-to-list (cats-just 1)) :to-equal '(1)))

    (it "should find just an element with find"
      (expect (cats-find (lambda (x) (= x 1)) (cats-just 1))
              :to-equal (cats-just 1)))

    (it "should return nothing if it can't find an element"
      (expect (cats-find (lambda (x) (= x 1)) (cats-just 2))
              :to-equal (cats-nothing)))

    (it "should return nothing when the argument is nothing"
      (expect (cats-find (lambda (x) (= x 1)) (cats-nothing))
              :to-equal (cats-nothing)))))
