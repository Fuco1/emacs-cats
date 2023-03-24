;; -*- lexical-binding: t -*-
(require 'buttercup)

(require 'cats-data-state)

(describe "cats-data-state"

  (describe "fmap"

    (it "maps a function over a result of a stateful computation"
      (expect (cats-eval-state
               (cats-fmap
                #'1+
                (cats-data-state :run (lambda (s) (cons 1 s))))
               nil)
              :to-be 2)))

  (describe "apply"

    (it "to sequence two effects together"
      (expect (cats-exec-state
               (cats-apply
                (cats-data-state :run (lambda (s) (cons 1 (cons 1 s))))
                (cats-data-state :run (lambda (s) (cons 2 (cons 2 s)))))
               nil)
              :to-equal (list 2 1))))


  (describe "bind"

    (it "to sequence two effects together depending on the results of previous effects"
      (expect (cats-eval-state
               (cats-bind
                (cats-data-state :run (lambda (s) (cons 2 (cons 1 s))))
                (lambda (x)
                  (cats-data-state :run (lambda (s) (cons (cons x s) s)))))
               nil)
              :to-equal (list 2 1))))

  (describe "state management"

    (it "can get the current state"
      (expect (cats-eval-state (cats-state-get) 1) :to-be 1))

    (it "can set the current state"
      (expect (cats-exec-state (cats-state-put 2) 1) :to-be 2))

    (it "can modify the current state"
      (expect (cats-exec-state (cats-state-modify #'1+) 1) :to-be 2))))
