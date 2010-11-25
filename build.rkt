#lang racket/base

(require (planet schematics/sake:1))

(define-task compile
  ()
  (action:compile "all-tests.rkt"))

(define-task test
  (compile)
  (action:test "all-tests.rkt" 'all-tests))

(define-task gui-test
  (compile)
  (let ([run (dynamic-require "plot-screen-test.rkt" 'run)])
    (run)))

(define-task all
  (test compile))

(define-task default
  (all))