#lang racket/base

(define-syntax-rule (log-bonfire string-expr)
  (log-info (string-append "schematics/bonfire " string-expr)))


(provide log-bonfire)