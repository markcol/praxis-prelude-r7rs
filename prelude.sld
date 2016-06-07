;; -*-  scheme -*-

;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis prelude)
  (import
    (scheme base)
    (praxis list)
    (praxis comprehension)
    (praxis matrix)
    (praxis hash-table)
    (praxis io)
    (praxis sort)
    (praxis high-order)
    (praxis bit)
    (praxis bit-vector)
    (praxis control-flow)
    (praxis date)
    (praxis assert)
    (praxis math)
    (praxis string)
    (praxis random)

    ; (praxis dictionary)
    ; (praxis match)
    ; (praxis structure)
    ; (praxis generator)
    ; (praxis misc)
    )
  (export
    ;; Lists
    take drop split take-while drop-while split-while
    cons* fold-left fold-right range mappend iterate
    filter remove flatten all? any? zip cross make-list
    sum maximum-by

    ;; List Comprehensions
    fold-of list-of sum-of nth-of

    ;; Pattern matching
    list-match list-match-aux

    ;; Structures
    define-structure

    ;; Matrices
    make-matrix matrix-rows matrix-cols matrix-ref matrix-set!
    for

    ;; Hash tables
    make-hash string-hash list->hash

    ;; Dictionaries
    make-dict tree key val lkid ht size bal nil nil? rot-left rot-right balance
    lookup insert update delete-successor delete nth rank avl-map avl-fold
    avl-for-each to-list from-list make-gen new dispatch

    ;; I/O
    read-file for-each-input map-input fold-input read-line filter-input

    ;; Strings
    string-index string-downcase string-upcase string-split string-join
    string-find

    ;; Sorting
    sort merge dosort domerge unique uniq-c vector-sort!

    ;; Higher-order functions
    identity constant fst snd compose complement swap
    left-section right-section curried-lambda define-curried

    ;; Math functions
    ipow isqrt ilog expm half double square add1 sub1 log2 log10
    digits undigits

    ;; Bits
    logand logior logxor lognot ash

    ;; Bit Vector
    make-bitvector bitvector-ref bitvector-set!
    bitvector-reset! bitvector-count bitvector-length

    ;; Random Numbers
    rand randit mod-diff flip-cycle init-rand next-rand inif-rand
    fortune shuffle

    ;; Control Flow
    when unless while let-values let-values-help

    ;; Generators
    define-generator

    ;; Date Arithmetic
    julian gregorian easter today

    ;; Unit Testing
    assert

    ;; Miscellaneous
    define-integrable define-macro when aif awhen gensym box unbox box!
    permutations
    )

  )
