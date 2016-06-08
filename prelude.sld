;; -*-  scheme -*-
;; From http://programmingpraxis.com/contents/standard-prelude/

(define-library (praxis prelude)
  (import (scheme base)
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
	  (praxis misc)
	  (praxis dictionary)
	  (praxis match)
	  (praxis structure)
	  (praxis generator))
  (export take drop split take-while drop-while split-while
	  cons* fold-left fold-right range mappend iterate
	  filter remove flatten all? any? zip cross make-list
	  sum maximum-by fold-of list-of sum-of nth-of list-match
	  list-match-aux define-structure make-matrix matrix-rows
	  matrix-cols matrix-ref matrix-set! for make-hash
	  string-hash list->hash make-dict tree key val lkid
	  ht size bal nil nil? rot-left rot-right balance lookup
	  insert update delete-successor delete nth rank avl-map
	  avl-fold avl-for-each to-list from-list make-gen new
	  dispatch read-file for-each-input map-input fold-input
	  read-line filter-input string-index string-downcase
	  string-upcase string-split string-join string-find sort
	  merge unique uniq-c vector-sort! identity constant fst
	  snd compose complement swap left-section right-section
	  curried-lambda define-curried ipow isqrt ilog expm half
	  double square add1 sub1 log2 log10 digits undigits logand
	  logior logxor lognot ash make-bitvector bitvector-ref
	  bitvector-set! bitvector-reset! bitvector-count
	  bitvector-length rand randint mod-diff flip-cycle
	  init-rand next-rand unif-rand fortune shuffle when
	  unless while let-values define-generator julian
	  gregorian easter today assert define-integrable
	  define-macro when aif awhen gensym box unbox box!
	  permutations))
s
