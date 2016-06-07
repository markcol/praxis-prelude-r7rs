# Programming Praxis Standard Prelude for R7RS Scheme

This repository mplements the
[Programming Praxis](https://programmingpraxis.com)
[Standard Prelude](https://programmingpraxis.com/conents/standard-prelude/)
as modules for use in R7RS Scheme code.

## Installation

This package is intended to be used via the R7RS module system. The
easiest way to do that is to extract this repository into the “praxis” directory
in your personal modules directory.

For example, using [Chibi Scheme](http://synthcode.com/wiki/chibi-scheme):

``` shell
$ export CHIBI_MODULE_PATH=~/lib/scheme
$ cd $CHIBI_MODULE_PATH
$ git clone https://github.com/markcol/praxis-prelude-r7rs praxis
```

Alternatively, on Unix-like systems, you could symlink the
Git repository to your custom module structure:

``` shell
$ cd ~/src
$ git clone https://github.com/markcol/praxis-prelude-r7rs
$ ln -s ~/src/praxis-prelude-r7rs ~/lib/scheme/praxis
```

Other Scheme implementations may use different methods to define the
location of custom modules. See your implementation's documentation
for details.

## Usage

To use the Standard Prelude in your code, import the “(praxis prelude)” module:

``` scheme
(import (scheme small)
        (praxis prelude))

;; ...
```

If you would like to use only specific components of the prelude, you
can load specific sub-modules:

``` scheme
(import (scheme small)
        (praxis matrix))

;; ...
```

The submodules correspond (roughly) to the sections within the
Standard Prelude, as shown in the following table:

Submodule     | Standard Prelude Section
:-------------|:-------------------------
list          | List Utilities
comprehension | List Comprehensions
match         | Pattern Matching
structure     | Structures
matrix        | Matricies
hash-table    | Hash Tables
dictionary    | Dictionaries
io            | Input-Output
string        | Strings
sort          | Sorting
high-order    | Higher-Order Functions
math          | Math Functions
bits          | Bits
bit-vector    | Bits
random        | Random Numbers
control-flow  | Control Flow
date          | Date Arithmetic
assert        | Unit Testing
misc          | Miscellaneous

## Known Issues

1. Have not finished converting code that uses `syntax-case'.
1. The 'date' module requirres implementation-specific code.
   Implmentations currently exist only for Chibi and Chez Scheme.

## TODO

1. Add more implementation-specific code for date support using
   '(cond-expand ...)'.
1. Add tests to ensure that code works as expected.
1. Add implementation-specific tests to ensure that the code works
   properly across a variety of popular Scheme implementations
   (Bigloo, Chez, Chicken, Chibi, Gauche at a minimum).
1. Check for SRFIs that should be used, instead of code here, if appropriate.
1. Chose optimal implementation-specific code using '(cond-expand ...)'.
