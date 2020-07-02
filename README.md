# ahcc - Almost a Haskell C Compiler

This is an attempt to kill 4 birds with one stone: learning something about Haskell, compilers, the C language and assembly at once.

The tutorial series on the symbolic calculator avaiable at https://www.schoolofhaskell.com/user/school/starting-with-haskell/basics-of-haskell is being used to learn somethign about Haskell and compilers.

The tutorial series at https://norasandler.com/2017/11/29/Write-a-Compiler.html is being used to apply this to a real C compiler.

## Installation

Install ghc and cabal

Clone this repo, cd and run the following:

`cabal install --only-dependencies`  
`cabal configure`  
`cabal build`

## Usage

The compiler can then be run from `dist/build/c-compiler/c-compiler`

If provided with the path to a .c file it will write assembly.s, this can then be linked with gcc and then run

If provided with any argument after the file path it will instead print a variety of steps out to the console and not output assembly.s. These include a list of tokens, the AST and the outputted assembly. 

## Running tests

The repo write_a_c_compiler contains lots of test .c files, and a script for running these which uses `run_compiler` from this repo

This can be run by cloning that repo, cd'ing in and running e.g. ` ./test_compiler.sh ~/ahcc/run_compiler.sh 3 4`

More details on the tests can be found in the readme of that repo