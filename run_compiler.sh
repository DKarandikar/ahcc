set -e
ghc ~/haskell/c_compiler/compiler.hs
~/haskell/c_compiler/compiler $1
a=$1
xbase=${a##*/}
xpref=${1%.*}
# echo $xpref
gcc assembly.s -o $xpref
chmod +x $xpref
rm assembly.s