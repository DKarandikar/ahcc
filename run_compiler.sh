set -e
~/ahcc/dist/build/c-compiler/c-compiler $1
a=$1
xbase=${a##*/}
xpref=${1%.*}
# echo $xpref
gcc assembly.s -o $xpref
chmod +x $xpref
rm assembly.s