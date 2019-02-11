source $stdenv/setup
mkdir $out
cp -r $frontend/bin $out/bin
chmod 755 $out/bin/frontend.jsexe
$closurecompiler/bin/closure-compiler --compilation_level SIMPLE --jscomp_off=checkVars $out/bin/frontend.jsexe/all.js > $out/bin/frontend.jsexe/allmin.js
rm $out/bin/frontend.jsexe/all.js
