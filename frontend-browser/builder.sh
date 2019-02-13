source $stdenv/setup
mkdir $out
cp -r $frontendMin/bin $out/bin
chmod 755 $out/bin/frontend.jsexe
cp $src/js/auth0.min.js $out/bin/frontend.jsexe
cp $src/js/auth.js $out/bin/frontend.jsexe
rm -f $out/bin/frontend.jsexe/index.html
cp $src/index.html $out/bin/frontend.jsexe/index.html
