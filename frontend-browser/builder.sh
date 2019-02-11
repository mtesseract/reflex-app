source $stdenv/setup
mkdir $out
cp -r $frontendMin/bin $out/bin
chmod 755 $out/bin/frontend.jsexe
rm -f $out/bin/frontend.jsexe/index.html
cat > $out/bin/frontend.jsexe/index.html <<EOF
<!DOCTYPE html>
<html>
  <head>
    <script language="javascript" src="allmin.js"></script>
  </head>
  <body>
  </body>
</html>
EOF
