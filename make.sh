#! /bin/sh
set -e

# generate the version file
scripts/set_version.sh

TARGET=src/goblint
FRAMA="/usr/local/lib/frama-c"
FLAGS="-cflags -for-pack,Goblint -cflags -I,$FRAMA -lflags -I,$FRAMA -no-links -use-ocamlfind -j 8 -no-log -ocamlopt ocamlopt.opt"
OCAMLBUILD=ocamlbuild

ocb() {
  OCAMLFIND_IGNORE_DUPS_IN="$FRAMA" $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)   rm -rf goblint goblint.byte goblint.ml doclist.odocl;
             ocb -clean
             ;;
    opt | nat*)
             ocb -no-plugin $TARGET.native &&
             cp _build/$TARGET.native goblint
             ;;
    debug)   ocb -tag debug $TARGET.native &&
             cp _build/$TARGET.native goblint
             ;;
    bdebug)  ocb -tag debug $TARGET.d.byte &&
             cp _build/$TARGET.d.byte goblint.byte
            ;;
    profile) ocb -tag profile $TARGET.p.native &&
             cp _build/$TARGET.p.native goblint
             ;;
    byte)    ocb $TARGET.byte &&
             cp _build/$TARGET.byte goblint.byte
             ;;
    plug*)   ocb frama.cmxs && sudo mv _build/src/frama.cmxs $FRAMA/plugins/Goblint.cmxs
             ;;
    all)     ocb $TARGET.native $TARGET.byte &&
             cp _build/$TARGET.native goblint &&
             cp _build/$TARGET.byte goblint.byte
             ;;
    doc*)    rm -rf doc;
             ls src/*/*/*.ml src/*/*.ml src/*.ml | sed 's/.*\/\(.*\)\.ml/\1/' > doclist.odocl;
             ocb -ocamldoc ocamldoc.opt -docflags -colorize-code,-keep-code doclist.docdir/index.html;
             rm doclist.odocl;
             ln -sf _build/doclist.docdir doc
             ;;
    tag*)    otags -vi `find src/ -iregex [^.]*\.mli?`;;
    depend)  echo "No!";;
    *)       echo "Unknown action '$1'. Try clean, opt, debug, profile, byte, or doc.";;
  esac; }

create_deps() {
  ls -1 src/*/*.ml | perl -pe 's/.*\/(.*)\.ml/open \u$1/g' > $TARGET.ml
  case $1 in
    plug*) echo "open Frama" >> $TARGET.ml ;;
    *)     echo "open Maingoblint" >> $TARGET.ml ;;
  esac
}

if [ $# -eq 0 ]; then
  create_deps all;
  rule all
else
  while [ $# -gt 0 ]; do
    create_deps $1;
    rule $1;
    shift 
  done
fi

#rm -f $TARGET.ml
