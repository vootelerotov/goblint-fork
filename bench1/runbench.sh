#!/bin/sh

ulimit -Ss 49152
export OCAMLRUNPARAM=s=32M,i=32M,o=150

CMD="../goblint"

date

for c in bench_*
do
  for f in aget_comb.c pfscan_comb.c ctrace_comb.c knot_comb.c smtprc_comb.c ypbind_comb.c automount_comb.c zfs-fuse_comb.c
  do
    echo "";
    echo "";
    echo "";
    echo Analyzing: ${f};
    echo ${CMD} --conf ${c} ${f};
  	${CMD} --conf ${c} ${f} 
  done
done
#mv result.txt result.sens.widen.txt
