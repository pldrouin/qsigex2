#!/bin/bash

set -x

DLIB=libdatsrc.a
SLIB=libdatan.so
SRCDIR=../datsrc
LIBDIR=../../lib

for X in \
auxder auxdrg auxdri auxgrd auxhes auxzbr auxzfn avoutp \
avtble gbetaf gbinco ggamma gincbt gincgm glngam lsq2ex \
lsqasg lsqasm lsqasn lsqcog lsqcon lsqexp lsqgen lsqgfn \
lsqgss lsqlin lsqmar lsqnon lsqp2g lsqpol minasy mincjg \
mincmb mincnt mincov mindir minenc minfnd mingld minglp \
mingls mingsq minmar minpow minprb minqdr minsim mtxadd \
mtxadv mtxchi mtxchl mtxchm mtxcpv mtxdec mtxdot mtxequ \
mtxgcl mtxgrw mtxgsm mtxgsv mtxgva mtxgvd mtxgvt mtxhsd \
mtxhst mtxlsc mtxmar mtxmat mtxmbt mtxmlt mtxmsc mtxmsv \
mtxnrv mtxpcl mtxprw mtxpsm mtxpsv mtxsbv mtxsub mtxsv1 \
mtxsv2 mtxsv3 mtxsv4 mtxsvd mtxtra mtxtrp mtxunt mtxwrt \
mtxzer mtxzrv regcon regpol rnecuy rnline rnmlcg rnmngn \
rnmnpr rnradi rnstnr scbinm scchi2 scftst schypg scnorm \
scpois scstnr scstud sdbinm sdchi2 sdftst sdhypg sdnorm \
sdpois sdstnr sdstud smerqs smerss smhsfl smhsgr smhsin \
smmnvr smsdgr sqchi2 sqftst sqnorm sqpois sqstnr sqstud \
timser
do
  if [ -f $X.f ]; then
    FILE=$X.f
  else
    FILE=$SRCDIR/$X.f
  fi
  g77 -c -mcpu=i486 -mieee-fp -ffloat-store -fno-second-underscore -fno-f2c -O $FILE
done
rm -f $LIBDIR/$DLIB
ar avu $LIBDIR/$DLIB *.o
ar xv $LIBDIR/libgrsrc.a
gcc -shared -Wl,-soname,$SLIB.1 -o $SLIB.1.1 *.o
ar t $LIBDIR/libgrsrc.a | while read X
do
  rm -f $X
done
mv -f $SLIB.1.1 ../../lib/.
