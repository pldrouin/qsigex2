DEST      = .
SOURCE    = .
LIBRARY   = libdatsrc.a
CC        = gcc
CFLAGS    = -g -fPIC -I../include

.c.o:
	$(CC) -c $(CFLAGS) $< 

OBJS            = auxder.o auxdrg.o auxdri.o auxgrd.o auxhes.o \
		  auxzbr.o auxzfn.o avoutp.o avtble.o gbetaf.o \
		  gbinco.o ggamma.o gincbt.o gincgm.o glngam.o \
		  lsq2ex.o lsqasg.o lsqasm.o \
		  lsqasn.o lsqcog.o lsqcon.o lsqexp.o lsqgen.o \
		  lsqgfn.o lsqgss.o lsqlin.o lsqmar.o lsqnon.o \
		  lsqp2g.o lsqpol.o minasy.o mincjg.o mincmb.o \
		  mincnt.o mincov.o mindir.o minenc.o minfnd.o \
		  mingld.o minglp.o mingls.o mingsq.o minmar.o \
		  minpow.o minprb.o minqdr.o minsim.o mtxadd.o \
		  mtxadv.o mtxchi.o mtxchl.o mtxchm.o mtxcpv.o \
		  mtxdec.o mtxdot.o mtxequ.o mtxgcl.o mtxgrw.o \
		  mtxgsm.o mtxgsv.o mtxgva.o mtxgvd.o mtxgvt.o \
		  mtxhsd.o mtxhst.o mtxlsc.o \
		  mtxmar.o mtxmat.o mtxmbt.o mtxmlt.o mtxmsc.o \
		  mtxmsv.o mtxnrv.o mtxpcl.o mtxprw.o mtxpsm.o \
		  mtxpsv.o mtxsbv.o mtxsub.o mtxsv1.o mtxsv2.o \
		  mtxsv3.o mtxsv4.o mtxsvd.o mtxtra.o mtxtrp.o \
		  mtxunt.o mtxwrt.o mtxzer.o mtxzrv.o regcon.o \
		  regpol.o rnecuy.o rnline.o rnmlcg.o rnmngn.o \
		  rnmnpr.o rnradi.o rnstnr.o scbinm.o scchi2.o \
		  scftst.o schypg.o scnorm.o scpois.o scstnr.o \
		  scstud.o sdbinm.o sdchi2.o sdftst.o sdhypg.o \
		  sdnorm.o sdpois.o sdstnr.o sdstud.o smerqs.o \
		  smerss.o smhsfl.o smhsgr.o smhsin.o smmnvr.o \
		  smsdgr.o sqchi2.o sqftst.o sqnorm.o sqpois.o \
		  sqstnr.o sqstud.o timser.o

$(LIBRARY):     $(OBJS)
	ar rvu $@ $(OBJS)
	ranlib $@

$(OBJS): ../include/datsrc.h
