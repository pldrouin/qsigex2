      SUBROUTINE MINPOW(X0,N,NRED,LIST,USERFN,FMIN,EPSILN,
     +NSTEP,ALLDIR)
      PARAMETER(MAXN=50)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X0(N),XOLD(MAXN),XE(MAXN),DIR(MAXN),DIRN(MAXN)
      DIMENSION LIST(N),ALLDIR(N,NRED)
      EXTERNAL USERFN
      PARAMETER(MAXSTP=1000,EPSDEF=1.D-8,T=1.D-15,ONE=1.D0,
     +ZERO=0.D0)
      IF(NSTEP.LT.1) NSTEP=MAXSTP
      EPS=EPSILN
      IF(EPS.LE.0.) EPS=EPSDEF
C Initialize directions along coordinates
      F=USERFN(X0,N)
      IRED=0
      DO 10 I=1,N
        IF(LIST(I).EQ.1) THEN
          IRED=IRED+1
          CALL MTXZRV(DIR,N)
          DIR(I)=ONE
          CALL MTXPCL(ALLDIR,DIR,N,NRED,IRED)
        END IF
   10 CONTINUE
      DO 30 ISTEP=1,NSTEP
        LSTEP=ISTEP
        FOLD=F
        FL=FOLD
        CALL MTXCPV(X0,XOLD,N)
        DELTAF=0.
        IMAX=0
C Loop over complete direction set and find direction which
C yields largest function decrease. Its index is IMAX
        DO 20 I=1,NRED
          CALL MTXGCL(ALLDIR,DIR,N,NRED,I)
          NSTDIR=NSTEP
          CALL MINDIR(X0,DIR,N,USERFN,F,NSTDIR,ZERO)
          IF(NSTDIR.LT.0) GO TO 40
          D=ABS(F-FL)
          IF(D.GT.DELTAF) THEN
            DELTAF=D
            IMAX=I
          END IF
          FL=F
   20   CONTINUE
C Test for break-off criterion
        IF(ABS(FL-FOLD) .LT. EPS*ABS(FL)+T) GO TO 50
C Construct extrapolated point XE and direction DIRN from
C XOLD to X0
        CALL MTXSBV(X0,XOLD,DIRN,N)
        CALL MTXADV(DIRN,X0,XE,N)
        FE=USERFN(XE,N)
C Now there are 3 points (XOLD, X0, XE)
C and their function values (FOLD, F, FE)
        IF(FE.LT.F) THEN
          TEST=2.*(FOLD-2.*F+FE)*(FOLD-F-DELTAF)**2-
     +         (FOLD-FE)**2*DELTAF
          IF(TEST.LT.0.) THEN
            NST=NSTEP
C Find minimum along DIRN and replace X0 by position
C of minimum. Replace direction with index IMAX by DIRN
            CALL MINDIR(X0,DIRN,N,USERFN,F,NST,ZERO)
            IF(NST.LT.0) GO TO 40
            CALL MTXPCL(ALLDIR,DIRN,N,NRED,IMAX)
          END IF
        END IF
   30 CONTINUE
   40 NSTEP=-1
      GO TO 60
   50 NSTEP=LSTEP
      FMIN=FL
   60 CONTINUE
      END
