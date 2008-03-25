      SUBROUTINE MINCJG(X0,N,NRED,LIST,USERFN,FMIN,EPSILN,NSTEP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXN=50)
      DIMENSION X0(N),GR(MAXN),G(MAXN),H(MAXN),D(MAXN),LIST(N)
      PARAMETER(MAXSTP=1000,EPSDEF=1.D-8,T=1.D-15,ZERO=0.D0,
     +ONE=1.D0)
      EXTERNAL USERFN
      IF(NSTEP.LT.1) NSTEP=MAXSTP
      EPS=EPSILN
      IF(EPS.LE.ZERO) EPS=EPSDEF
C initialization : directions G and H identical to gradient at X0
      FMINL=USERFN(X0,N)
      IF(NRED.LE.0) THEN
C all variables are fixed
        FMIN=FMINL
        NSTEP=1
        GO TO 40
      END IF
      CALL AUXGRD(X0,GR,N,NRED,LIST,USERFN)
      CALL MTXMSV(GR,G,-ONE,N)
      CALL MTXCPV(G,H,N)
C start iteration
      DO 10 ISTEP=1,NSTEP
        LSTEP=ISTEP
        IF(ISTEP.GT.1) THEN
          IF(ABS(FMINL-FMIN) .LT. EPS*ABS(FMIN)+T) THEN
            NSTEP=ISTEP
            GO TO 30
          END IF
          FMINL=FMIN
        END IF
        NST=NSTEP
C X0 is position of minimum along direction H
        CALL MINDIR(X0,H,N,USERFN,FMIN,NST,ZERO)
        IF(NST.LT.0) GO TO 20
        CALL AUXGRD(X0,GR,N,NRED,LIST,USERFN)
C GR is gradient at X0
C compute next set of directions G and H
        CALL MTXMSV(GR,GR,-ONE,N)
        CALL MTXSBV(GR,G,D,N)
        CALL MTXDOT(D,GR,S,N)
        CALL MTXDOT(G,G,A,N)
        IF(A.EQ.ZERO) GO TO 30
        GAMMA=S/A
        CALL MTXMSV(H,H,GAMMA,N)
        CALL MTXADV(GR,H,H,N)
        CALL MTXCPV(GR,G,N)
   10 CONTINUE
   20 NSTEP=-1
      GO TO 40
   30 NSTEP=LSTEP
   40 CONTINUE
      END
