      SUBROUTINE MINGLD(A,B,C,EPSILN,USERFN,XMIN,YMIN,NSTEP,X0,
     +XDIR,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(Q=0.61803D0,MAXSTP=1000,EPSDEF=1.D-8,TT=1.D-15)
      EXTERNAL USERFN
      EPS=EPSILN
      IF(EPS.LE.0.)EPS=EPSDEF
      IF(NSTEP.LT.1)NSTEP=MAXSTP
      S=USERFN(B,X0,XDIR,N)
      ISTEP=0
   10 ISTEP=ISTEP+1
      IF(ABS(B-A).GT.ABS(C-B)) THEN
        X=A+Q*(B-A)
        Y=USERFN(X,X0,XDIR,N)
        IF(Y.LT.S) THEN
          C=B
          B=X
          S=Y
        ELSE
          A=X
        END IF
      ELSE
        X=B+Q*(C-B)
        Y=USERFN(X,X0,XDIR,N)
        IF(Y.LT.S) THEN
          A=B
          B=X
          S=Y
        ELSE
          C=X
        END IF
      END IF
      IF (ABS(C-A).GT.EPS*ABS(A)+TT .AND. ISTEP.LE.NSTEP) GO TO 10
      IF(ISTEP.GT.NSTEP) THEN
        NSTEP=-1
      ELSE
        NSTEP=ISTEP
      END IF
      XMIN=B
      YMIN=C
      END
