      SUBROUTINE MINDIR(X0,DIR,N,USERFN,FMIN,NSTEP,EPSILN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXSTP=1000,EPSDEF=1.D-8)
      PARAMETER(DELTAX=1.D-3,CUT=0.1D0,ZERO=0.D0)
      DIMENSION X0(N),DIR(N)
      EXTERNAL USERFN
C initialization
      NST=NSTEP
      IF(NST.LE.0) NST=MAXSTP
      EPS=EPSILN
      IF(EPS.LE.ZERO) EPS=EPSDEF
      XA=0.
      E=ABS(XA)
      IF(E.LT.CUT) E=CUT
      XB=E*DELTAX
      NSTIN=NST
C enclose minimum
      CALL MINENC(XA,XB,XC,YA,YB,YC,USERFN,NST,X0,DIR,N)
      IF(NST.LT.0) THEN
        NSTEP=-1
        GO TO 20
      END IF
      IF(XC.LT.XA)THEN
C reverse order of arguments
        DUM=XC
        XC=XA
        XA=DUM
      END IF
      NST=NSTIN
C locate minimum
      CALL MINCMB(XA,XC,EPS,USERFN,XMIN,FMIN,NST,X0,DIR,N)
      IF(NST.LT.0) THEN
        NSTEP=-1
        GO TO 20
      END IF
      DO 10 I=1,N
        X0(I)=X0(I)+XMIN*DIR(I)
   10 CONTINUE
   20 CONTINUE
      END
