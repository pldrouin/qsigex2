      DOUBLE PRECISION FUNCTION SQSTUD(P,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(BIG=1D10,EPSILN=1D-6,ONE=1.D0,ZERO=0.D0,HALF=.5D0)
      EXTERNAL SZSTUD
C boundary of range
      IF(P.GE.ONE) SQSTUD=BIG
      IF(P.LE.ZERO) SQSTUD=-BIG
C normal range
      IF(P.LT.ONE .AND. P.GT.ZERO) THEN
        X0=ZERO
        X1=P
        CALL AUXZBR(X0,X1,SZSTUD,P,N,0)
        CALL AUXZFN(X0,X1,XZERO,SZSTUD,P,N,0,EPSILN)
        SQSTUD=XZERO
      END IF
      END
C-----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SZSTUD(X,P,N,NDUM)
C returns P minus cumulative Student's distribution of (X,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SZSTUD=P-SCSTUD(X,N)
      END
