      DOUBLE PRECISION FUNCTION SQCHI2(P,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (BIG=1D10,ONE=1.D0, ZERO=0.D0)
      EXTERNAL SZCHI2
C boundary of range
      IF(P.GE.ONE) SQCHI2=BIG
      IF(P.LE.ZERO) SQCHI2=ZERO
C normal range
      IF(P.LT.ONE .AND. P.GT.ZERO) THEN
        X1=DBLE(N)
        X0=DBLE(0.5)*X1
        CALL AUXZBR(X0,X1,SZCHI2,P,N,0)
        CALL AUXZFN(X0,X1,XZERO,SZCHI2,P,N,0,EPSILN)
        SQCHI2=XZERO
      END IF
      END
C-----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SZCHI2(X,P,N,NDUM)
C returns P minus cumulative chisquared distribution of (X,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SZCHI2=P-SCCHI2(X,N)
      END
