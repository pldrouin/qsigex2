      DOUBLE PRECISION FUNCTION SQSTNR(P)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(BIG=1.D10,EPSILN=1D-8,ONE=1.D0,ZERO=0.D0,HALF=.5D0)
      EXTERNAL SZSTNR
C boundary of range
      IF(P.GE.ONE) SQSTNR=BIG
      IF(P.LE.ZERO) SQSTNR=-BIG
C normal range
      IF(P.LT.ONE .AND. P.GT.ZERO) THEN
        X0=ZERO
        X1=DBLE(0.1)
        CALL AUXZBR(X0,X1,SZSTNR,P,0,0)
        CALL AUXZFN(X0,X1,XZERO,SZSTNR,P,0,0,EPSILN)
        SQSTNR=XZERO
      END IF
      END
C-----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SZSTNR(X,P,NDUM1,NDUM2)
C returns P minus cumulative standardized normal of X
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SZSTNR=P-SCSTNR(X)
      END
