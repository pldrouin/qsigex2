      DOUBLE PRECISION FUNCTION SQFTST(P,N1,N2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (BIG=1D10, EPSILN=1D-6, ONE=1.D0, ZERO=0.D0)
      EXTERNAL SZFTST
C boundary of range
      IF(P.GE.ONE) SQFTST=BIG
      IF(P.LE.ZERO) SQFTST=ZERO
C normal range
      X0=ZERO
      X1=P
      IF(P.LT.ONE .AND. P.GT.ZERO) THEN
        CALL AUXZBR(X0,X1,SZFTST,P,N1,N2)
        CALL AUXZFN(X0,X1,XZERO,SZFTST,P,N1,N2,EPSILN)
        SQFTST=XZERO
      END IF
      END
C-----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SZFTST(X,P,N1,N2)
C returns P minus cumulative F-distribution of (X,N1,N2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SZFTST=P-SCFTST(X,N1,N2)
      END
