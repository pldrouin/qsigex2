      DOUBLE PRECISION FUNCTION SQPOIS(K,P)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(ZERO=0.D0,ONE=1.D0,BIG=1.D10,EPSILN=1.D-8)
      EXTERNAL SZPOIS
C boundary of range
      IF(P.GE.ONE) SQPOIS=BIG
      IF(P.LE.ZERO) SQPOIS=ZERO
C normal range
      IF(P.LT.ONE .AND. P.GT.ZERO) THEN
        X0=ZERO
        X1=DBLE(K)
        CALL AUXZBR(X0,X1,SZPOIS,P,K,0)
        CALL AUXZFN(X0,X1,XZERO,SZPOIS,P,K,0,EPSILN)
        SQPOIS=XZERO
      END IF
      END
C-----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SZPOIS(ALAMBD,P,K,NDUM2)
C returns P minus cumulative Poisson
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SZPOIS=P-SCPOIS(K,ALAMBD)
      END
