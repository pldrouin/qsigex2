      DOUBLE PRECISION FUNCTION LSQEXP(X,NR,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(NR)
      PARAMETER (BIG=700.D0,ZERO=0.D0)
      ARG=X(2)*T
      IF(ARG.GT.BIG) THEN
        LSQEXP=ZERO
      ELSE
        LSQEXP=X(1)*EXP(-ARG)
      END IF
      END
