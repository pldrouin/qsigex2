      DOUBLE PRECISION FUNCTION LSQP2G(X,NR,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(NR)
      PARAMETER (BIG=700.D0,TWO=2.D0,ZERO=0.D0)
      BACK=X(1)+X(2)*T+X(3)*T**2
      ARG1=(X(5)-T)**2/(TWO*X(6)**2)
      ARG2=(X(8)-T)**2/(TWO*X(9)**2)
      IF(ARG1.GT.BIG) THEN
        GAUSS1=ZERO
      ELSE
        GAUSS1=X(4)*EXP(-ARG1)
      END IF
      IF(ARG2.GT.BIG) THEN
        GAUSS2=ZERO
      ELSE
        GAUSS2=X(7)*EXP(-ARG2)
      END IF
      LSQP2G=BACK+GAUSS1+GAUSS2
      END
