      SUBROUTINE AUXZBR(X0,X1,FUNCT,PAR,NPAR1,NPAR2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0)
      EXTERNAL FUNCT
      IF(X0.EQ.X1) X1=X0+ONE
      F0=FUNCT(X0,PAR,NPAR1,NPAR2)
      F1=FUNCT(X1,PAR,NPAR1,NPAR2)
      DO 10 I=1,1000
        IF(F0*F1 .GT. ZERO) THEN
          IF(ABS(F0).LE.ABS(F1)) THEN
            XS=X0
            X0=X0+TWO*(X0-X1)
            X1=XS
            F1=F0
            F0=FUNCT(X0,PAR,NPAR1,NPAR2)
          ELSE
            XS=X1
            X1=X1+TWO*(X1-X0)
            X0=XS
            F0=F1
            F1=FUNCT(X1,PAR,NPAR1,NPAR2)
          END IF
        ELSE
          GO TO 20
        END IF
   10 CONTINUE
   20 CONTINUE
      END
