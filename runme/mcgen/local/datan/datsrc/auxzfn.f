      SUBROUTINE AUXZFN(X0,X1,XZERO,FUNCT,PAR,NPAR1,NPAR2,EPSILN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ZERO=0.D0, HALF=0.5D0)
      EXTERNAL FUNCT
      XZERO=X0
      DO 10 I=1,2000
        F0=FUNCT(X0,PAR,NPAR1,NPAR2)
        F1=FUNCT(X1,PAR,NPAR1,NPAR2)
        IF(F0.EQ.ZERO) THEN
          XZERO=X0
          GO TO 20
        ELSE IF(F1.EQ.ZERO) THEN
          XZERO=X1
          GO TO 20
        END IF
        XM=HALF*(X0+X1)
        IF(ABS(X0-X1).GE.EPSILN) THEN
          FM=FUNCT(XM,PAR,NPAR1,NPAR2)
          TEST=F0*FM
          IF(TEST .LT. ZERO) THEN
            X1=XM
          ELSE
            X0=XM
          END IF
        ELSE
          XZERO=XM
          GO TO 20
        END IF
   10 CONTINUE
   20 CONTINUE
      END
