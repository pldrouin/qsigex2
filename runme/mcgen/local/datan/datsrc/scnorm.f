      DOUBLE PRECISION FUNCTION SCNORM(X,X0,SIGMA)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      U=(X-X0)/SIGMA
      SCNORM=SCSTNR(U)
      END
