      DOUBLE PRECISION FUNCTION LSQGFN(ETA,X,N,NR,K)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(NR),ETA(N)
C computes constraint equations for fit of straight
C line to measurements with errors in abscissa and ordinate
      LSQGFN=ETA(2*K)-X(1)-X(2)*ETA(2*K-1)
      END
