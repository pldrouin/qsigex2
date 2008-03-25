      SUBROUTINE MTXDOT(U,V,S,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(N),V(N)
      S=0.
      DO 10 I=1,N
        S=S+U(I)*V(I)
   10 CONTINUE
      END
