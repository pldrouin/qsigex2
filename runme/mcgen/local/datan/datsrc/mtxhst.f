      SUBROUTINE MTXHST(V,UP,B,C,N,LP,L)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(N),C(N)
      DUP=UP
      S=C(LP)*DUP
      DO 10 I=L,N
        S=S+C(I)*V(I)
   10 CONTINUE
      S=S*B
      C(LP)=C(LP)+S*DUP
      DO 20 I=L,N
        C(I)=C(I)+S*V(I)
   20 CONTINUE
      END
