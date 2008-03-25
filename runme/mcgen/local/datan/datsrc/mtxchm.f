      SUBROUTINE MTXCHM(U,A,R,M,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(M,M),A(M,N),R(M,N)
      PARAMETER(Z=0.D0)
      DO 30 I=1,M
        DO 20 K=1,N
          R(I,K)=Z
          DO 10 L=I,M
            R(I,K)=R(I,K)+U(I,L)*A(L,K)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      END
