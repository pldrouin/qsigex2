      SUBROUTINE MTXMAT(A,B,R,M,L,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(L,M),B(L,N),R(M,N)
      DO 30 J=1,N
        DO 20 I=1,M
          R(I,J)=0.
          DO 10 LL=1,L
            R(I,J)=R(I,J)+A(LL,I)*B(LL,J)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      END
