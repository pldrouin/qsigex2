      SUBROUTINE MTXSUB(A,B,R,M,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,N),B(M,N),R(M,N)
      DO 20 J=1,N
        DO 10 I=1,M
          R(I,J)=A(I,J)-B(I,J)
   10   CONTINUE
   20 CONTINUE
      END
