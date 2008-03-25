      SUBROUTINE MTXMBT(A,B,R,M,L,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,L),B(N,L),R(M,N)
      DO 30 J=1,N
        DO 20 I=1,M
          R(I,J)=0.
          DO 10 LL=1,L
            R(I,J)=R(I,J)+A(I,LL)*B(J,LL)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      END
