      SUBROUTINE MTXSVD(A,B,X,R,M,N,NB,FRAC,OK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(M,N),B(M,NB),X(N,NB),R(NB),D(NMAX),E(NMAX)
      COMMON /DASV01/ D,E
      LOGICAL OK
C STEP 1: Bidiagonalisation of A
      CALL MTXSV1(A,B,D,E,M,N,NB)
C STEP 2: Diagonalisation of bidiagonal matrix
      CALL MTXSV2(A,B,D,E,M,N,NB,OK)
C STEP 3: Order singular values and perform  permutations
      CALL MTXSV3(A,B,D,M,N,NB)
C STEP 4: Singular value analysis
      CALL MTXSV4(A,B,D,X,R,M,N,NB,FRAC)
      END
