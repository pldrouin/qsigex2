      SUBROUTINE MTXDEC(A,B,X,R,M,N,FRAC,OK,D,U,V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(M,N),B(M,M),X(N,M),R(M)
      DIMENSION D(N),E(NMAX)
      DIMENSION U(M,M),V(N,N)
      COMMON /DASV04/ E
      LOGICAL OK
C STEP 0: Set B equal to (N x N) unit matrix
      CALL MTXUNT(B,M)
C STEP 1: Bidiagonalisation of A
      CALL MTXSV1(A,B,D,E,M,N,M)
C STEP 2: Diagonalisation of bidiagonal matrix
      CALL MTXSV2(A,B,D,E,M,N,M,OK)
C STEP 3: Order singular values and perform permutations
      CALL MTXSV3(A,B,D,M,N,M)
      CALL MTXGSM(A,V,M,N,N,N,1,1)
      CALL MTXTRP(B,U,M,M)
C STEP 4: Singular value analysis
      CALL MTXSV4(A,B,D,X,R,M,N,M,FRAC)
      END
