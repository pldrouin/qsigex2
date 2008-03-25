      SUBROUTINE MTXCHI(A,U,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N,N),U(N,N)
C Step 1: Cholesky decomposition
      CALL MTXCHL(A,U,N)
      DO 50 I=1,N
C Step 2: Forward Substitution
        DO 20 L=I,N
          IF(L.EQ.I) THEN
            A(N,L)=DBLE(1.)/U(L,L)
          ELSE
            A(N,L)=DBLE(0.)
            DO 10 K=I,L-1
              A(N,L)=A(N,L)-U(K,L)*A(N,K)
   10       CONTINUE
            A(N,L)=A(N,L)/U(L,L)
          END IF
   20   CONTINUE
C Step 3: Back Substitution
        DO 40 L=N,I,-1
          IF(L.EQ.N) THEN
            A(I,L)=A(N,L)/U(L,L)
          ELSE
            A(I,L)=A(N,L)
            DO 30 K=N,L+1,-1
              A(I,L)=A(I,L)-U(L,K)*A(I,K)
   30       CONTINUE
            A(I,L)=A(I,L)/U(L,L)
          END IF
   40   CONTINUE
   50 CONTINUE
C Fill lower triangle symmetrically
      IF(N.GT.1) THEN
        DO 70 I=1,N
          DO 60 L=1,I-1
            A(I,L)=A(L,I)
   60     CONTINUE
   70   CONTINUE
      END IF
      END
