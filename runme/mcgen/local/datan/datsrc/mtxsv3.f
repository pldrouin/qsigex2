      SUBROUTINE MTXSV3(A,B,D,M,N,NB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,N),B(M,NB),D(N)
C Order singular values
      IF(N.GT.1) THEN
   10   DO 20 I=2,N
          IF(D(I).GT.D(I-1)) GO TO 30
   20   CONTINUE
        RETURN
   30   CONTINUE
        DO 70 I =  2, N
          T=D(I-1)
          K=I-1
          DO 40 J = I , N
            IF(T.LT.D(J)) THEN
              T=D(J)
              K=J
            END IF
   40     CONTINUE
          IF(K.NE.I-1) THEN
C perform permutation on singular values
            D(K)=D(I-1)
            D(I-1)=T
C perform permutation on matrix A
            DO 50 J =  1, N
              T=A(J,K)
              A(J,K)=A(J,I-1)
              A(J,I-1)=T
   50       CONTINUE
C perform permutation on matrix B
            DO 60 J =  1, NB
              T=B(K,J)
              B(K,J)=B(I-1,J)
              B(I-1,J)=T
   60       CONTINUE
          END IF
   70   CONTINUE
        GO TO 10
      END IF
      END
