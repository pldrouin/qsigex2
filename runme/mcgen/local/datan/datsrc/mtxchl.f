      SUBROUTINE MTXCHL(A,U,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(N,N),U(N,N)
      CALL MTXZER(U,N,N)
      DO 30 K=1,N
        S=0.
        DO 20 J=K,N
          IF(K.GT.1) THEN
            S=0.
            DO 10 L=1,K-1
              S=S+U(L,K)*U(L,J)
   10       CONTINUE
          END IF
          U(K,J)=A(K,J)-S
          IF(K.EQ.J) THEN
            U(K,J)=SQRT(ABS(U(K,J)))
          ELSE
            U(K,J)=U(K,J)/U(K,K)
          END IF
   20   CONTINUE
   30 CONTINUE
      END
