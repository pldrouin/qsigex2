      SUBROUTINE MTXEQU(A,B,N,M)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N,N),B(N,M)
      DO 70 K=1,N-1
        AMAX=0.
        KK=K
        DO 10 L=K,N
          IF(ABS(AMAX).LT.ABS(A(L,K))) THEN
            AMAX=A(L,K)
            KK=L
          END IF
   10   CONTINUE
        IF(KK.NE.K) THEN
          DO 20 J=K,N
            SAVE=A(K,J)
            A(K,J)=A(KK,J)
            A(KK,J)=SAVE
   20     CONTINUE
          DO 30 I=1,M
            SAVE=B(K,I)
            B(K,I)=B(KK,I)
            B(KK,I)=SAVE
   30     CONTINUE
        END IF
        DO 60 I=K+1,N
          DO 40 J=K+1,N
            A(I,J)=A(I,J)-A(K,J)*A(I,K)/A(K,K)
   40     CONTINUE
          DO 50 J=1,M
            B(I,J)=B(I,J)-B(K,J)*A(I,K)/A(K,K)
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE
      DO 100 J=1,M
        B(N,J)=B(N,J)/A(N,N)
        IF(N.GT.1) THEN
          DO 90 I1=1,N-1
            I=N-I1
            DO 80 L=I+1,N
              B(I,J)=B(I,J)-A(I,L)*B(L,J)
   80       CONTINUE
            B(I,J)=B(I,J)/A(I,I)
   90     CONTINUE
        END IF
  100 CONTINUE
      END
