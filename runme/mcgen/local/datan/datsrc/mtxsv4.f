      SUBROUTINE MTXSV4(A,B,D,X,R,M,N,NB,FRAC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,N),B(M,NB),D(N),X(N,NB),R(NB)
      PARAMETER (EPSILN=1.D-15)
      FRACT=ABS(FRAC)
      IF(FRACT.LT.EPSILN) FRACT=EPSILN
      SINMAX=0.
      DO 10 I =  1, N
        SINMAX=MAX(SINMAX,D(I))
   10 CONTINUE
      SINMIN=SINMAX*FRACT
      KK=N
      DO 20 I=1,N
        IF(D(I).LE.SINMIN) THEN
          KK=I-1
          GO TO 30
        END IF
   20 CONTINUE
   30 CONTINUE
      DO 60 I =  1, M
        IF(I.LE.KK) THEN
          S1=1./D(I)
          DO 40 J=1,NB
            B(I,J)=B(I,J)*S1
   40     CONTINUE
        ELSE
          DO 50 J=1,NB
            IF(I.EQ.KK+1) THEN
              R(J)=B(I,J)**2
            ELSE
              R(J)=R(J)+B(I,J)**2
            END IF
            IF(I.LE.N) B(I,J)=0.
   50     CONTINUE
        END IF
   60 CONTINUE
      DO 90 I=1,N
        DO 80 J=1,NB
          X(I,J)=0.
          DO 70 K=1,N
            X(I,J)=X(I,J)+A(I,K)*B(K,J)
   70     CONTINUE
   80   CONTINUE
   90 CONTINUE
      N=KK
      END
