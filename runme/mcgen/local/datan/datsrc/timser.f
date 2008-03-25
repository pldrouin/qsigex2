      SUBROUTINE TIMSER(Y,N,K,L,P,ETA,CONETA,A,ATA1,ATA1AT,SCRAT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Y(N),ETA(N+2*K),CONETA(N+2*K),A(2*K+1,L+1)
      DIMENSION ATA1(L+1,L+1),ATA1AT(L+1,2*K+1),SCRAT(L+1,L+1)
      PARAMETER (MAX=1000, ZERO=0.D0, ONE=1.D0, HALF=0.5D0)
      DIMENSION X(MAX),YTMP(MAX),ETATMP(MAX),T(MAX)
      COMMON /DADV03/ X,YTMP,ETATMP,T
C quantile of Student's distribution
      PPRIME=HALF*(P+1)
      NF=2*K-L
      TALPHA=SQSTUD(PPRIME,NF)
C compute matrices depending only on K and L
      K21=2*K+1
      L1=L+1
      DO 20 I=1,K21
        DO 10 J=1,L1
          IF(J.EQ.1) THEN
            A(I,J)=-ONE
          ELSE
            A(I,J)=A(I,J-1)*DBLE(I-K-1)
          END IF
   10   CONTINUE
   20 CONTINUE
      CALL MTXMAT(A,A,ATA1,L1,K21,L1)
      CALL MTXCHI(ATA1,SCRAT,L1)
      CALL MTXMBT(ATA1,A,ATA1AT,L1,L1,K21)
      CALL MTXMSC(ATA1AT,ATA1AT,-ONE,L1,K21)
C loop over inner part of time series
      IA=2*K+1
      IB=N
      DO 60 I=IA,IB
C moving averages and confidence limits for inner part
        CALL MTXGSM(Y,YTMP,N,1,K21,1,I-IA+1,1)
        CALL MTXMLT(ATA1AT,YTMP,X,L1,K21,1)
        ETA(I)=X(1)
        CALL MTXMLT(A,X,ETATMP,K21,L1,1)
        CALL MTXADD(YTMP,ETATMP,ETATMP,K21,1)
        CALL MTXMAT(ETATMP,ETATMP,SY2,1,K21,1)
        SY2=SY2/DBLE(NF)
        A0=SQRT(ABS(ATA1(1,1)))
        CONETA(I)=A0*SQRT(SY2)*TALPHA
C moving averages and confidence limits for end sections
        IF(I.EQ.IA .OR. I.EQ.IB) THEN
          IF(I.EQ.IA) THEN
            IADD=IA
            IS=-1
          ELSE
            IADD=IB
            IS=1
          END IF
          DO 50 I1=1,2*K
            J=IS*I1
            DO 40 I2=1,L1
              DO 30 I3=1,I2
                IF(I3.EQ.1) THEN
                  T(I2)=ONE
                ELSE
                  T(I2)=T(I2)*J
                END IF
   30         CONTINUE
   40       CONTINUE
            CALL MTXMBT(ATA1,T,SCRAT,L1,L1,1)
            CALL MTXMLT(T,SCRAT,SETA2,1,L1,1)
            SETA2=SY2*SETA2
            CALL MTXMLT(T,X,ETAI,1,L1,1)
            CONETA(IADD+J)=SQRT(ABS(SETA2))*TALPHA
            ETA(IADD+J)=ETAI
   50     CONTINUE
        END IF
   60 CONTINUE
      END
