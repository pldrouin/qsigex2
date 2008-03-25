      SUBROUTINE MTXSV2(A,B,D,E,M,N,NB,OK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,N),B(M,NB),D(N),E(N)
      LOGICAL OK,ELZERO
      PARAMETER(ZERO=0.D0)
      OK=.TRUE.
      NITERM=10*N
      NITER=0
      BMX=D(1)
      IF(N.GT.1) THEN
        DO 10 I =  2, N
          BMX=MAX(ABS(D(I))+ABS(E(I)),BMX)
   10   CONTINUE
      END IF
      DO 60 K=N,1,-1
   20   CONTINUE
        IF(K.NE.1) THEN
          IF((BMX+D(K))-BMX.EQ.ZERO) THEN
C Since D(K).EQ.0. perform Givens transform with result E(K)=0.
            CALL MTXS21(A,D,E,M,N,NB,K)
          END IF
C Find L (2. LE. L .LE. K) so that either E(L)=0. or D(L-1)=0.
C In the latter case transform E(L) to zero. In both cases the
C matrix splits and the bottom right minor begins with row L.
C If no such L is found set L=1
          DO 30 LL=K,1,-1
            L=LL
            IF(L.EQ.1) THEN
              ELZERO=.FALSE.
              GO TO 40
            ELSE IF((BMX-E(L))-BMX.EQ.ZERO) THEN
              ELZERO=.TRUE.
              GO TO 40
            ELSE IF((BMX+D(L-1))-BMX.EQ.ZERO) THEN
              ELZERO=.FALSE.
            END IF
   30     CONTINUE
   40     IF (L.GT. 1.AND. .NOT.ELZERO) THEN
            CALL MTXS22(B,D,E,M,N,NB,K,L)
          END IF
          IF(L.NE.K) THEN
C one more QR pass with order K
            CALL MTXS23(A,B,D,E,M,N,NB,K,L)
            NITER=NITER+1
            IF(NITER.LE.NITERM) GO TO 20
C set flag indicating non-convergence
            OK=.FALSE.
          END IF
        END IF
        IF(D(K).LT.0.) THEN
C for negative singular values perform change of sign
          D(K)=-D(K)
          DO 50 J=1,N
            A(J,K)=-A(J,K)
   50     CONTINUE
        END IF
C order is decreased by one in next pass
   60 CONTINUE
      END
C-----------------------------------------------------------------
      SUBROUTINE MTXS21(A,D,E,M,N,NB,K)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,N),D(N),E(N)
      DO 20 I = K- 1,  1, -1
        IF(I.EQ.K-1) THEN
          CALL MTXGVA(D(I),E(I+1),CS,SN)
        ELSE
          CALL MTXGVA(D(I),H,CS,SN)
        END IF
        IF(I.GT.1) THEN
          H=0.
          CALL MTXGVT(E(I),H,CS,SN)
        END IF
        DO 10 J =  1,N
          CALL MTXGVT(A(J,I),A(J,K),CS,SN)
   10   CONTINUE
   20 CONTINUE
      END
C-----------------------------------------------------------------
      SUBROUTINE MTXS22(B,D,E,M,N,NB,K,L)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION B(M,NB),D(N),E(N)
      DO 20 I=L,K
        IF(I.EQ.L) THEN
          CALL MTXGVA(D(I),E(I),CS,SN)
        ELSE
          CALL MTXGVA(D(I),H,CS,SN)
        END IF
        IF(I.LT.K) THEN
          H=0.
          CALL MTXGVT(E(I+1),H,CS,SN)
        END IF
        DO 10 J=1,NB
          CALL MTXGVT(CS,SN,B(I,J),B(L-1,J))
   10   CONTINUE
   20 CONTINUE
      END
C-----------------------------------------------------------------
      SUBROUTINE MTXS23(A,B,D,E,M,N,NB,K,L)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,N),B(M,NB),D(N),E(N)
C Determine shift parameter
      F=((D(K-1)-D(K))*(D(K-1)+D(K))+(E(K-1)-E(K))*(E(K-1)+E(K)))/
     +  (2.*E(K)*D(K-1))
      IF(ABS(F).GT.1.E10) THEN
        G=ABS(F)
      ELSE
        G=SQRT(1.+F*F)
      END IF
      IF(F.GE.0.) THEN
        T=F+G
      ELSE
        T=F-G
      END IF
      F=((D(L)-D(K))*(D(L)+D(K))+E(K)*(D(K-1)/T-E(K)))/D(L)
      DO 30 I = L , K-1
        IF(I.EQ.L) THEN
C Define R(L)
          CALL MTXGVD(F,E(I+1),CS,SN)
        ELSE
C Define R(I) , I.NE.L
          CALL MTXGVA(E(I),H,CS,SN)
        END IF
        CALL MTXGVT(D(I),E(I+1),CS,SN)
        H=0.
        CALL MTXGVT(H,D(I+1),CS,SN)
        DO 10 J =  1, N
          CALL MTXGVT(A(J,I),A(J,I+1),CS,SN)
   10   CONTINUE
C Define T(I)
        CALL MTXGVA(D(I),H,CS,SN)
        CALL MTXGVT(E(I+1),D(I+1),CS,SN)
        IF(I.LT.K-1) THEN
          H=0.
          CALL MTXGVT(H,E(I+2),CS,SN)
        END IF
        DO 20 J =  1, NB
          CALL MTXGVT(B(I,J),B(I+1,J),CS,SN)
   20   CONTINUE
   30 CONTINUE
      END
