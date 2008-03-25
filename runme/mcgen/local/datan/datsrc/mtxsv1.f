      SUBROUTINE MTXSV1(A,B,D,E,M,N,NB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(M,N),B(M,NB),V(NMAX),S(NMAX),UPS(NMAX),D(N),E(N)
      DIMENSION BBS(NMAX)
      COMMON /DASV00/ V,S,UPS,BBS
      DO 50 I=1,N
C set up Householder Transformation Q(I)
        IF(I.LT.N .OR. M.GT.N) THEN
          CALL MTXGCL(A,V,M,N,I)
          CALL MTXHSD(V,UP,BB,M,I,I+1)
C apply Q(I) to A
          DO 10 J=I,N
            CALL MTXGCL(A,S,M,N,J)
            CALL MTXHST(V,UP,BB,S,M,I,I+1)
            CALL MTXPCL(A,S,M,N,J)
   10     CONTINUE
C apply Q(I) to B
          DO 20 K=1,NB
            CALL MTXGCL(B,S,M,NB,K)
            CALL MTXHST(V,UP,BB,S,M,I,I+1)
            CALL MTXPCL(B,S,M,NB,K)
   20     CONTINUE
        END IF
        IF(I.LT.N-1) THEN
C set up Householder Transformation H(I)
          CALL MTXGRW(A,V,M,N,I)
          CALL MTXHSD(V,UP,BB,N,I+1,I+2)
C save H(I)
          UPS(I)=UP
          BBS(I)=BB
C apply H(I) to A
          DO 40 J=I,M
            CALL MTXGRW(A,S,M,N,J)
            CALL MTXHST(V,UP,BB,S,N,I+1,I+2)
C save elements I+2,... in row J of matrix A
            IF (J.EQ.I) THEN
              DO 30 K=I+2,N
                S(K)=V(K)
   30         CONTINUE
            END IF
            CALL MTXPRW(A,S,M,N,J)
   40     CONTINUE
        END IF
   50 CONTINUE
C copy diagonal of transformed matrix A to D
C and upper parallel A to E
      IF(N.GT.1) THEN
        DO 60 I=2,N
          D(I)=A(I,I)
          E(I)=A(I-1,I)
   60   CONTINUE
      END IF
      D(1)=A(1,1)
      E(1)=0.
C construct product matrix H=H(2)*H(3)*...*H(N), H(N)=I
      DO 90 I=N,1,-1
        IF(I.LE.N-1) CALL MTXGRW(A,V,M,N,I)
        DO 70 K=1,N
          A(I,K)=0.
   70   CONTINUE
        A(I,I)=1.
        IF(I.LT.N-1) THEN
          DO 80 K=I,N
            CALL MTXGCL(A,S,M,N,K)
            CALL MTXHST(V,UPS(I),BBS(I),S,N,I+1,I+2)
            CALL MTXPCL(A,S,M,N,K)
   80     CONTINUE
        END IF
   90 CONTINUE
      END
