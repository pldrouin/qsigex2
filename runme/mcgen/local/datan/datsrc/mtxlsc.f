      SUBROUTINE MTXLSC(A,B,E,D,X,R,A2,M,N,L,FRAC,OK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(M,N),B(M),E(L,N),D(L),X(N)
      LOGICAL OK
      PARAMETER(LMAX=1050,NMAX=1000)
      DIMENSION UP(LMAX),BB(LMAX),A2(M,*),P2(NMAX),S(NMAX),V(NMAX)
      COMMON /DASV02/ UP,BB,P2,S,V
C step 1
      NMINL=N-L
      DO 40 I=1,L
        CALL MTXGRW(E,V,L,N,I)
        CALL MTXHSD(V,UP(I),BB(I),N,I,I+1)
        DO 20 J=I,L
          CALL MTXGRW(E,S,L,N,J)
          CALL MTXHST(V,UP(I),BB(I),S,N,I,I+1)
          IF(J.EQ.I .AND. N.GT.I) THEN
            DO 10 K=I+1,N
              S(K)=V(K)
   10       CONTINUE
          END IF
          CALL MTXPRW(E,S,L,N,J)
   20   CONTINUE
        DO 30 J=1,M
          CALL MTXGRW(A,S,M,N,J)
          CALL MTXHST(V,UP(I),BB(I),S,N,I,I+1)
          CALL MTXPRW(A,S,M,N,J)
   30   CONTINUE
   40 CONTINUE
C step 2
      X(1)=D(1)/E(1,1)
      IF(L.GT.1) THEN
        DO 60 J=2,L
          X(J)=D(J)
          DO 50 K=1,J-1
            X(J)=X(J)-E(J,K)*X(K)
   50     CONTINUE
          X(J)=X(J)/E(J,J)
   60   CONTINUE
      END IF
C step 3
      DO 80 J=1,M
        DO 70 K=1,L
          B(J)=B(J)-A(J,K)*X(K)
   70   CONTINUE
   80 CONTINUE
C step 4
      L2=1
      CALL MTXGSM(A,A2,M,N,M,NMINL,1,L+1)
      CALL MTXSVD(A2,B,P2,R,M,NMINL,L2,FRAC,OK)
      IF(OK) THEN
        CALL MTXPSM(X,P2,N,1,NMINL,1,L+1,1)
        DO 90 I=L,1,-1
          CALL MTXGRW(E,V,L,N,I)
          CALL MTXHST(V,UP(I),BB(I),X,N,I,I+1)
   90   CONTINUE
      END IF
      END
