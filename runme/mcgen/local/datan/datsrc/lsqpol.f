      SUBROUTINE LSQPOL(T,Y,DELTAY,N,NR,X,CX,R,A,SCRAT,OK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(N,NR),T(N),Y(N),DELTAY(N),X(NR),CX(NR,NR)
      DIMENSION SCRAT(NR,NR),C(NMAX)
      COMMON /DASV04/ C
      LOGICAL OK
      PARAMETER(ONE=1.D0)
C Set up matrix A'
      DO 20 I=1,N
        DO 10 K=1,NR
          IF(K.EQ.1) THEN
            A(I,K)=-ONE/DELTAY(I)
          ELSE
            A(I,K)=-(T(I)**(K-1))/DELTAY(I)
          END IF
   10   CONTINUE
   20 CONTINUE
C Set up vector C'
      DO 30 I=1,N
        C(I)=-Y(I)/DELTAY(I)
   30 CONTINUE
C Set up matrix GX
      CALL MTXMAT(A,A,CX,NR,N,NR)
C Determine vector X of unknowns
      CALL MTXSVD(A,C,X,R,N,NR,1,DBLE(0.),OK)
      IF(OK) THEN
C Determine covariance matrix CX by inverting GX
        CALL MTXCHI(CX,SCRAT,NR)
      END IF
      END
