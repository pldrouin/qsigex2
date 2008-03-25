      SUBROUTINE LSQLIN(T,C,DELTAY,N,NR,X,CX,R,A,SCRAT,OK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N,NR),T(N),C(N),DELTAY(N),X(NR),CX(NR,NR)
      DIMENSION SCRAT(NR,NR)
      LOGICAL OK
C compute matrix A' from A
      DO 20 I=1,N
        DO 10 K=1,NR
          A(I,K)=A(I,K)/DELTAY(I)
   10   CONTINUE
   20 CONTINUE
C Set up vector C'
      DO 30 I=1,N
        C(I)=C(I)/DELTAY(I)
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
