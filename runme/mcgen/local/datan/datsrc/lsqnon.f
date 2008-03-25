      SUBROUTINE LSQNON(USERFN,T,Y,DELTAY,N,NR,NRED,LIST,X,CX,R,A,
     +SCRAT,NSTEP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(N,NRED),T(N),Y(N),DELTAY(N),X(NR)
      DIMENSION SCRAT(NRED,NRED),CX(NRED,NRED),C(NMAX)
      DIMENSION X2(NMAX),X1RED(NMAX)
      DIMENSION LIST(NR)
      COMMON /DASV02/ C,X2,X1RED
      PARAMETER(MAXSTP=100,EPSILN=1.D-8,TT=1.D-15,FACT=0.5D0)
      PARAMETER(ZERO=0.0D0)
      LOGICAL OK,COVMAT
      EXTERNAL USERFN
      COVMAT=.TRUE.
      IF(NSTEP.LT.0) THEN
        COVMAT=.FALSE.
        NSTEP=ABS(NSTEP)
      END IF
      IF(NSTEP.LT.1) NSTEP=MAXSTP
C Initial value of minimum function
      R=ZERO
      DO 10 I=1,N
        R=R+((Y(I)-USERFN(X,NR,T(I)))/DELTAY(I))**2
   10 CONTINUE
C For NRED=0 : set NSTEP=1 and return
      IF(NRED.LE.0) THEN
        NSTEP=1
        GO TO 130
      END IF
C For NR=NRED :  set LIST
      IF(NR.EQ.NRED) THEN
        DO 20 I=1,NR
          LIST(I)=1
   20   CONTINUE
      END IF
C start iteration
      DO 120 ISTEP=1,NSTEP
C Numerical Derivatives
        CALL AUXDRI(USERFN,X,T,N,NR,NRED,LIST,A,OK)
        IF(.NOT.OK) THEN
          NSTEP=-3
          GO TO 130
        END IF
        DO 40 I=1,N
          DO 30 K=1,NRED
            A(I,K)=A(I,K)/DELTAY(I)
   30     CONTINUE
   40   CONTINUE
        DO 50 I=1,N
          C(I)=(Y(I)-USERFN(X,NR,T(I)))/DELTAY(I)
   50   CONTINUE
        CALL MTXSVD(A,C,X1RED,R1,N,NRED,1,DBLE(0.),OK)
        IF(.NOT.OK) THEN
          NSTEP=-1
          GO TO 130
        END IF
        CALL MTXZRV(X2,NR)
        CALL MTXPSV(X2,X1RED,NR,NRED,LIST)
        CALL MTXADV(X,X2,X2,NR)
        R1=ZERO
        DO 60 I=1,N
          R1=R1+((Y(I)-USERFN(X2,NR,T(I)))/DELTAY(I))**2
   60   CONTINUE
        DO 80 K=1,30
          IF(R1.GT.R*(DBLE(1.)+EPSILN)+TT) THEN
C minimum function has increased: reduce step by factor of two
            CALL MTXMSV(X1RED,X1RED,FACT,NRED)
            CALL MTXZRV(X2,NR)
            CALL MTXPSV(X2,X1RED,NR,NRED,LIST)
            CALL MTXADV(X,X2,X2,NR)
            R1=ZERO
            DO 70 I=1,N
              R1=R1+((Y(I)-USERFN(X2,NR,T(I)))/DELTAY(I))**2
   70       CONTINUE
          ELSE
            GO TO 90
          END IF
   80   CONTINUE
        NSTEP=-4
        GO TO 130
   90   CONTINUE
        CALL MTXZRV(X2,NR)
        CALL MTXPSV(X2,X1RED,NR,NRED,LIST)
        CALL MTXADV(X,X2,X,NR)
C test for break-off criterion
        IF(ABS(R-R1) .LT. EPSILN*ABS(R)+TT) THEN
          R=R1
C Compute covariance matrix CX of unknowns
          IF(COVMAT) THEN
            CALL AUXDRI(USERFN,X,T,N,NR,NRED,LIST,A,OK)
            IF(.NOT.OK) THEN
              NSTEP=-3
              GO TO 130
            END IF
            DO 110 I=1,N
              DO 100 K=1,NRED
                A(I,K)=A(I,K)/DELTAY(I)
  100         CONTINUE
  110       CONTINUE
            CALL MTXMAT(A,A,CX,NRED,N,NRED)
            CALL MTXCHI(CX,SCRAT,NRED)
            NSTEP=ISTEP
          END IF
          GO TO 130
        ELSE
          R=R1
        END IF
  120 CONTINUE
      NSTEP=-2
  130 RETURN
      END
