      SUBROUTINE LSQMAR(USERFN,T,Y,DELTAY,N,NR,NRED,LIST,X,CX,R,A,
     +SCRAT,NSTEP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(N,NRED),T(N),Y(N),DELTAY(N),X(NR)
      DIMENSION SCRAT(NRED,NRED),CX(NRED,NRED)
      DIMENSION C(NMAX),X1(NMAX),X2(NMAX),X1RED(NMAX),X2RED(NMAX)
      DIMENSION LIST(NR)
      COMMON /DASV02/ C,X1,X2,X1RED,X2RED
      PARAMETER(MAXSTP=100,EPSILN=1.D-8,TT=1.D-15,ALAMS=1.D-3)
      LOGICAL NEW,OK,FINAL,COVMAT
      EXTERNAL USERFN
      COVMAT=.TRUE.
      IF(NSTEP.LT.0) THEN
        COVMAT=.FALSE.
        NSTEP=ABS(NSTEP)
      END IF
      IF(NSTEP.LT.1) NSTEP=MAXSTP
      ALAM=ALAMS
C Initial value of minimum function
      R=DBLE(0.)
      DO 10 I=1,N
        R=R+((Y(I)-USERFN(X,NR,T(I)))/DELTAY(I))**2
   10 CONTINUE
      IF(NRED.LE.0) THEN
        NSTEP=1
        GO TO 100
      END IF
C For NR=NRED :  set LIST
      IF(NR.EQ.NRED) THEN
        DO 20 I=1,NR
          LIST(I)=1
   20   CONTINUE
      END IF
C start iteration
      FINAL=.FALSE.
      DO 90 ISTEP=1,NSTEP
C Numerical Derivatives
        CALL AUXDRI(USERFN,X,T,N,NR,NRED,LIST,A,OK)
        IF(.NOT.OK) THEN
          NSTEP=-3
          GO TO 100
        END IF
        DO 40 I=1,N
          DO 30 K=1,NRED
            A(I,K)=A(I,K)/DELTAY(I)
   30     CONTINUE
   40   CONTINUE
        DO 50 I=1,N
          C(I)=(Y(I)-USERFN(X,NR,T(I)))/DELTAY(I)
   50   CONTINUE
        IF(FINAL) THEN
C Final Step
          CALL MTXSVD(A,C,X1RED,R,N,NRED,1,DBLE(0.),OK)
          IF(.NOT.OK) THEN
            NSTEP=-1
            GO TO 100
          END IF
          CALL MTXZRV(X1,NR)
          CALL MTXPSV(X1,X1RED,NR,NRED,LIST)
          CALL MTXADV(X,X1,X,NR)
C Compute covariance matrix CX of unknowns
          IF(COVMAT) THEN
            CALL AUXDRI(USERFN,X,T,N,NR,NRED,LIST,A,OK)
            IF(.NOT.OK) THEN
              NSTEP=-3
              GO TO 100
            END IF
            DO 70 I=1,N
              DO 60 K=1,NRED
                A(I,K)=A(I,K)/DELTAY(I)
   60         CONTINUE
   70       CONTINUE
            CALL MTXMAT(A,A,CX,NRED,N,NRED)
            CALL MTXCHI(CX,SCRAT,NRED)
          END IF
          NSTEP=ISTEP
          GO TO 100
        END IF
C compute minimum function for two values of lambda (ALAM)
        CALL MTXMAR(A,C,ALAM,X1RED,X2RED,N,NRED,DBLE(0.),OK)
        IF(.NOT.OK) THEN
          NSTEP=-1
          GO TO 100
        END IF
        CALL MTXZRV(X1,NR)
        CALL MTXZRV(X2,NR)
        CALL MTXPSV(X1,X1RED,NR,NRED,LIST)
        CALL MTXPSV(X2,X2RED,NR,NRED,LIST)
        CALL MTXADV(X1,X,X1,NR)
        CALL MTXADV(X2,X,X2,NR)
        R1=DBLE(0.)
        R2=DBLE(0.)
        DO 80 I=1,N
          R1=R1+((Y(I)-USERFN(X1,NR,T(I)))/DELTAY(I))**2
          R2=R2+((Y(I)-USERFN(X2,NR,T(I)))/DELTAY(I))**2
   80   CONTINUE
        IF(.NOT.OK) THEN
          NSTEP=-1
          GO TO 100
        END IF
C evaluate results
        IF(R2.LE.R+TT) THEN
C reduce lambda and accept new point
          ALAM=ALAM*DBLE(0.1)
          CALL MTXCPV(X2,X,NR)
          RMIN=R2
          NEW=.TRUE.
        ELSE IF (R2.GT.R+TT .AND. R1.LE.R+TT) THEN
C keep current value of lambda and accept new point
          CALL MTXCPV(X1,X,NR)
          RMIN=R1
          NEW=.TRUE.
        ELSE
C increase lambda and reject new point
          ALAM=ALAM*DBLE(10.)
          NEW=.FALSE.
        END IF
        IF(NEW) THEN
C test for break-off criterion
          IF(ABS(R-RMIN) .LT. EPSILN*ABS(RMIN)+TT) THEN
            FINAL=.TRUE.
          ELSE
            R=RMIN
          END IF
        END IF
   90 CONTINUE
      NSTEP=-2
  100 RETURN
      END
