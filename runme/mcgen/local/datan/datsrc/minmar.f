      SUBROUTINE MINMAR(X0,N,NRED,LIST,USERFN,FMIN,EPSILN,
     +NSTEP,HESSE)
      PARAMETER(MAXN=50)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X0(N),GRAD(MAXN),HESSE(NRED,NRED),X1(MAXN),
     +X2(MAXN),X1RED(MAXN),X2RED(MAXN)
      DIMENSION LIST(N)
      LOGICAL NEW,OK
      PARAMETER(MAXSTP=100,EPSDEF=1.D-8,T=1.D-15,ALAMS=1.D-3)
      PARAMETER(ZERO=0.D0,ONE=1.D0)
      EXTERNAL USERFN
      OK=.TRUE.
      IF(NSTEP.LT.1) NSTEP=MAXSTP
      EPS=EPSILN
      IF(EPS.LE.ZERO) EPS=EPSDEF
      ALAM=ALAMS
C initial value of minimum function
      FMINL=USERFN(X0,N)
      IF(NRED.LE.0) THEN
        FMIN=FMINL
        NSTEP=1
        GO TO 20
      END IF
C start iteration
      DO 10 ISTEP=1,NSTEP
        CALL AUXGRD(X0,GRAD,N,NRED,LIST,USERFN)
        CALL AUXHES(X0,HESSE,N,NRED,LIST,USERFN)
C compute minimum function for two values of lambda (ALAM)
        CALL MTXMAR(HESSE,GRAD,ALAM,X1RED,X2RED,NRED,NRED,ZERO,OK)
        IF(.NOT.OK) THEN
          NSTEP=-4
          GO TO 20
        END IF
        CALL MTXZRV(X1,N)
        CALL MTXZRV(X2,N)
        CALL MTXPSV(X1,X1RED,N,NRED,LIST)
        CALL MTXPSV(X2,X2RED,N,NRED,LIST)
        CALL MTXSBV(X0,X1,X1,N)
        CALL MTXSBV(X0,X2,X2,N)
        F1=USERFN(X1,N)
        F2=USERFN(X2,N)
C evaluate results
        IF(F2.LE.FMINL) THEN
C reduce lambda and accept new point
          ALAM=ALAM*0.1D0
          CALL MTXCPV(X2,X0,N)
          FMIN=F2
          NEW=.TRUE.
        ELSE IF (F2.GT.FMINL+T .AND. F1.LE.FMINL+T) THEN
C keep current value of lambda and accept new point
          CALL MTXCPV(X1,X0,N)
          FMIN=F1
          NEW=.TRUE.
        ELSE
C increase lambda and reject new point
          ALAM=ALAM*10.D0
          NEW=.FALSE.
        END IF
        IF(NEW) THEN
C test for break-off criterion
          IF(ABS(FMINL-FMIN) .LT. EPS*ABS(FMIN)+T) THEN
            NSTEP=ISTEP
            GO TO 20
          END IF
          FMINL=FMIN
        END IF
   10 CONTINUE
      NSTEP=-1
   20 CONTINUE
      END
