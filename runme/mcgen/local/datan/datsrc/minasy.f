      SUBROUTINE MINASY(USERFN,N,NRED,LIST,X0,CX,FCONT,DXPLUS,
     +DXMINS,XR,NSTEP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=100)
      DIMENSION X(NMAX),DX(NMAX),LIST(N),XR(NRED,NRED+1)
      DIMENSION CX(NRED,NRED),DXPLUS(NRED),DXMINS(NRED)
      DIMENSION X0(N)
      PARAMETER(MAXSTP=1000)
      PARAMETER(EPS=1.D-3,EPSILN=1.D-4,TT=1.D-6,FACT=1.5D0)
      PARAMETER(ZERO=0.0D0,ONE=1.D0,HALF=0.5D0,TWO=2.D0,BIG=1.D20)
      LOGICAL LENCLS
      EXTERNAL USERFN
      IF(NSTEP.LE.0) NSTEP=MAXSTP
      IF(N.EQ.NRED) THEN
        DO 10 I=1,N
          LIST(I)=1
   10   CONTINUE
      END IF
      DXMIN=BIG
      DO 20 I=1,NRED
        DX(I)=SQRT(CX(I,I))
        DXMIN=MIN(DXMIN,DX(I))
   20 CONTINUE
      DXMIN=DBLE(.01)*DXMIN
      NRED1=NRED-1
      DO 60 IVAR=1,N
        IF(LIST(IVAR).EQ.0) GO TO 60
C fix variable IVAR
        LIST(IVAR)=0
        SAV=X0(IVAR)
        DO 50 ISIGN=-1,1,2
          SIGNUM=DBLE(ISIGN)
          DEL=DX(IVAR)
          LENCLS=.FALSE.
C set XSMALL to x at minimum position
          XSMALL=X0(IVAR)
          XIVAR=X0(IVAR)+SIGNUM*DEL
          DO 30 I=1,NSTEP
            NSTEPL=NSTEP
            CALL MTXCPV(X0,X,N)
            X(IVAR)=XIVAR
            EPSI=ZERO
            R1=DXMIN
            CALL MINSIM(X,N,NRED1,LIST,USERFN,R1,EPSI,NSTEPL,XR)
            IF(NSTEPL.LT.1) THEN
              NSTEP=-1
              GO TO 70
            END IF
C test for convergence
            IF(ABS(R1-FCONT).LT.EPS) THEN
              IF(ISIGN.LT.0) THEN
                DXMINS(IVAR)=X0(IVAR)-X(IVAR)
              ELSE
                DXPLUS(IVAR)=X(IVAR)-X0(IVAR)
              END IF
              GO TO 40
            ELSE
              IF(.NOT.LENCLS) THEN
C zero was not yet enclosed in last step
                IF(R1.GT.FCONT) THEN
C zero is now enclosed, perform first interval halving
                  LENCLS=.TRUE.
                  XBIG=X(IVAR)
                  X(IVAR)=HALF*(XBIG+XSMALL)
                ELSE
C zero not enclosed, widen range
                  DEL=DEL*TWO
                  X(IVAR)=XSMALL+SIGNUM*DEL
                END IF
              ELSE
C continue interval halving
                IF(R1.GT.FCONT) THEN
                  XBIG=X(IVAR)
                ELSE
                  XSMALL=X(IVAR)
                END IF
                X(IVAR)=HALF*(XBIG+XSMALL)
              END IF
              XIVAR=X(IVAR)
            END IF
   30     CONTINUE
          NSTEP=-2
          GO TO 70
   40     CONTINUE
   50   CONTINUE
C unfix variable IVAR
        LIST(IVAR)=1
   60 CONTINUE
   70 RETURN
      END
