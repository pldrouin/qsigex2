      SUBROUTINE LSQASG(Y,CY,FY,F,E,M,N,NR,NRED,LIST,X0,CX,R,W,
     +DXPLUS,DXMINS,A2,NSTEP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION Y(N),CY(N,N),X0(NR),CX(NRED,NRED),LIST(NR)
      DIMENSION E(M,N+NRED),FY(N,N),F(N+NRED,N+NRED),A2(N+NRED,*)
      DIMENSION X(NMAX),DXPLUS(NRED),DXMINS(NRED),DX(NMAX)
      DIMENSION YSAV(NMAX)
      COMMON /DASV04/ X,DX,YSAV
      PARAMETER(MAXSTP=100,EPS=1.D-2,EPSILN=1.D-4,TT=1.D-6,
     +FACT=1.5D0)
      PARAMETER(ZERO=0.0D0,ONE=1.D0,HALF=0.5D0,TWO=2.D0)
      LOGICAL LENCLS
      IF(NSTEP.LE.0) NSTEP=MAXSTP
      IF(NR.EQ.NRED) THEN
        DO 10 I=1,NR
          LIST(I)=1
   10   CONTINUE
      END IF
      IF(W.LE.EPSILN) THEN
        G=ONE
      ELSE
        G=SQCHI2(W,NRED)
      END IF
      RTARG=R+G
      DO 20 I=1,NRED
        DX(I)=SQRT(CX(I,I))
   20 CONTINUE
      NRED1=NRED-1
      CALL MTXCPV(Y,YSAV,N)
      DO 60 IVAR=1,NR
        IF(LIST(IVAR).EQ.0) GO TO 60
C fix variable IVAR
        LIST(IVAR)=0
        DO 50 ISIGN=-1,1,2
          SIGNUM=DBLE(ISIGN)
          DEL=DX(IVAR)
          LENCLS=.FALSE.
C set XSMALL to x at minimum position
          XSMALL=X0(IVAR)
          XIVAR=X0(IVAR)+SIGNUM*DEL
          DO 30 I=1,NSTEP
            NSTEPL=NSTEP
            CALL MTXCPV(X0,X,NR)
            X(IVAR)=XIVAR
            CALL MTXCPV(YSAV,Y,N)
            CALL LSQGEN(Y,CY,FY,F,E,M,N,NR,NRED1,LIST,X,CX,R1,A2,
     +      NSTEPL)
            IF(NSTEPL.LT.1) THEN
              NSTEP=-1
              GO TO 70
            END IF
C test for convergence
            IF(ABS(R1-RTARG).LT.EPS*G) THEN
              IF(ISIGN.LT.0) THEN
                DXMINS(IVAR)=X0(IVAR)-X(IVAR)
              ELSE
                DXPLUS(IVAR)=X(IVAR)-X0(IVAR)
              END IF
              GO TO 40
            ELSE
              IF(.NOT.LENCLS) THEN
C zero was not yet enclosed in last step
                IF(R1.GT.RTARG) THEN
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
                IF(R1.GT.RTARG) THEN
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
