      SUBROUTINE MINSIM(X0,N,NRED,LIST,USERFN,FMIN,EPSILN,NSTEP,X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXN=50)
      DIMENSION X0(N),X(NRED,NRED+1)
      DIMENSION Y(MAXN+1),XEXP(MAXN),XR(MAXN),XR2(MAXN),
     +XPRIME(MAXN),X1(MAXN),X2(MAXN),XMX(MAXN),XMN(MAXN)
      DIMENSION LIST(N)
      PARAMETER (EPSDEF=1.D-25,TT=1.D-30,MAXSTP=1000)
      PARAMETER (ALPHA=1.D0,BETA=.5D0,GAMMA=2.D0)
      PARAMETER (ONE=1.D0,HALF=.5D0,ZERO=0.D0)
      EXTERNAL USERFN
C initialization
      EPS=EPSILN
      IF(EPS.LE.ZERO) EPS=EPSDEF
      NST=NSTEP
      IF(NST.LE.0) NST=MAXSTP
      M=NRED+1
C construct initial simplex and compute function at its corners
      Y(1)=USERFN(X0,N)
      CALL MTXGSV(X0,X1,N,NRED,LIST)
      CALL MTXPCL(X,X1,NRED,M,1)
      D=FMIN
      IF(D.LE.ZERO) D=1.D-5
      IRED=0
      DO 10 I=1,N
        IF(LIST(I).EQ.1) THEN
          IRED=IRED+1
          CALL MTXCPV(X1,X2,NRED)
          X2(IRED)=X2(IRED)+D
          CALL MTXPCL(X,X2,NRED,M,IRED+1)
          CALL MTXCPV(X0,XEXP,N)
          CALL MTXPSV(XEXP,X2,N,NRED,LIST)
          Y(IRED+1)=USERFN(XEXP,N)
        END IF
   10 CONTINUE
      ISTEP=0
C begin iteration
   20 ISTEP=ISTEP+1
C compute indices of corner with largest (MX), next to largest (NE)
C and smallest (MN) function value
      MN=1
      IF(Y(1).GT.Y(2)) THEN
        MX=1
        NE=2
      ELSE
        MX=2
        NE=1
      END IF
      DO 30 I=1,NRED+1
        IF(Y(I).LT.Y(MN)) MN=I
        IF(Y(I).GT.Y(MX)) THEN
          NE=MX
          MX=I
        ELSE IF (Y(I).GT.Y(NE)) THEN
          IF(I.NE.MX) NE=I
        END IF
   30 CONTINUE
C test for break-off condition
      IF(ABS(Y(MX)-Y(MN)).LT.EPS*ABS(Y(MN))+TT .OR. ISTEP.GT.NST)
     +GO TO 60
C compute XPRIME, i.e. point in center of gravity of hyperplane
C opposite to point with largest function value
      CALL MTXGCL(X,XMX,NRED,M,MX)
      CALL MTXGCL(X,XMN,NRED,M,MN)
      CALL MTXZRV(XPRIME,NRED)
      DO 40 I=1,NRED+1
        IF(I.NE.MX) THEN
          CALL MTXGCL(X,X1,NRED,M,I)
          CALL MTXADV(XPRIME,X1,XPRIME,NRED)
        END IF
   40 CONTINUE
      F=ONE/DBLE(NRED)
      CALL MTXMSV(XPRIME,XPRIME,F,NRED)
C construct points by reflection (XR) and extension (XR2)
      CALL MTXMSV(XPRIME,X1,ONE+ALPHA,NRED)
      CALL MTXMSV(XMX,X2,ALPHA,NRED)
      CALL MTXSBV(X1,X2,XR,NRED)
      CALL MTXCPV(X0,XEXP,N)
      CALL MTXPSV(XEXP,XR,N,NRED,LIST)
      YR=USERFN(XEXP,N)
      IF(YR.LE.Y(MN)) THEN
        CALL MTXMSV(XR,X1,GAMMA,NRED)
        CALL MTXMSV(XPRIME,X2,ONE-GAMMA,NRED)
        CALL MTXADV(X1,X2,XR2,NRED)
        CALL MTXCPV(X0,XEXP,N)
        CALL MTXPSV(XEXP,XR2,N,NRED,LIST)
        YR2=USERFN(XEXP,N)
        IF(YR2.LT.Y(MN)) THEN
C perform extension
          CALL MTXPCL(X,XR2,NRED,M,MX)
          Y(MX)=YR2
        ELSE
C perform reflection
          CALL MTXPCL(X,XR,NRED,M,MX)
          Y(MX)=YR
        END IF
      ELSE IF(YR.GE.Y(NE)) THEN
        IF(YR.LT.Y(MX)) THEN
C perform reflection
          CALL MTXPCL(X,XR,NRED,M,MX)
          Y(MX)=YR
        END IF
        CALL MTXMSV(XMX,X1,BETA,NRED)
        CALL MTXMSV(XPRIME,X2,ONE-BETA,NRED)
        CALL MTXADV(X1,X2,XR2,NRED)
        CALL MTXCPV(X0,XEXP,N)
        CALL MTXPSV(XEXP,XR2,N,NRED,LIST)
        YR2=USERFN(XEXP,N)
        IF(YR2.LT.Y(MX)) THEN
C perform compression
          CALL MTXPCL(X,XR2,NRED,M,MX)
          Y(MX)=YR2
        ELSE
C perform contraction
          DO 50 I=1,NRED+1
            IF(I.NE.MN) THEN
              CALL MTXGCL(X,X1,NRED,M,I)
              CALL MTXADV(X1,XMN,X2,NRED)
              CALL MTXMSV(X2,X2,HALF,NRED)
              CALL MTXPCL(X,X2,NRED,M,I)
              CALL MTXCPV(X0,XEXP,N)
              CALL MTXPSV(XEXP,X2,N,NRED,LIST)
              Y(I)=USERFN(XEXP,N)
            END IF
   50     CONTINUE
        END IF
      ELSE
C perform reflection
        CALL MTXPCL(X,XR,NRED,M,MX)
        Y(MX)=YR
      END IF
      GO TO 20
C Fill output arguments
   60 ISTEP=ISTEP-1
      FMIN=Y(MN)
      CALL MTXGCL(X,X1,NRED,M,MN)
      CALL MTXPSV(X0,X1,N,NRED,LIST)
      IF(ISTEP.GT.NST) THEN
        NSTEP=-1
      ELSE
        NSTEP=ISTEP
      END IF
      END
