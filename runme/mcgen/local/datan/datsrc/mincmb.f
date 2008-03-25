      SUBROUTINE MINCMB(A,B,EPSILN,USERFN,XMIN,FMIN,NSTEP,
     +X0,XDIR,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION MINFND
      LOGICAL PARSTP
      PARAMETER(C=0.381966D0,MAXSTP=1000,EPSDEF=1.D-12,TT=1.D-15)
      PARAMETER(ZERO=0.D0, HALF=0.5D0, ONE=1.D0, TWO=2.D0)
      DIMENSION X0(N),XDIR(N)
      EXTERNAL USERFN
C if some input values are not given use defaults
      IF(NSTEP.LT.1) NSTEP=MAXSTP
      EPS=EPSILN
      IF(EPS.LE.ZERO) EPS=EPSDEF
C initialize X at golden section position between A and B
      X=A+C*(B-A)
C initialize E,V,W,FX,FV,FW
      E=ZERO
      V=X
      W=X
      FX=MINFND(X,X0,XDIR,N,USERFN)
      FV=FX
      FW=FX
      ISTEP=1
C start iteration
   10 CONTINUE
      IF(ISTEP.GE.NSTEP) THEN
C prepare exit if step number is too large
        NSTEP=-1
        XMIN=X
        FMIN=FX
        GO TO 20
      END IF
      XM=HALF*(A+B)
      TOL=EPS*ABS(X)+TT
      T2=TWO*TOL
      IF(ABS(X-XM).GT.T2-HALF*(B-A)) THEN
        P=ZERO
        Q=ZERO
        R=ZERO
        PARSTP = .FALSE.
        IF(ABS(E).GT.TOL) THEN
C fit parabola
          R=(X-W)*(FX-FV)
          Q=(X-V)*(FX-FW)
          P=(X-V)*Q-(X-W)*R
          Q=TWO*(Q-R)
          IF(Q.GT.ZERO) THEN
            P=-P
          ELSE
            Q=-Q
          END IF
          R=E
          E=D
          IF(ABS(P).LT.ABS(HALF*Q*R) .AND. P.GT.Q*(A-X) .AND. P.LT.
     +    Q*(B-X)) THEN
C use result of parabolic fit
            D=P/Q
            U=X+D
            IF((U-A).LT.T2 .OR. (B-U).LT.T2) THEN
C make sure that U is not too near to A or B
              D=-TOL
              IF(X.LT.XM) D=TOL
            END IF
            PARSTP = .TRUE.
          END IF
        END IF
        IF(.NOT. PARSTP) THEN
C perform golden section step
          IF(X.LT.XM) THEN
            E=B-X
          ELSE
            E=A-X
          END IF
          D=C*E
        END IF
C determine point U where function is to be computed
C making sure that it is not too close to X
        IF(ABS(D) .GE. TOL) THEN
          U=X+D
        ELSE IF (D.GT.ZERO) THEN
          U=X+TOL
        ELSE
          U=X-TOL
        END IF
        FU=MINFND(U,X0,XDIR,N,USERFN)
C update A,B,V,W,X
        IF(FU.LE.FX) THEN
          IF(U.LT.X) THEN
            B=X
          ELSE
            A=X
          END IF
          V=W
          FV=FW
          W=X
          FW=FX
          X=U
          FX=FU
        ELSE
          IF(U.LT.X) THEN
            A=U
          ELSE
            B=U
          END IF
          IF(FU.LE.FW .OR. W.EQ.X) THEN
            V=W
            FV=FW
            W=U
            FW=FU
          ELSE IF(FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
            V=U
            FV=FU
          END IF
        END IF
        ISTEP=ISTEP+1
        GO TO 10
      ELSE
        XMIN=X
        FMIN=FX
        NSTEP=ISTEP
      END IF
   20 CONTINUE
      END
