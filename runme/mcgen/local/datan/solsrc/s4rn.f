      PROGRAM S4RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(N=50,R=.5D0,DT=1.D-2,V0=1.D0,PI=3.14159D0)
      DIMENSION XA(2),XB(2),X(2,N),XLST(2,N),DX(2),V(2,N),XTRY(2)
      DIMENSION VI(2),VJ(2),W(2),UI(2),UJ(2),H1(2),H2(2)
      DIMENSION EPERP(2),EPAR(2)
      DIMENSION RAND(2)
      DIMENSION XPL(46),YPL(46)
      DIMENSION HIST(100)
      DIMENSION RCS(46),RSN(46)
      CHARACTER*15 TX,TY,CAPT
      DATA XA/0.D0,0.D0/,XB/10.D0,10.D0/
      DATA DEL/.1D0/
      DATA CAPT/'t = ***** dt'/
C identify program to user
      WRITE(*,*)' Program S4RN performs'
      WRITE(*,*)' Monte Carlo simulation of molecular motion'
      WRITE(*,*)' and produces graphical output'
      WRITE(*,*)' '
C initialization
      D=2.D0*R
      DSQ=D*D
      DELPHI=2.D0*PI/45.D0
      PHI=0.D0
      DO 5 I=0,45
        RCS(I+1)=R*COS(PHI)
        RSN(I+1)=R*SIN(PHI)
        PHI=PHI+DELPHI
5     CONTINUE
C let user choose
      WRITE(*,*)'Enter number of time steps (several 100 or 1000)'
      WRITE(*,*)' >'
      READ(*,*)NT
      MT=NT/10
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - display of trajectories'
      WRITE(*,*)' 2 - display of histograms'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      IF(ISWIT.EQ.1) THEN
C initialize graphics for trajectories
        CALL GROPEN
        CALL GROPWS(NWS)
        CALL GRWNCC(XA(1),XB(1),XA(2),XB(2))
        CALL GRFRAM
      ELSE IF(ISWIT.EQ.2) THEN
C let user choose type of histogram
        WRITE(*,*)' Choose'
        WRITE(*,*)' 1 - histogram of v_1'
        WRITE(*,*)' 2 - histogram of v_2'
        WRITE(*,*)' 3 - histogram of |v|'
        WRITE(*,*)' 4 - histogram of |v|^2 '
        WRITE(*,*)' >'
        READ(*,*)IHIST
C initialize histograms
        NX=100
        IF(IHIST.EQ.1)THEN
          X0=-V0
          DELX=0.02D0*V0
          TX='v_1'
          TY='N(v_1#)'
        ELSE IF(IHIST.EQ.2)THEN
          X0=-V0
          DELX=0.02D0*V0
          TX='v_2'
          TY='N(v_2#)'
        ELSE IF(IHIST.EQ.3) THEN
          X0=0.D0
          DELX=0.01D0*V0
          TX='v'
          TY='N(v)'
        ELSE IF(IHIST.EQ.4) THEN
          X0=0.D0
          DELX=0.01D0*V0**2
          TX='v^2'
          TY='N(v^2#)'
        END IF
      END IF
C choose initial positions
      DO 40 I=1,N
10      CALL RNECUY(RAND,2)
        DO 20 K=1,2
          XTRY(K)=XA(K)+R+RAND(K)*(XB(K)-XA(K)-D)
20      CONTINUE
        IF(I.GT.1) THEN
C repeat if there is overlap with existing particle
          DO 30 J=1,I-1
            IF((XTRY(1)-X(1,J))**2+(XTRY(2)-X(2,J))**2.LE.DSQ)
     +      GO TO 10
30        CONTINUE
        END IF
        X(1,I)=XTRY(1)
        X(2,I)=XTRY(2)
        XLST(1,I)=X(1,I)
        XLST(2,I)=X(2,I)
C draw initial positions
        DO 35 L=1,46
          XPL(L)=X(1,I)+RCS(L)
          YPL(L)=X(2,I)+RSN(L)
35      CONTINUE
        CALL GRSTCL(MOD(I,8)+1)
        CALL GRPLIN(46,XPL,YPL)
        IF(I.EQ.1) THEN
C draw double circle for sphere with non zero initial velocity
          DO 36 L=1,46
            XPL(L)=X(1,I)+.9D0*RCS(L)
            YPL(L)=X(2,I)+.9D0*RSN(L)
36        CONTINUE
          CALL GRPLIN(46,XPL,YPL)
        END IF
40    CONTINUE
C set initial velocities
      DO 45 I=2,N
        V(1,I)=0.D0
        V(2,I)=0.D0
45    CONTINUE
      CALL RNECUY(RAND,1)
      PHI=2.D0*PI*RAND(1)
      V(1,1)=V0*COS(PHI)
      V(2,1)=V0*SIN(PHI)
C loop over all times
      DO 1000 M=1,NT
C loop over all particles
        DO 60 I=1,N
C advance particle
          DO 50 K=1,2
            X(K,I)=X(K,I)+V(K,I)*DT
50        CONTINUE
C if advance is large enough draw piece of trajectory
          IF(ISWIT.EQ.1) THEN
            IF(ABS(XLST(1,I)-X(1,I)).GT.DEL .OR. ABS(XLST(2,I)-
     +      X(2,I)).GT.DEL) THEN
              XPL(1)=XLST(1,I)
              YPL(1)=XLST(2,I)
              XPL(2)=X(1,I)
              YPL(2)=X(2,I)
              XLST(1,I)=X(1,I)
              XLST(2,I)=X(2,I)
              CALL GRSTCL(MOD(I,8)+1)
              CALL GRPLIN(2,XPL,YPL)
            END IF
          END IF
60      CONTINUE
C test for collisions
        DO 200 I=1,N
C test for collisions with wall
          DO 70 K=1,2
            IF(X(K,I).LT.XA(K)+R.OR.X(K,I).GT.XB(K)-R) V(K,I)=-V(K,I)
70        CONTINUE
C test for collisions with another particle
          IF(I.LT.N) THEN
            DO 100 J=I+1,N
              DX(1)=X(1,I)-X(1,J)
              DX(2)=X(2,I)-X(2,J)
              DD=DX(1)**2+DX(2)**2
              IF(DD.LT.DSQ) THEN
                DINV=1.D0/SQRT(DD)
                EPERP(1)=DX(1)*DINV
                EPERP(2)=DX(2)*DINV
                EPAR(1)=EPERP(2)
                EPAR(2)=-EPERP(1)
                CALL MTXGCL(V,VI,2,N,I)
                CALL MTXGCL(V,VJ,2,N,J)
                CALL MTXADV(VI,VJ,W,2)
                CALL MTXMSV(W,W,0.5D0,2)
                CALL MTXSBV(VI,W,UI,2)
                CALL MTXDOT(UI,EPERP,UIPERP,2)
                CALL MTXDOT(UI,EPAR,UIPAR,2)
                UIPERP=-UIPERP
                CALL MTXMSV(EPERP,H1,UIPERP,2)
                CALL MTXMSV(EPAR,H2,UIPAR,2)
                CALL MTXADV(H1,H2,UI,2)
                CALL MTXADV(UI,W,VI,2)
                CALL MTXMSV(UI,UJ,-1.D0,2)
                CALL MTXADV(UJ,W,VJ,2)
                CALL MTXPCL(V,VI,2,N,I)
                CALL MTXPCL(V,VJ,2,N,J)
              END IF
100         CONTINUE
          END IF
200     CONTINUE
        IF(ISWIT.EQ.2) THEN
          IF(MOD(M,MT).EQ. 1.OR. M.EQ.NT) THEN
C produce histogram
            WRITE(CAPT(5:9),'(I5)')M
            CALL SMHSIN(HIST,X0,DELX,NX)
            DO 47 J=1,N
              IF(IHIST.EQ.1) HIS=V(1,J)
              IF(IHIST.EQ.2) HIS=V(2,J)
              IF(IHIST.EQ.3) HIS=SQRT(V(1,J)**2+V(2,J)**2)
              IF(IHIST.EQ.4) HIS=V(1,J)**2+V(2,J)
              CALL SMHSFL(HIST,X0,DELX,NX,HIS,1.D0)
47          CONTINUE
            CALL SMHSGR(HIST,X0,DELX,NX,TX,15,TY,15,CAPT,15,NWS)
          END IF
        END IF
1000  CONTINUE
      CALL GRCLSE
      END
