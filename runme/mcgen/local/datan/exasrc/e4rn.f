      PROGRAM E4RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(N=50,R=.5D0,DT=1.D-2,V0=1.D0,PI=3.14159D0)
      DIMENSION XA(2),XB(2),X(2,N),DX(2),V(2,N),XTRY(2)
      DIMENSION VI(2),VJ(2),W(2),UI(2),UJ(2),H1(2),H2(2)
      DIMENSION EPERP(2),EPAR(2),RAND(2),HIST(40)
      DATA XA/0.D0,0.D0/,XB/10.D0,10.D0/,NT/5000/,MT/500/
      DATA NX/40/,X0/-1.D0/,DELX/.05D0/
C identify program to user
      WRITE(*,*)' Program E4RN performs'
      WRITE(*,*)' Monte Carlo simulation of molecular motion'
      WRITE(*,*)' '
C initialization
      D=2.D0*R
      DSQ=D*D
      DELPHI=2.D0*PI/45.D0
      NWS=1
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
40      CONTINUE
C set initial velocities
        DO 45 I=2,N
          V(1,I)=0.D0
          V(2,I)=0.D0
45      CONTINUE
        CALL RNECUY(RAND,1)
        PHI=2.D0*PI*RAND(1)
        V(1,1)=V0*COS(PHI)
        V(2,1)=V0*SIN(PHI)
C write header for output table
        WRITE(*,*)'The following lines are numerical'
        WRITE(*,*)'histograms for the x-component of velocity'
        WRITE(*,*)'velocity of the 50 particles in the simulation.'
        WRITE(*,*)'Each line is a histogram for a fixed time'
        WRITE(*,*)'Line number 1 shows that 49 particles are at'
        WRITE(*,*)'rest. They are in the histogram bin around'
        WRITE(*,*)'vx=0. One particle has nonzero velocity.'
C loop over all times
        DO 1000 M=1,NT
          IF(MOD(M,MT).EQ.1) THEN
C produce histogram
            CALL SMHSIN(HIST,X0,DELX,NX)
            DO 47 J=1,N
              CALL SMHSFL(HIST,X0,DELX,NX,V(1,J),1.D0)
47          CONTINUE
            WRITE(*,'(1X,40I2)')(INT(HIST(K)),K=1,40)
          END IF
C advance all particles
          DO 60 I=1,N
            DO 50 K=1,2
              X(K,I)=X(K,I)+V(K,I)*DT
50          CONTINUE
60        CONTINUE
C test for collisions
          DO 200 I=1,N
C test for collisions with wall
            DO 70 K=1,2
              IF(X(K,I).LE.XA(K)+R.OR.
     +        X(K,I).GE.XB(K)-R) V(K,I)=-V(K,I)
70          CONTINUE
C test for collisions with another particle
            IF(I.LT.N) THEN
              DO 100 J=I+1,N
                DX(1)=X(1,I)-X(1,J)
                DX(2)=X(2,I)-X(2,J)
                DD=DX(1)**2+DX(2)**2
                IF(DD.LE.DSQ) THEN
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
100           CONTINUE
            END IF
200       CONTINUE
1000    CONTINUE
        END
