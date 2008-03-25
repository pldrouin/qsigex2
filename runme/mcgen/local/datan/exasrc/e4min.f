      PROGRAM E4MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION X(2),RANDOM(1000),XPL(100),YPL(100),LIST(2)
      DIMENSION CX(2,2),DXPLUS(2),DXMINS(2),SCRAT(2,3),SCRAT1(2,2)
      COMMON /CMINGH/ T(100),HIST(100),DELTAT,NT,NEVENT
      PARAMETER(NR=2,NRED=2,T0=-5.25D0)
      DOUBLE PRECISION MINGLP,MINGSQ
      CHARACTER*75 CAPT,TX,TY
      LOGICAL OK
      EXTERNAL MINGLP,MINGSQ
C identify program to user
      WRITE(*,*)' program E4MIN performs fit of Gaussian '
      WRITE(*,*)' to histogram by minimization'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number of events (>1,<=1000)'
      WRITE(*,*)' >'
      READ(*,*)NEVENT
      WRITE(*,*)' Enter number of histogram bins (<101)'
      WRITE(*,*)' >'
      READ(*,*)NT
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - likelihood minimization with Poisson stat.'
      WRITE(*,*)' 2 - minimization of sum of squares'
      WRITE(*,*)' >'
      READ(*,*)ICH
C draw sample
      NSEED1=1
      NSEED2=2
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNSTNR(RANDOM,NEVENT)
C prepare and fill histogram
      DELTAT=2.D0*ABS(T0)/DBLE(NT)
      DO 5 I=1,NT
        T(I)=T0+DBLE(I-0.5D0)*DELTAT
5     CONTINUE
      CALL SMHSIN(HIST,T0,DELTAT,NT)
      DO 10 I=1,NEVENT
        CALL SMHSFL(HIST,T0,DELTAT,NT,RANDOM(I),1.D0)
10    CONTINUE
C set first approximation
      X(1)=1.D0
      X(2)=2.D0
C minimize with MINSIM
      LIST(1)=1
      LIST(2)=1
      NSTEP=0
      EPSILN=0.D0
      WRITE(*,*)' minimization with MINSIM'
      WRITE(*,'(3(A,I4),A,2I2)')' N = ',NT,', NR = ',NR,
     +', NRED = ',NRED,', LIST = ',LIST
      WRITE(*,'(A,2F10.5)')' first approx.: X = ',X
      IF(ICH.EQ.1) THEN
        CALL MINSIM(X,NR,NRED,LIST,MINGLP,FMIN,EPSILN,NSTEP,SCRAT)
      ELSE
        CALL MINSIM(X,NR,NRED,LIST,MINGSQ,FMIN,EPSILN,NSTEP,SCRAT)
      END IF
      IF(NSTEP.LT.0) THEN
        WRITE(*,*)' minimization procedure failed'
        STOP
      END IF
      WRITE(*,'(A,F8.2,A,I6,/,A,(2F10.5))')
     +' result of minimization: FMIN = ', FMIN,', NSTEP =',NSTEP,
     +' X =',X
C determine covariance matrix
      FACT=DBLE(1.)
      IF(ICH.EQ.1) THEN
        CALL MINCOV(X,NR,NRED,LIST,MINGLP,FACT,SCRAT,SCRAT1,CX,OK)
      ELSE
        CALL MINCOV(X,NR,NRED,LIST,MINGSQ,FACT,SCRAT,SCRAT1,CX,OK)
      END IF
      IF(.NOT.OK) THEN
        WRITE(*,*)' determination of covariance matrix fails'
        STOP
      END IF
      WRITE(*,*)' covariance matrix CX = '
      CALL MTXWRT(CX,NRED,NRED)
      WRITE(*,'(/)')
C determine asymmetric errors
      NSTEP=0
      FCONT=FMIN+0.5D0
      IF(ICH.EQ.1) THEN
        CALL MINASY(MINGLP,NR,NRED,LIST,X,
     +  CX,FCONT,DXPLUS,DXMINS,SCRAT,NSTEP)
      ELSE
        CALL MINASY(MINGSQ,NR,NRED,LIST,X,
     +  CX,FCONT,DXPLUS,DXMINS,SCRAT,NSTEP)
      END IF
      WRITE(*,*)' asymmetric errors:'
      WRITE(*,'(A,2F10.5)')' DXPLUS = ',DXPLUS
      WRITE(*,'(A,2F10.5)')' DXMINS = ',DXMINS
      WRITE(*,'(/)')
1000  CONTINUE
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C plot
C prepare caption
      CAPT(1:38)='N = ****, x_1# = ******, x_2# = ******'
      CAPT(39:74)=', &D@x_1# = ******, &D@x_2# = ******'
      WRITE(CAPT(5:8),'(I4)') NEVENT
      WRITE(CAPT(18:23),'(F6.3)') X(1)
      WRITE(CAPT(33:38),'(F6.3)') X(2)
      WRITE(CAPT(51:56),'(F6.3)') SQRT(CX(1,1))
      WRITE(CAPT(69:74),'(F6.3)') SQRT(CX(2,2))
      LCAPT=74
      TX='y'
      LTX=1
      TY='N(y)'
      LTY=4
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - plot of histogram and fitted function'
      WRITE(*,*)' 2 - plot of parameters and confidence region'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      IF(ISWIT.EQ.1) THEN
C draw Gaussian corresponding to solution
        FACT=DBLE(NEVENT)*DELTAT*.3989423D0/X(2)
        NPL=100
        DPL=2.D0*ABS(T0)/DBLE(NPL-1)
        DO 30 IPL=1,NPL
          XPL(IPL)=T0+DBLE(IPL-1)*DPL
          YPL(IPL)=FACT*EXP(-0.5D0*((XPL(IPL)-X(1))/X(2))**2)
30      CONTINUE
        CALL GRHSCV(XPL,YPL,NPL,
     +  HIST,T0,DELTAT,NT,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      ELSE
C plot confidence region
        CALL GROPEN
        CALL GROPWS(NWS)
        XMIN=X(1)-2.D0*SQRT(CX(1,1))
        XMAX=2.D0*X(1)-XMIN
        YMIN=X(2)-2.D0*SQRT(CX(2,2))
        YMAX=2.D0*X(2)-YMIN
        CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
        CALL GRVWWC(0.2D0,0.9D0,0.15D0,0.85D0)
        CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
        CALL GRFRAM
        CALL GRBOUN
        CALL GRTXTC(1.D0,CAPT,74)
        TX='x_1'
        CALL GRSCLX(TX,3)
        TY='x_2'
        CALL GRSCLY(TY,3)
C draw solution with symmetric errors and covariance ellipse
        CALL GRSTCL(2)
        DX1=SQRT(CX(1,1))
        DX2=SQRT(CX(2,2))
        RH=CX(1,2)/(DX1*DX2)
        IF(ABS(RH).LT..001D0) RH=0.001D0
        CALL GRDATP(1,.5D0,X(1),X(2),DX1,DX2,RH)
C draw asymmetric errors
        CALL GRSTCL(3)
        DO 20 I=1,2
          IF(I.EQ.1) THEN
            XPL(1)=X(1)-DXMINS(1)
          ELSE
            XPL(1)=X(1)+DXPLUS(1)
          END IF
          XPL(2)=XPL(1)
          YPL(1)=YMIN
          YPL(2)=YMAX
          CALL GRPLIN(2,XPL,YPL)
20      CONTINUE
        DO 40 I=1,2
          IF(I.EQ.1) THEN
            YPL(1)=X(2)-DXMINS(2)
          ELSE
            YPL(1)=X(2)+DXPLUS(2)
          END IF
          YPL(2)=YPL(1)
          XPL(1)=XMIN
          XPL(2)=XMAX
          CALL GRPLIN(2,XPL,YPL)
40      CONTINUE
        NX=30
        NY=30
        DX=(XMAX-XMIN)/NX
        DY=(YMAX-YMIN)/NY
        CALL GRSTCL(4)
C draw confidence region
        IF(ICH.EQ.1)THEN
          CALL MINCNT(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,X,NR,MINGLP)
        ELSE
          CALL MINCNT(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,X,NR,MINGSQ)
        END IF
        CALL GRCLSE
      END IF
      END
