      PROGRAM S3MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /COM/ X(2),RANDOM(1000),XPL(1000),YPL(1000),LIST(2),
     *CX(2,2),DXPLUS(2),DXMINS(2),SCRAT(2,3),SCRAT1(2,2)
C COMMON /COM/ is needed for Microsoft FORTRAN compiler
      COMMON /CMINBH/ T(100),HIST(100),DELTAT,NT,NEVENT
      PARAMETER(NR=2,NRED=2,NPL=1000,T0=-10.25D0)
      DOUBLE PRECISION MINBLP,MINBSQ
      CHARACTER*75 CAPT,TX,TY
      LOGICAL OK
      EXTERNAL MINBLP,MINBSQ
C identify program to user
      WRITE(*,*)' program S3MIN performs fit of Breit-Wigner function'
      WRITE(*,*)' to histogram by minimization'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number n of events (3 <= n <= 1000)'
      WRITE(*,*)' >'
      READ(*,*)NEVENT
      WRITE(*,*)' Enter number (>=3, <=100) of histogram bins'
      WRITE(*,*)' >'
      READ(*,*)NT
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - likelihood minimization with Poisson stat.'
      WRITE(*,*)' 2 - minimization of sum of squares'
      WRITE(*,*)' >'
      READ(*,*)ICH
C draw sample
      NSEED1=87654
      NSEED2=98765
      CALL RNE2IN(NSEED1,NSEED2)
      A=0.D0
      GAMMA=1.D0
      CALL RNBW(A,GAMMA,RANDOM,NEVENT)
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
      WRITE(*,'(3(A,I4),A,2I2)')' N = ',NT,', NR = ',NR,', NRED = ' ,
     +NRED,', LIST = ',LIST
      WRITE(*,'(A,2F10.5)')' first approx.: X = ',X
      IF(ICH.EQ.1) THEN
        CALL MINSIM(X,NR,NRED,LIST,MINBLP,FMIN,EPSILN,NSTEP,SCRAT)
      ELSE
        CALL MINSIM(X,NR,NRED,LIST,MINBSQ,FMIN,EPSILN,NSTEP,SCRAT)
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
        CALL MINCOV(X,NR,NRED,LIST,MINBLP,FACT,SCRAT,SCRAT1,CX,OK)
      ELSE
        CALL MINCOV(X,NR,NRED,LIST,MINBSQ,FACT,SCRAT,SCRAT1,CX,OK)
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
        CALL MINASY(MINBLP,NR,NRED,LIST,X,CX,FCONT,DXPLUS,DXMINS,SCRA
     +  T,NSTEP)
      ELSE
        CALL MINASY(MINBSQ,NR,NRED,LIST,X,CX,FCONT,DXPLUS,DXMINS,SCRA
     +  T,NSTEP)
      END IF
      WRITE(*,*)' asymmetric errors:'
      WRITE(*,'(A,2F10.5)')' DXPLUS = ',DXPLUS
      WRITE(*,'(A,2F10.5)')' DXMINS = ',DXMINS
      WRITE(*,'(/)')
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
        FACT=DBLE(NEVENT)*DELTAT*.6366198D0
        DPL=2.D0*ABS(T0)/DBLE(NPL-1)
        DO 30 IPL=1,NPL
          XPL(IPL)=T0+DBLE(IPL-1)*DPL
          YPL(IPL)=FACT*ABS(X(2))/(4.D0*(XPL(IPL)-X(1))**2+X(2)**2)
30      CONTINUE
        CALL GRHSCV(XPL,YPL,NPL,HIST,T0,DELTAT,NT,TX,LTX,TY,LTY,CA
     +  PT,LCAPT,NWS)
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
          CALL MINCNT(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,X,NR,MINBLP)
        ELSE
          CALL MINCNT(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,X,NR,MINBSQ)
        END IF
        CALL GRCLSE
      END IF
      END
C ------------------------------------------------------------------
      SUBROUTINE RNBW(A,GAMMA,R,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(PI=3.14159D0,HALF=0.5D0)
      DIMENSION R(N)
      CALL RNECUY(R,N)
      DO 10 I=1,N
        R(I)=A+HALF*GAMMA*TAN(PI*(R(I)-HALF))
10    CONTINUE
      END
C ------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION MINBLP(X,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CMINBH/ T(100),HIST(100),DELTAT,NT,NEVENT
      DIMENSION X(N)
      PARAMETER(ZERO=0.D0,TOP=.6366198D0)
      FNORM=DBLE(NEVENT)*DELTAT
      MINBLP=ZERO
      DO 10 I=1,NT
C GI is the value of the probability density of the population at T(I)
C (by replacing the RHS of the following statement it can be
C  changed from normal to any desired distribution)
        GI=TOP*ABS(X(2))/(4.D0*(T(I)-X(1))**2+X(2)**2)
C normalize to number of events in sample
        GI=FNORM*GI
        ALAM=GI
        IF(ALAM.GT.ZERO) THEN
          NI=NINT(HIST(I))
            ALNLAM=LOG(ALAM)
            ALNNI=GLNGAM(DBLE(NI+1))
          MINBLP=MINBLP+ALNNI-HIST(I)*ALNLAM+ALAM
        END IF
10    CONTINUE
      END
C ------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION MINBSQ(X,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CMINBH/ T(100),HIST(100),DELTAT,NT,NEVENT
      DIMENSION X(N)
      PARAMETER(ZERO=0.D0,TOP=.6366198D0)
      FNORM=DBLE(NEVENT)*DELTAT
      MINBSQ=ZERO
      DO 10 I=1,NT
C GI is the value of the probability density of the population at T(I)
C (by replacing the RHS of the following statement it can be
C  changed from normal to any desired distribution)
        GI=TOP*ABS(X(2))/(4.D0*(T(I)-X(1))**2+X(2)**2)
C normalize to number of events in sample
        GI=FNORM*GI
        IF(HIST(I).GT.ZERO) THEN
          MINBSQ=MINBSQ+(HIST(I)-GI)**2/HIST(I)
        END IF
10    CONTINUE
      END
