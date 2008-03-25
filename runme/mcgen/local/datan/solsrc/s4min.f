      PROGRAM S4MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(NR=5,NPL=1000,T0=-10.25D0,ONE=1.D0)
      COMMON /COM/ X(NR),RANDOM(1000),XPL(1000),YPL(1000),
     +CX(NR,NR),SCRAT(NR,NR+1),XMC(NR),SCRAT1(NR,NR),LIST(NR)
C COMMON /COM/ is needed for Microsoft FORTRAN compiler
      COMMON /CMINBH/ T(100),HIST(100),DELTAT,NT,NEVENT
      DOUBLE PRECISION MINBLP,MINBSQ
      CHARACTER*75 CAPT,TX,TY
      CHARACTER*40 TEXT(NR)
      LOGICAL OK
      EXTERNAL MINBLP,MINBSQ
      DATA TEXT /'fraction of events in 1st BW function   ',
     +'mean of 1st BW function  (>= -5., <=5.) ',
     +'FWHM of 1st BW function  (>0.)          ',
     +'mean of 2nd BW function  (>= -5., <=5.) ',
     +'FWHM of 2nd BW function  (>0.)          '/
C identify program to user
      WRITE(*,*)' program S4MIN performs fit of 2 Breit-Wigner'
      WRITE(*,*)' functions to sample or histogram'
      WRITE(*,*)' by minimization'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number of events (<= 1000)'
      WRITE(*,*)' >'
      READ(*,*)NEVENT
      WRITE(*,*)' Enter number of histogram bins (<= 100)'
      WRITE(*,*)' >'
      READ(*,*)NT
      DO 1 I=1,NR
        WRITE(*,*)' Enter ',TEXT(I)
        WRITE(*,*)' >'
        READ(*,*)XMC(I)
        WRITE(*,*)' Enter initial value for that parameter'
        WRITE(*,*)' >'
        READ(*,*)X(I)
        WRITE(*,*)' Enter'
        WRITE(*,*)' 1 - if parameter is to be kept variable'
        WRITE(*,*)' 0 - if parameter is to fixed'
        WRITE(*,*)' >'
        READ(*,*)LIST(I)
1     CONTINUE
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - likelihood minimization with Poisson stat.'
      WRITE(*,*)' 2 - minimization of sum of squares'
      WRITE(*,*)' >'
      READ(*,*)ICH
C draw sample
      NSEED1=87654
      NSEED2=98765
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNECUY(RANDOM,NEVENT)
      DO 2 I=1,NEVENT
        IF(RANDOM(I).LT.XMC(1)) THEN
          CALL RNBW(XMC(2),XMC(3),RANDOM(I),1)
        ELSE
          CALL RNBW(XMC(4),XMC(5),RANDOM(I),1)
        END IF
2     CONTINUE
C prepare and fill histogram
      DELTAT=2.D0*ABS(T0)/DBLE(NT)
      DO 5 I=1,NT
        T(I)=T0+DBLE(I-0.5D0)*DELTAT
5     CONTINUE
      CALL SMHSIN(HIST,T0,DELTAT,NT)
      DO 10 I=1,NEVENT
        CALL SMHSFL(HIST,T0,DELTAT,NT,RANDOM(I),1.D0)
10    CONTINUE
C set NRED
      NRED=0
      DO 15 I=1,NR
        IF(LIST(I).EQ.1) NRED=NRED+1
15    CONTINUE
C minimize with MINSIM
      NSTEP=0
      EPSILN=0.D0
      WRITE(*,*)' minimization with MINSIM'
      WRITE(*,'(3(A,I4),A,5I2)')' N = ',NT,', NR = ',NR,', NRED = ' ,
     +NRED,', LIST = ',LIST
      WRITE(*,'(A,5F10.5)')' first approx.: X = ',X
      IF(ICH.EQ.1) THEN
        CALL MINSIM(X,NR,NRED,LIST,MINBLP,FMIN,EPSILN,NSTEP,SCRAT)
      ELSE
        CALL MINSIM(X,NR,NRED,LIST,MINBSQ,FMIN,EPSILN,NSTEP,SCRAT)
      END IF
      IF(NSTEP.LT.0) THEN
        WRITE(*,*)' minimization procedure failed'
        STOP
      END IF
      WRITE(*,'(A,F8.2,A,I6,/,A,(5F10.5))')
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
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C plot
C prepare caption
      CAPT(1:38)='x_1#=******, x_2#=******, x_3#=******,'
      CAPT(39:63)=' x_4#=******, x_5#=******'
      WRITE(CAPT(6:11),'(F6.3)') X(1)
      WRITE(CAPT(19:24),'(F6.3)') X(2)
      WRITE(CAPT(32:37),'(F6.3)') X(3)
      WRITE(CAPT(45:50),'(F6.3)') X(4)
      WRITE(CAPT(58:63),'(F6.3)') X(5)
      LCAPT=63
      TX='y'
      LTX=1
      TY='N(y)'
      LTY=4
C draw curve corresponding to solution
      FACT=DBLE(NEVENT)*DELTAT
      DPL=2.D0*ABS(T0)/DBLE(NPL-1)
      DO 30 IPL=1,NPL
        XPL(IPL)=T0+DBLE(IPL-1)*DPL
        YPL(IPL)=FACT*(X(1)*BREWIG(XPL(IPL),X(2),X(3))+(ONE-X(1))
     +  *BREWIG(XPL(IPL),X(4),X(5)))
30    CONTINUE
      CALL GRHSCV(XPL,YPL,NPL,
     *HIST,T0,DELTAT,NT,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
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
      PARAMETER(ZERO=0.D0,ONE=1.D0)
      FNORM=DBLE(NEVENT)*DELTAT
      MINBLP=ZERO
      DO 10 I=1,NT
C GI is the value of the probability density of the population at T(I)
C (by replacing the RHS of the following statement it can be
C  changed from normal to any desired distribution)
        GI=X(1)*BREWIG(T(I),X(2),X(3))+(ONE-X(1))*BREWIG(T(I),X(4),X(5))
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
      PARAMETER(ZERO=0.D0,ONE=1.D0)
      FNORM=DBLE(NEVENT)*DELTAT
      MINBSQ=ZERO
      DO 10 I=1,NT
C GI is the value of the probability density of the population at T(I)
C (by replacing the RHS of the following statement it can be
C  changed from normal to any desired distribution)
        GI=X(1)*BREWIG(T(I),X(2),X(3))+(ONE-X(1))*BREWIG(T(I),X(4),X(5))
C normalize to number of events in sample
        GI=FNORM*GI
        IF(HIST(I).GT.ZERO) THEN
          MINBSQ=MINBSQ+(HIST(I)-GI)**2/HIST(I)
        END IF
10    CONTINUE
      END
C ------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION BREWIG(X,A,GAMMA)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C yields Breit-Wigner function at point X with mean A and FWHM GAMMA
      PARAMETER(TOP=.6366198D0)
      BREWIG=TOP*ABS(GAMMA)/(4.D0*(X-A)**2+GAMMA**2)
      END
