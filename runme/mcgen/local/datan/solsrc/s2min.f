      PROGRAM S2MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CMINBW/ Y,NNY
      DIMENSION X(2),Y(1000),XPL(1000),YPL(1000),LIST(2)
      DIMENSION CX(2,2),SCRAT(2,3),SCRAT1(2,2)
      PARAMETER(NR=2,NRED=2,NPL=1000,PI=3.14159D0)
      DOUBLE PRECISION MINBWL
      CHARACTER*75 STRING
      LOGICAL OK
      EXTERNAL MINBWL
C identify program to user
      WRITE(*,*)' program S2MIN performs fit of Breit-Wigner '
      WRITE(*,*)' to small sample by minimization'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number n of events(2 <= n <= 1001)'
      WRITE(*,*)' >'
      READ(*,*)NNY
C draw sample from Breit-Wigner distribution
      NSEED1=87654
      NSEED2=98765
      CALL RNE2IN(NSEED1,NSEED2)
      A=0.D0
      GAMMA=1.D0
      CALL RNBW(A,GAMMA,Y,NNY)
C write table of data
      WRITE(*,*)' sample Y is'
      WRITE(*,'(1X,10F7.2)')(Y(K),K=1,NNY)
      WRITE(*,'(/)')
C find first approximation
      CALL SMMNVR(Y,NNY,X(1),DELXM,S2,DELS2,X(2),DELS)
C minimize with MINSIM
      LIST(1)=1
      LIST(2)=1
      NSTEP=10000
      EPSILN=0.D0
      FMIN=.01D0
      WRITE(*,*)' minimization with MINSIM'
      WRITE(*,'(3(A,I4),A,2I2)')' N = ',NNY,', NR = ',NR,', NRED = ' ,
     +NRED,', LIST = ',LIST
      WRITE(*,'(A,2F10.5)')' first approx.: X = ',X
      CALL MINSIM(X,NR,NRED,LIST,MINBWL,FMIN,EPSILN,NSTEP,SCRAT)
      WRITE(*,'(A,F6.2,A,I6,/,A,(2F10.5))')
     +' result of minimization: FMIN = ', FMIN,', NSTEP =',NSTEP,' X =',
     +X
C determine covariance matrix
      FACT=DBLE(1.)
      CALL MINCOV(X,NR,NRED,LIST,MINBWL,FACT,SCRAT,SCRAT1,CX,OK)
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
C prepare graphics
      XMIN=-10.D0
      XMAX=10.D0
      YMIN=0.D0
      YMAX=1.D0
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
      CALL GRVWWC(-0.2D0,0.9D0,0.15D0,0.85D0)
      CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
      CALL GRFRAM
      CALL GRBOUN
      STRING(1:38)='N = ****, x_1# = ******, x_2# = ******'
      STRING(39:74)=', &D@x_1# = ******, &D@x_2# = ******'
      WRITE(STRING(5:8),'(I4)') NNY
      WRITE(STRING(18:23),'(F6.3)') X(1)
      WRITE(STRING(33:38),'(F6.3)') X(2)
      WRITE(STRING(51:56),'(F6.3)') SQRT(CX(1,1))
      WRITE(STRING(69:74),'(F6.3)') SQRT(CX(2,2))
      CALL GRTXTC(1.D0,STRING,74)
      STRING='x_1'
      CALL GRSCLX(STRING,3)
      STRING='x_2'
      CALL GRSCLY(STRING,3)
C plot scatter diagram
      CALL GRSTCL(2)
      DO 20 I=1,NNY
        XPL(1)=Y(I)
        XPL(2)=XPL(1)
        YPL(1)=0.D0
        YPL(2)=0.1D0
        CALL GRPLIN(2,XPL,YPL)
20    CONTINUE
C draw Breit-Wigner corresponding to solution
      FACT=2.D0/(PI*X(2))
      DPL=(XMAX-XMIN)/DBLE(NPL-1)
      DO 30 IPL=1,NPL
        XPL(IPL)=XMIN+DBLE(IPL-1)*DPL
        YPL(IPL)=FACT*X(2)**2/(4.D0*(XPL(IPL)-X(1))**2+X(2)**2)
30    CONTINUE
      CALL GRSTCL(4)
      CALL GRPLIN(NPL,XPL,YPL)
      CALL GRCLSE
      END
C--------------------------------------------
      DOUBLE PRECISION FUNCTION MINBWL(X,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /CMINBW/ Y,NY
      DIMENSION Y(1000)
      DIMENSION X(N)
      PARAMETER(PI=3.14159D0)
      F=DBLE(NY)*(LOG(2.D0)-LOG(PI)+LOG(ABS(X(2))))
      DO 10 I=1,NY
        F=F-LOG(4.D0*(Y(I)-X(1))**2+X(2)**2)
10    CONTINUE
      MINBWL=-F
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
