      PROGRAM E2MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CMINGS/ Y,NNY
      DIMENSION X(2),Y(1000),XPL(100),YPL(100),LIST(2)
      DIMENSION CX(2,2),SCRAT(2,3),SCRAT1(2,2)
      PARAMETER(NR=2,NRED=2)
      DOUBLE PRECISION MINGLS
      CHARACTER*75 STRING
      LOGICAL OK
      EXTERNAL MINGLS
C identify program to user
      WRITE(*,*)' Program E2MIN demonstrates use of MINCOV'
      WRITE(*,*)' by fitting Gaussian to small sample'
      WRITE(*,*)' and determining errors of parameters by MINCOV'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number n of events (2 <= n <=1000)'
      WRITE(*,*)' >'
      READ(*,*)NNY
C draw sample
      NSEED1=1
      NSEED2=2
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNSTNR(Y,NNY)
C write table of data
      WRITE(*,*)' sample Y is'
      CALL MTXWRT(Y,1,NNY) 
C set first approximation
      X(1)=1.D0
      X(2)=2.D0
C minimize with MINSIM
      LIST(1)=1
      LIST(2)=1
      NSTEP=10000
      EPSILN=0.D0
      FMIN=.01D0
      WRITE(*,*)' minimization with MINSIM'
      WRITE(*,'(3(A,I2),A,2I2)')' N = ',NNY,', NR = ',NR,
     +', NRED = ',NRED,', LIST = ',LIST
      WRITE(*,'(A,2F10.5)')' first approx.: X = ',X
      CALL MINSIM(X,NR,NRED,LIST,MINGLS,FMIN,EPSILN,NSTEP,SCRAT)
      WRITE(*,'(A,F6.2,A,I6,/,A,(2F10.5))')
     +' result of minimization: FMIN = ', FMIN,', NSTEP =',NSTEP,
     +' X =',X
C determine covariance matrix
      FACT=DBLE(1.)
      CALL MINCOV(X,NR,NRED,LIST,MINGLS,FACT,SCRAT,SCRAT1,CX,OK)
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
C plot sample as one dimensional scatter plot and Gaussian
      XMIN=-5.D0
      XMAX=5.D0
      YMIN=0.D0
      YMAX=.5D0
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
C draw Gaussian corresponding to solution
      FACT=.3989423D0/X(2)
      NPL=100
      DPL=(XMAX-XMIN)/DBLE(NPL-1)
      DO 30 IPL=1,NPL
        XPL(IPL)=XMIN+DBLE(IPL-1)*DPL
        YPL(IPL)=FACT*EXP(-0.5D0*((XPL(IPL)-X(1))/X(2))**2)
30    CONTINUE
      CALL GRSTCL(4)
      CALL GRPLIN(NPL,XPL,YPL)
      CALL GRCLSE
      END
