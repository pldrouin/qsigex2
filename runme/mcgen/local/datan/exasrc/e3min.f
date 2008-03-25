      PROGRAM E3MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CMINGS/ Y,NNY
      DIMENSION X(2),Y(1000),XPL(2),YPL(2),LIST(2),
     +CX(2,2),DXPLUS(2),DXMINS(2),SCRAT(2,3),SCRAT1(2,2)
      DOUBLE PRECISION MINGLS
      CHARACTER*75 STRING
      LOGICAL OK
      PARAMETER(NR=2,NRED=2)
      EXTERNAL MINGLS
C identify program to user
      WRITE(*,*)' Program E3MIN demonstrates use of'
      WRITE(*,*)' MINASY and MINCNT'
      WRITE(*,*)' '
      WRITE(*,*)' Enter number of events (>1)'
      WRITE(*,*)' >'
      READ(*,*)NNY
C draw sample
      NSEED1=1
      NSEED2=2
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNSTNR(Y,NNY)
C determine first approximation
      LIST(1)=1
      LIST(2)=1
      X(1)=1.0D0
      X(2)=2.0D0
C minimize with MINSIM
      NSTEP=10000
      EPSILN=0.D0
      FMIN=.01D0
      WRITE(*,*)' minimization with MINSIM'
      WRITE(*,'(3(A,I2),A,2I2)')
     +' N = ',NNY,', NR = ',NR,', NRED = ',NRED,', LIST = ',LIST
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
C prepare size of plot
      XMIN=X(1)-2.D0*SQRT(CX(1,1))
      XMAX=2.D0*X(1)-XMIN
      YMIN=X(2)-2.D0*SQRT(CX(2,2))
      YMAX=2.D0*X(2)-YMIN
C determine asymmetric errors
      FCONT=FMIN+0.5D0
      CALL MINASY(MINGLS,NR,NRED,LIST,X,CX,FCONT,DXPLUS,DXMINS,
     +SCRAT,NSTEP)
      WRITE(*,*)' asymmetric errors:'
      WRITE(*,'(A,2F10.5)')' DXPLUS = ',DXPLUS
      WRITE(*,'(A,2F10.5)')' DXMINS = ',DXMINS
      WRITE(*,'(/)')
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C plot confidence region
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
      CALL GRVWWC(0.2D0,0.9D0,0.15D0,0.85D0)
      CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
      CALL GRFRAM
      CALL GRBOUN
      STRING='N = ****, x_1# = ******, x_2# = ******'
      WRITE(STRING(5:8),'(I4)') NNY
      WRITE(STRING(18:23),'(F6.3)') X(1)
      WRITE(STRING(33:38),'(F6.3)') X(2)
      CALL GRTXTC(1.D0,STRING,38)
      STRING='x_1'
      CALL GRSCLX(STRING,3)
      STRING='x_2'
      CALL GRSCLY(STRING,3)
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
20    CONTINUE
      DO 30 I=1,2
        IF(I.EQ.1) THEN
          YPL(1)=X(2)-DXMINS(2)
        ELSE
          YPL(1)=X(2)+DXPLUS(2)
        END IF
        YPL(2)=YPL(1)
        XPL(1)=XMIN
        XPL(2)=XMAX
        CALL GRPLIN(2,XPL,YPL)
30    CONTINUE
      NX=30
      NY=30
      DX=(XMAX-XMIN)/NX
      DY=(YMAX-YMIN)/NY
      CALL GRSTCL(4)
C draw confidence region
      CALL MINCNT(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,X,NR,MINGLS)
      CALL GRCLSE
      END
