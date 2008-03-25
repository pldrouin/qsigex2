      PROGRAM E8LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXN=8,MAXNR=2,MAXM=4)
      COMMON /GRVARS/ X(MAXNR),Y(MAXN),CY(MAXN,MAXN),YY(MAXN),
     +GY(MAXN,MAXN),CX(MAXNR,MAXNR),CYY(MAXN,MAXN),
     +F(MAXN+MAXNR,MAXN+MAXNR),E(MAXM,MAXN+MAXNR),
     +A2(MAXN+MAXNR,MAXN+MAXNR),LIST(2),DXPLUS(2),DXMINS(2),
     +XPL(2),YPL(2)
      DIMENSION T(4),S(4),DT(4),DS(4),RHO(4)
      CHARACTER*75 STRING
      CHARACTER*1 ANSWER,YES1,YES2
      DATA YES1/'Y'/
      DATA YES2/'y'/
      DATA T /0.2D0,0.2D0,0.8D0,0.85D0/
      DATA S /0.15D0,0.7D0,1.1D0,0.8D0/
      DATA DT/0.4D0,0.4D0,0.4D0,0.2D0/
      DATA DS/0.2D0,0.4D0,0.4D0,0.4D0/
      DATA RHO/0.D0,0.D0,0.5D0,0.D0/
C identify program to user
      WRITE(*,*)' Program E8LSQ demonstrates'
      WRITE(*,*)' use of LSQASG and LSQCOG'
      WRITE(*,*)' '
C write table of data
      WRITE(*,*)'    T         S         DT        DS        RHO'
      DO 5 I=1,4
        WRITE(*,'(5F10.5)')T(I),S(I),DT(I),DS(I),RHO(I)
5     CONTINUE
      WRITE(*,'(/)')
C set up data for input to LSQGEN
      N=8
      M=4
      NR=2
      CALL MTXUNT(CY,8)
      DO 10 I=1,4
        Y((I-1)*2+1)=T(I)
        Y((I-1)*2+2)=S(I)
        CY((I-1)*2+1,(I-1)*2+1)=DT(I)**2
        CY((I-1)*2+2,(I-1)*2+2)=DS(I)**2
        CY((I-1)*2+1,(I-1)*2+2)=RHO(I)*DS(I)*DT(I)
        CY((I-1)*2+2,(I-1)*2+1)=RHO(I)*DS(I)*DT(I)
10    CONTINUE
C save input data
      CALL MTXCPV(Y,YY,N)
      CALL MTXTRA(CY,CYY,N,N)
C determine first approximation
      NRED=2
      LIST(1)=1
      LIST(2)=1
      X(2)=(S(4)-S(1))/(T(4)-T(1))
      X(1)=S(1)-X(2)*T(1)
C perform fit with LSQGEN
      WRITE(*,*)' performing fit with LSQGEN'
      WRITE(*,'(3(A,I2),A,2I2)')' N = ',N,', NR = ',NR,', NRED = ',
     +NRED,', LIST = ',LIST
      WRITE(*,'(A,2F10.5)')' first approx.: X = ',X
      NSTEP=100
      CALL LSQGEN(Y,CY,GY,F,E,M,N,NR,NRED,LIST,X,CX,R,A2,NSTEP)
      WRITE(*,'(A,F6.2,A,I3,/,A,(2F10.5))')' result of fit: R = ',R,
     +', NSTEP =',NSTEP,' X =',X
      WRITE(*,*)' covariance matrix CX = '
      CALL MTXWRT(CX,NRED,NRED)
      WRITE(*,'(/)')
C store solution, errors and correlation for later drawing
      X1=X(1)
      X2=X(2)
      DX1=SQRT(CX(1,1))
      DX2=SQRT(CX(2,2))
      RH=CX(1,2)/(DX1*DX2)
C restore input data and determine asymmetric errors
      CALL MTXCPV(YY,Y,N)
      CALL MTXTRA(CYY,CY,N,N)
      NSTEP=100
      W=0.D0
      CALL LSQASG(Y,CY,GY,F,E,M,N,NR,NRED,LIST,X,CX,R,W,DXPLUS,
     +DXMINS,A2,NSTEP)
      WRITE(*,*)' asymmetric errors:'
      WRITE(*,'(A,2F10.5)')' DXPLUS = ',DXPLUS
      WRITE(*,'(A,2F10.5)')' DXMINS = ',DXMINS
      WRITE(*,*)' press Y to see plot of confidence region'
      WRITE(*,*)' press N to stop  '
      READ(*,'(A)')ANSWER
      IF(ANSWER.EQ.YES1 .OR. ANSWER.EQ.YES2) THEN
C plot confidence region
C ask for number of workstation
        CALL GRNBWS()
        WRITE(*,*)' Please, enter number of workstation:'
        WRITE(*,*)' >'
        READ(*,*)NWS
        CALL GROPEN
        CALL GROPWS(NWS)
        XMIN=-1.5D0
        YMIN=0.D0
        XMAX=1.5D0
        YMAX=3.D0
        CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
        CALL GRVWWC(0.2D0,0.9D0,0.2D0,0.9D0)
        CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
        CALL GRFRAM
        CALL GRBOUN
        STRING='x_1'
        CALL GRSCLX(STRING,75)
        STRING='x_2'
        CALL GRSCLY(STRING,75)
C draw solution with symmetric errors and covariance ellipse
        CALL GRSTCL(2)
        CALL GRDATP(1,.5D0,X1,X2,DX1,DX2,RH)
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
30      CONTINUE
        FCONT=R+1.D0
        NX=30
        NY=30
        DX=(XMAX-XMIN)/NX
        DY=(YMAX-YMIN)/NY
        CALL GRSTCL(4)
C retore input data
        CALL MTXCPV(YY,Y,N)
        CALL MTXTRA(CYY,CY,N,N)
C draw confidence region
        CALL LSQCOG(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,
     +  Y,CY,GY,F,E,M,N,NR,X,A2)
        CALL GRCLSE
      END IF
      END
