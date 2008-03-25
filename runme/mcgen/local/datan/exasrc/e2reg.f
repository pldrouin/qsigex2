      PROGRAM E2REG
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(N=10,NR=10,NPL=200)
      DIMENSION T(N),Y(N),DELTAY(N),X(NR),CHI2(NR)
      DIMENSION B(NR,NR),A(N,NR)
      DIMENSION XPL(NPL,NR),YPL(NPL,NR),NCOL(NR)
      DIMENSION DATSX(N),DATCOV(N)
      CHARACTER*75 TX,TY,CAPT
      PARAMETER (ZERO=0.D0,ONE=1.D0)
      DATA T
     +/-.9D0,-.7D0,-.5D0,-.3D0,-.1D0,.1D0,.3D0,.5D0,.7D0,.9D0/
      DATA Y /81.D0,50.D0,35.D0,27.D0,26.D0,
     +60.D0,106.D0,189.D0,318.D0,520.D0/
C identify program to user
      WRITE(*,*)' Program E2REG performs polynomial regression'
      WRITE(*,*)' and presents data and polynomials graphically'
      WRITE(*,*)' '
C set errors
      DO 10 I=1,N
        DELTAY(I)=SQRT(Y(I))
10    CONTINUE
C perform polynomial regression
      CALL REGPOL(T,Y,DELTAY,N,NR,X,B,A,CHI2)
C output results in alphanumeric form
      WRITE(*,*)' T '
      WRITE(*,'(10F7.2)')T
      WRITE(*,*)' Y'
      WRITE(*,'(10F7.2)')Y
      WRITE(*,*)' DELTAY'
      WRITE(*,'(10F7.2)')DELTAY
      WRITE(*,*)' X'
      WRITE(*,'(10F7.2)')X
      WRITE(*,*)' CHI2'
      WRITE(*,'(10F7.2)')CHI2
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Enter maximum number of terms in polynomials'
      WRITE(*,*)' to be plotted (>0, <11)'
      WRITE(*,*)' >'
      READ(*,*)NPOL
C set errors along abscissa and covariances to zero for graphics
      DO 15 I=1,N
        DATSX(I)=0.D0
        DATCOV(I)=0.D0
15    CONTINUE
C compute polylines for polynomials of degrees 0,1,2,....
      T0=-1.5D0
      DT=3.D0/DBLE(NPL-1)
      DO 40 I=1,NPL
        DO 30 J=1,NPOL
          XPL(I,J)=T0+(I-1)*DT
          YPL(I,J)=ZERO
          DO 27 L=1,J
            D=B(L,1)
            IF(L.GT.1) THEN
              DO 25 K=2,L
                D=D+B(L,K)*XPL(I,J)**(K-1)
25            CONTINUE
            END IF
            YPL(I,J)=YPL(I,J)+X(L)*D
27        CONTINUE
30      CONTINUE
40    CONTINUE
      DO 50 J=1,10
        NCOL(J)=4
50    CONTINUE
C prepare texts for graphics
      TX='t'
      LTX=1
      TY='y'
      LTY=1
      CAPT=' '
      LCAPT=1
      NMARK=5
      SCALEF=.5D0
C call graphics routine for data points and multiple polylines
      CALL GRDTMC(XPL,YPL,NPL,NPOL,NCOL,NMARK,SCALEF,T,Y,DATSX,
     +DELTAY,DATCOV,N,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
