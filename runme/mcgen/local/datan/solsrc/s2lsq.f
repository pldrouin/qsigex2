      PROGRAM S2LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=1,MAXN=100)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DIMENSION C(MAXN),Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR)
      CHARACTER*75 TX,TY,CAPT
      DIMENSION DATSX(MAXN),DATCOV(MAXN)
      DIMENSION XPL(200),YPL(200)
      LOGICAL OK
      DATA NR /1/
C identify program to user
      WRITE(*,*)' Program S2LSQ generates data'
      WRITE(*,*)' and fits a power law (linear case)'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number n of data points (2 <= n <= 100))'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter first value T0 of controlled variable'
      WRITE(*,*)' >'
      READ(*,*)T0
      WRITE(*,*)' Enter step width DELTAT of controlled variable'
      WRITE(*,*)' >'
      READ(*,*)DELTAT
      WRITE(*,*)' Enter coefficient X'
      WRITE(*,*)' >'
      READ(*,*)X(1)
      WRITE(*,*)' Enter exponent W'
      WRITE(*,*)' >'
      READ(*,*)W
      WRITE(*,*)' Enter size of measurement errors'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C generate data points corresponding to power law
      CALL RNSTNR(DELTAY,N)
      DO 10 I=1,N
        T(I)=T0+DBLE(I-1)*DELTAT
        Y(I)=X(1)*(T(I)**W)
        Y(I)=Y(I)+DELTAY(I)*SIGMA
        DELTAY(I)=SIGMA
10    CONTINUE
C set up A and C
      DO 15 I=1,N
        C(I)=-Y(I)
        A(I,1)=-(T(I)**W)
15    CONTINUE
C perform fit
      CALL LSQLIN(T,C,DELTAY,N,NR,X,CX,R,A,SCRAT,OK)
C compute chi**2 probability
      IF(NR.EQ.N) THEN
        P=0.D0
      ELSE
        P=1.D0-SCCHI2(R,N-NR)
      END IF
C prepare graphics
      TX='t'
      TY='y'
      LTX=1
      LTY=1
      CAPT='x#=******, &D@x=******, M=******, P=******'
      WRITE(CAPT(4:9),'(F6.2)')X(1)
      WRITE(CAPT(17:22),'(F6.2)')SQRT(CX(1,1))
      WRITE(CAPT(27:32),'(F6.2)')R
      WRITE(CAPT(37:42),'(F6.4)')P
      LCAPT=42
C curve corresponding to solution
      NPL=200
CCCC          DT=(DBLE(N-1)/DBLE(NPL-1))*DELTAT
      DT=(DBLE(N)/DBLE(NPL-1))*DELTAT
      DO 20 IPL=1,NPL
        XPL(IPL)=T0+DT*DBLE(IPL)
        YPL(IPL)=X(1)*(XPL(IPL)**W)
20    CONTINUE
C prepare data points for graphical presentation
      DO 30 I=1,N
        DATSX(I)=0.D0
        DATCOV(I)=0.D0
30    CONTINUE
C graphical output
      CALL GRDTCV(XPL,YPL,NPL,1,.5D0,T,Y,DATSX,DELTAY,DATCOV,N,
     +TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
