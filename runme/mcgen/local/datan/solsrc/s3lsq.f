      PROGRAM S3LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=2,MAXN=100)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DIMENSION Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR)
      DIMENSION TLOG(MAXN),YLOG(MAXN),DELLOG(MAXN)
      CHARACTER*75 TX,TY,CAPT
      DIMENSION DATSX(MAXN),DATCOV(MAXN)
      DIMENSION XPL(200),YPL(200)
      DIMENSION LIST(MAXNR)
      LOGICAL OK
      EXTERNAL POWERL
      DATA NR /2/
C identify program to user
      WRITE(*,*)' Program S3LSQ generates data'
      WRITE(*,*)' and fits a power law (nonlinear case)'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number n of data points (3 <= n <= 100)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter first value T0 of controlled variable'
      WRITE(*,*)' >'
      READ(*,*)T0
      WRITE(*,*)' Enter step width DELTAT of controlled variable'
      WRITE(*,*)' >'
      READ(*,*)DELTAT
      WRITE(*,*)' Enter coefficient X(1)'
      WRITE(*,*)' >'
      READ(*,*)X(1)
      WRITE(*,*)' Enter exponent X(2)'
      WRITE(*,*)' >'
      READ(*,*)X(2)
      WRITE(*,*)' Enter size of measurement errors (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
C generate data points corresponding to power law
      CALL RNSTNR(DELTAY,N)
      DO 10 I=1,N
        T(I)=T0+DBLE(I-1)*DELTAT
        Y(I)=POWERL(X,NR,T(I))
        Y(I)=Y(I)+DELTAY(I)*SIGMA
        DELTAY(I)=SIGMA
10    CONTINUE
C find first approximation of unknowns by straight line in log-log plot
      NPOS=0
      DO 12 I=1,N
        IF(T(I).GT.0.D0 .AND. Y(I).GT.0.D0) THEN
          NPOS=NPOS+1
          TLOG(NPOS)=LOG(T(I))
          YLOG(NPOS)=LOG(Y(I))
          DELLOG(NPOS)=1.D0
        END IF
12    CONTINUE
      CALL LSQPOL(TLOG,YLOG,DELLOG,NPOS,NR,X,CX,R,A,SCRAT,OK)
      X(1)=EXP(X(1))
C perform fit
      NSTEP=100
      CALL LSQNON(POWERL,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,A,SCRAT,
     +NSTEP)
      IF(N.NE.NR) THEN
        P=1.D0-SCCHI2(R,N-NR)
        DX1=SQRT(CX(1,1))
        DX2=SQRT(CX(2,2))
        RHO=CX(1,2)/(DX1*DX2)
      END IF
C prepare graphics
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      TX='t'
      TY='y'
      LTX=1
      LTY=1
      CAPT(1:24)='x_1#=******, x_2#=******'
      WRITE(CAPT(6:11),'(F6.2)')X(1)
      WRITE(CAPT(19:24),'(F6.2)')X(2)
      LCAPT=24
      IF(N.NE.NR) THEN
        CAPT(25:69)=
     *', &D@x_1#=******, &D@x_2#=******, &r@=******'
        WRITE(CAPT(35:40),'(F6.2)')DX1
        WRITE(CAPT(51:56),'(F6.2)')DX2
        WRITE(CAPT(63:68),'(F6.2)')RHO
        LCAPT=68
      END IF
C curve corresponding to solution
      NPL=200
      DT=(DBLE(N-1)/DBLE(NPL-1))*DELTAT
      DO 20 IPL=1,NPL
        XPL(IPL)=T0+DT*DBLE(IPL)
        YPL(IPL)=POWERL(X,NR,XPL(IPL))
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
C---------------------------------------------
      DOUBLE PRECISION FUNCTION POWERL(X,NR,T)
      DOUBLE PRECISION X,T
      DIMENSION X(*)
      POWERL=X(1)*T**X(2)
      END
