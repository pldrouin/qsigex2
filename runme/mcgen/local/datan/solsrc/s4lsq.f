      PROGRAM S4LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=2,MAXN=21,N=21,T0=-3.D0,DELTAT=0.3D0)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DIMENSION Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR)
      CHARACTER*75 TX,TY,CAPT
      DIMENSION DATSX(MAXN),DATCOV(MAXN)
      DIMENSION XPL(100),YPL(100)
      DIMENSION LIST(MAXNR)
      EXTERNAL BWFNCT,GSFNCT
      DATA NR /2/
C identify program to user
      WRITE(*,*)' Program S4LSQ generates data'
      WRITE(*,*)' corresponding to a Breit-Wigner law'
      WRITE(*,*)' and fits a Breit-Wigner  OR a Gaussian'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter size of measurement errors (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - Fit to Breit-Wigner function'
      WRITE(*,*)' 2 - Fit to Gaussian'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
C generate data points corresponding to Breit-Wigner
      X(1)=0.0D0
      X(2)=1.0D0
      CALL RNSTNR(DELTAY,N)
      DO 10 I=1,N
        T(I)=T0+DBLE(I-1)*DELTAT
        Y(I)=BWFNCT(X,NR,T(I))
        Y(I)=Y(I)+DELTAY(I)*SIGMA
        DELTAY(I)=SIGMA
10    CONTINUE
C set first approximation of unknowns
      X(1)=0.5D0
      X(2)=0.5D0
C perform fit
      NSTEP=100
      IF(ISWIT.EQ.1) THEN
        CALL LSQNON(BWFNCT,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,A,SCRAT,
     +  NSTEP)
      ELSE
        CALL LSQNON(GSFNCT,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,A,SCRAT,
     +  NSTEP)
      END IF
      P=1.D0-SCCHI2(R,N-NR)
      IF(NSTEP.LT.0) THEN
        WRITE(*,'(A,I5)')' LSQNON ended with NSTEP = ',NSTEP
      ELSE
C prepare graphics
        TX='t'
        TY='y'
        LTX=1
        LTY=1
        CAPT='x_1#=******, x_2#=******, M=******, P=********'
        WRITE(CAPT(6:11),'(F6.2)')X(1)
        WRITE(CAPT(19:24),'(F6.2)')X(2)
        WRITE(CAPT(29:34),'(F6.2)')R
        WRITE(CAPT(39:46),'(F8.6)')P
        IF(ISWIT.EQ.1) THEN
          WRITE(CAPT(47:69),'(A)')' (fit to Breit-Wigner)'
        ELSE
          WRITE(CAPT(47:69),'(A)')' (fit to Gaussian)    '
        END IF
        LCAPT=69
C curve corresponding to solution
        NPL=100
        DO 20 IPL=1,NPL
          XPL(IPL)=-4.D0+0.08D0*DBLE(IPL)
          IF(ISWIT.EQ.1) THEN
            YPL(IPL)=BWFNCT(X,NR,XPL(IPL))
          ELSE
            YPL(IPL)=GSFNCT(X,NR,XPL(IPL))
          END IF
20      CONTINUE
C prepare data points for graphical presentation
        DO 30 I=1,N
          DATSX(I)=0.D0
          DATCOV(I)=0.D0
30      CONTINUE
C ask for number of workstation
        CALL GRNBWS()
        WRITE(*,*)' Please, enter number of workstation:'
        WRITE(*,*)' >'
        READ(*,*)NWS
C graphical output
        CALL GRDTCV(XPL,YPL,NPL,1,.5D0,T,Y,DATSX,DELTAY,DATCOV,N,
     +  TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END IF
      END
C-------------------------------------------------
      DOUBLE PRECISION FUNCTION BWFNCT(X,NR,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*)
      BWFNCT=2.D0*X(2)**2/((3.14159*X(2))*(4.D0*(T-X(1))**2+X(2)**2))
      END
C-------------------------------------------------
      DOUBLE PRECISION FUNCTION GSFNCT(X,NR,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*)
      GSFNCT=(.39894228D0/X(2))*EXP(-0.5D0*((T-X(1))/X(2))**2)
      END
