      PROGRAM S6LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=3,MAXN=100,T0=-3.D0)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DIMENSION Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR)
      CHARACTER*75 TX,TY,CAPT
      DIMENSION RANDOM(1000),HIST(100)
      DIMENSION XPL(100),YPL(100)
      DIMENSION LIST(MAXNR)
      EXTERNAL BWFNCT
      DATA NR /3/
C identify program to user
      WRITE(*,*)' Program S6LSQ generates data'
      WRITE(*,*)' corresponding to a Breit-Wigner law'
      WRITE(*,*)' and fits a Breit-Wigner to histogram'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number NEV (<= 1000)'
      WRITE(*,*)' of events to be generated'
      WRITE(*,*)' >'
      READ(*,*)NEV
      WRITE(*,*)' Enter number NT (<= 100) of histogram bins'
      WRITE(*,*)' >'
      READ(*,*)NT
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C generate events and build histogram
      CALL RNBW(0.D0,1.D0,RANDOM,NEV)
      DELT=2.D0*ABS(T0)/DBLE(NT)
      CALL SMHSIN(HIST,T0,DELT,NT)
      DO 5 I=1,NEV
        CALL SMHSFL(HIST,T0,DELT,NT,RANDOM(I),1.D0)
5     CONTINUE
      N=0
      DO 10 I=1,NT
        IF(HIST(I).GT.0.D0) THEN
          N=N+1
          T(N)=T0+(DBLE(I)-0.5D0)*DELT
          Y(N)=HIST(I)
          DELTAY(N)=SQRT(Y(N))
        END IF
10    CONTINUE
      IF(N.LE.NR) THEN
        WRITE(*,*)' only ',N,' bins with at least one event'
        STOP
      END IF
C set first approximation of unknowns
      X(1)=0.5D0
      X(2)=0.5D0
      X(3)=DBLE(NEV)
C perform fit
      NSTEP=100
      CALL LSQNON(BWFNCT,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,A,SCRAT,
     +NSTEP)
      P=1.D0-SCCHI2(R,N-NR)
      IF(NSTEP.LT.0) THEN
        WRITE(*,'(A,I5)')' LSQNON ended with NSTEP = ',NSTEP
      ELSE
C prepare graphics
        TX='t'
        TY='y'
        LTX=1
        LTY=1
        CAPT(1:33)='x_1#=*****,x_2=#*****,x_3#=*****,'
        CAPT(34:74)='&D@x_1#=*****,&D@x_2=#*****,&D@x_3#=*****'
        WRITE(CAPT(6:10),'(F5.2)')X(1)
        WRITE(CAPT(17:21),'(F5.2)')X(2)
        WRITE(CAPT(28:32),'(F5.1)')X(3)
        WRITE(CAPT(42:46),'(F5.2)')SQRT(CX(1,1))
        WRITE(CAPT(56:60),'(F5.2)')SQRT(CX(2,2))
        WRITE(CAPT(70:74),'(F5.1)')SQRT(CX(3,3))
        LCAPT=74
C curve corresponding to solution
        NPL=100
        DO 20 IPL=1,NPL
          XPL(IPL)=-4.D0+0.08D0*DBLE(IPL)
          YPL(IPL)=BWFNCT(X,NR,XPL(IPL))
20      CONTINUE
C graphical output
        CALL GRHSCV(XPL,YPL,NPL,HIST,T0,DELT,NT,TX,LTX,TY,LTY,CAPT,
     +  LCAPT,NWS)
      END IF
      END
C ------------------------------------------------------------------
      SUBROUTINE RNBW(A,GAMMA,R,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(PI=3.14159D0,HALF=0.5D0)
      DIMENSION R(N)
C program generates random numbers corresponding to
C a Breit-Wigner distribution
      CALL RNECUY(R,N)
      DO 10 I=1,N
        R(I)=A+HALF*GAMMA*TAN(PI*(R(I)-HALF))
10    CONTINUE
      END
C-------------------------------------------------
      DOUBLE PRECISION FUNCTION BWFNCT(X,NR,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*)
      BWFNCT=X(3)*2.D0*X(2)**2/((3.14159*X(2))*(4.D0*(T-X(1))**2+X(2)
     +**2))
      END
