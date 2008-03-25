      PROGRAM E1ML
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION T(1000),XPL(500),YPL(500)
      CHARACTER*75 TX,TY,CAPT
      PARAMETER(FACT=.39894228D0)
      EXTERNAL FUE1ML
      DOUBLE PRECISION FUE1ML
      DATA TX/'t,&t'/,TY/'-(l-l_max#)'/
C identify program to user
      WRITE(*,*)' Program E1ML computes mean life and'
      WRITE(*,*)' asymmetric errors from few radioactive decays'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number N of decays (1 <= N <= 1000)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter seed number ISEED1 (>0)'
      WRITE(*,*)' >'
      READ(*,*)ISEED1
      WRITE(*,*)' Enter seed number ISEED2 (>0)'
      WRITE(*,*)' >'
      READ(*,*)ISEED2
C initialization
      IF(ISEED1.LE.0) ISEED1=123456
      IF(ISEED2.LE.0) ISEED2=654321
      CALL RNE2IN(ISEED1,ISEED2)
C prepare graphics
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(0.D0,5.D0,0.D0,0.6D0)
      CALL GRVWWC(0.1D0,1.1D0,0.1D0,0.75D0)
      CALL GRWNWC(-0.214D0,1.2D0,-0.1D0,0.9D0)
      CALL GRSTFR(NWS,5.D0,0.D0)
      CALL GRFRAM
      CALL GRSCLX(TX,4)
      CALL GRSCLY(TY,11)
      CALL GRBOUN
C generate sample and draw 1D scatter diagram
      CALL RNECUY(T,N)
      TBAR=0.D0
      YPL(1)=0.0D0
      YPL(2)=0.1D0
      CALL GRSTCL(2)
      DO 2 I=1,N
        T(I)=-LOG(T(I))
        TBAR=TBAR+T(I)
        XPL(1)=T(I)
        XPL(2)=T(I)
        CALL GRPLIN(2,XPL,YPL)
2     CONTINUE
C draw line -(l-lmax)=.5
      TBAR=TBAR/DBLE(N)
      XPL(1)=0.D0
      XPL(2)=5.D0
      YPL(1)=.5D0
      YPL(2)=.5D0
      CALL GRPLIN(2,XPL,YPL)
C draw short line at tau=tbar
      XPL(1)=TBAR
      XPL(2)=TBAR
      YPL(1)=.52D0
      YPL(2)=.48D0
      CALL GRSTCL(4)
      CALL GRPLIN(2,XPL,YPL)
C draw likelihood function -(l-lmax)
      FMIN=DBLE(N)*(1.+LOG(TBAR))
      DO 20 I=1,500
        XPL(I)=DBLE(I)*0.01D0
        YPL(I)=DBLE(N)*(TBAR/XPL(I)+LOG(XPL(I)))-FMIN
20    CONTINUE
      CALL GRPLIN(500,XPL,YPL)
C compute asymmetric errors
      X0=.001D0
      X1=TBAR
      CALL AUXZBR(X0,X1,FUE1ML,TBAR,N,NDUM)
      CALL AUXZFN(X0,X1,SIGMNS,FUE1ML,TBAR,N,NDUM,1.D-5)
      SIGMNS=TBAR-SIGMNS
      X0=TBAR
      X1=5.D0
      CALL AUXZBR(X0,X1,FUE1ML,TBAR,N,NDUM)
      CALL AUXZFN(X0,X1,SIGPLS,FUE1ML,TBAR,N,NDUM,1.D-5)
      SIGPLS=SIGPLS-TBAR
C compute symmetric error
      SIG=TBAR/SQRT(DBLE(N))
C prepare and draw caption
      CAPT(1:39)='N =****, t^^"-# = ****, &D@_-# = *****,'
      CAPT(40:71)=' &D@_+# = *****, &Dt^"~# = *****'
      WRITE(CAPT(4:7),'(I4)')N
      WRITE(CAPT(19:23),'(F4.2)')TBAR
      WRITE(CAPT(34:38),'(F5.2)')SIGMNS
      WRITE(CAPT(50:54),'(F5.2)')SIGPLS
      WRITE(CAPT(67:71),'(F5.2)')SIG
      CALL GRSTCL(1)
      CALL GRTXTC(1.D0,CAPT,71)
      CALL GRCLSE
      END
C-----------------------------------------------------------
      DOUBLE PRECISION FUNCTION FUE1ML(TAU,TBAR,N,NDUM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      FUE1ML=DBLE(N)*(TBAR/TAU+LOG(TAU/TBAR)-1.D0)-.5D0
      END
