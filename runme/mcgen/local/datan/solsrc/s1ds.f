      PROGRAM S1DS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(1000),HIST(120),XPL(121),YPL(121)
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program S1DS performs Monte Carlo folding '
      WRITE(*,*)' of uniform distributions'
      WRITE(*,*)' '
      WRITE(*,*)' Enter number NEPX of experiments (>>1)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)' Enter number N of random numbers per experiment (>0)'
      WRITE(*,*)' >'
      READ(*,*)N
C initialize histogram
      TX='x'
      TY='N(x)'
      LTX=1
      LTY=4
      CAPT(1:21)='n_exp#=*******, n=*****'
      WRITE(CAPT(8:14),'(I7)')NEXP
      WRITE(CAPT(19:23),'(I5)')N
      LCAPT=23
      H0=-3.D0
      DELH=.05D0
      NH=120
      CALL SMHSIN(HIST,H0,DELH,NH)
      B=SQRT(3.D0/DBLE(N))
C loop over all experiments
      DO 30 IEXP=1,NEXP
C simulate sample R
        CALL RNECUY(R,N)
        X=0.D0
        DO 10 I=1,N
          X=X+B*(2.D0*R(I)-1.D0)
10      CONTINUE
C enter chi squared into histogram
        CALL SMHSFL(HIST,H0,DELH,NH,X,1.D0)
30    CONTINUE
C curve of normal distribution
      NPL=NH+1
      FACT=DBLE(NEXP)*DELH
      DO 40 IPT=1,NPL
        XPL(IPT)=H0+(IPT-1)*DELH
        YPL(IPT)=FACT*SDSTNR(XPL(IPT))
40    CONTINUE
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL GRHSCV(XPL,YPL,NPL,HIST,H0,DELH,NH,TX,LTX,TY,LTY,CAPT,
     +LCAPT,NWS)
      END
