      PROGRAM S2DS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION HIST(100),XPL(101),YPL(101)
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program S2DS performs Monte Carlo folding '
      WRITE(*,*)' of uniform with Gaussian distribution'
      WRITE(*,*)' '
C ask for parameters
      WRITE(*,*)' Enter number NEPX of experiments (>>1)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)' Enter left edge A of uniform distribution'
      WRITE(*,*)' >'
      READ(*,*)A
      WRITE(*,*)' Enter right edge B of uniform distribution'
      WRITE(*,*)' >'
      READ(*,*)B
      WRITE(*,*)'Enter width SIGMA of Gaussian distribution (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
C initialize histogram
      TX='x'
      TY='N(x)'
      LTX=1
      LTY=4
      CAPT(1:43)='n_exp#=*****, a=******, b=******, &s=******'
      WRITE(CAPT(8:12),'(I5)')NEXP
      WRITE(CAPT(17:22),'(F6.2)')A
      WRITE(CAPT(27:32),'(F6.2)')B
      WRITE(CAPT(38:43),'(F6.2)')SIGMA
      LCAPT=43
      H0=A-3.D0*SIGMA
      H1=B+3.D0*SIGMA
      DELH=.01D0*(H1-H0)
      NH=100
      CALL SMHSIN(HIST,H0,DELH,NH)
C loop over all experiments
      DO 30 IEXP=1,NEXP
        CALL RNECUY(RUNI,1)
        RUNI=A+(B-A)*RUNI
        CALL RNSTNR(RNORM,1)
        RNORM=RNORM*SIGMA
        ENTRY=RUNI+RNORM
C fill histogram histogram
        CALL SMHSFL(HIST,H0,DELH,NH,ENTRY,1.D0)
30    CONTINUE
C curve of folding between uniform and normal distribution
      NPL=NH+1
      FACT=DBLE(NEXP)*DELH/(B-A)
      DO 40 IPT=1,NPL
        XPL(IPT)=H0+(IPT-1)*DELH
        ARG1=(B-XPL(IPT))/SIGMA
        ARG2=(A-XPL(IPT))/SIGMA
        YPL(IPT)=FACT*(SCSTNR(ARG1)-SCSTNR(ARG2))
40    CONTINUE
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL GRHSCV(XPL,YPL,NPL,HIST,H0,DELH,NH,TX,LTX,TY,LTY,CAPT,LC
     +APT,NWS)
      END
