      PROGRAM E3TEST
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(1000),HIST(100),XPL(101),YPL(101)
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program E3TEST simulates set of measurements'
      WRITE(*,*)' and performs chi-squared test'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number NEPX of experiments (>0)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)
     +' Enter number N of measurements per experiment (>0)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter mean A for generation'
      WRITE(*,*)' >'
      READ(*,*)A
      WRITE(*,*)' Enter hypothesis A0 for mean'
      WRITE(*,*)' >'
      READ(*,*)A0
      WRITE(*,*)
     +' Enter standard deviation SIGMA for generation (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)
     +' Enter hypothesis SIGMA0 for standard deviation (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA0
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - histogram of chi squared'
      WRITE(*,*)' 2 - histogram of chi-squared probability'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
C initialize histogram
      CAPT(1:33)='n_exp#=*****, n=*****, a=******, '
      CAPT(34:71)='&s@=******, a_0#=******, &s@_0#=******'
      WRITE(CAPT(8:12),'(I5)')NEXP
      WRITE(CAPT(17:21),'(I5)')N
      WRITE(CAPT(26:31),'(F6.2)')A
      WRITE(CAPT(38:43),'(F6.2)')SIGMA
      WRITE(CAPT(51:56),'(F6.2)')A0
      WRITE(CAPT(66:71),'(F6.2)')SIGMA0
      LCAPT=71
      IF(ISWIT.EQ.1) THEN
        H1=DBLE(4*N)
        TX='&c@^2'
        TY='N(&c@^2#)'
        LTX=5
        LTY=9
      ELSE
        H1=1.D0
        TX='P(&c@^2#)'
        TY='N(P)'
        LTX=9
        LTY=4
      END IF
      H0=0.D0
      DELH=0.01D0*H1
      NH=100
      CALL SMHSIN(HIST,H0,DELH,NH)
C loop over all experiments
      DO 30 IEXP=1,NEXP
C simulate sample R
        CALL RNSTNR(R,N)
        DO 10 I=1,N
          R(I)=A+R(I)*SIGMA
10      CONTINUE
C compute chi squared
        CHI2=0.D0
        DO 20 I=1,N
          CHI2=CHI2+((R(I)-A0)/SIGMA0)**2
20      CONTINUE
        IF(ISWIT.EQ.1) THEN
C enter chi squared into histogram
          CALL SMHSFL(HIST,H0,DELH,NH,CHI2,1.D0)
        ELSE
C compute probability and enter it into histogram
          P=1.D0-SCCHI2(CHI2,N)
          CALL SMHSFL(HIST,H0,DELH,NH,P,1.D0)
        END IF
30    CONTINUE
      FACT=(H1-H0)*DBLE(NEXP)/DBLE(NH)
      IF(ISWIT.EQ.1) THEN
C curve of chi-squared distribution
        NPL=NH+1
        DO 40 IPT=1,NPL
          XPL(IPT)=H0+(IPT-1)*DELH
          IF(IPT.EQ. 1.AND. N.EQ.1) XPL(IPT)=1.D-5
          YPL(IPT)=FACT*SDCHI2(XPL(IPT),N)
40      CONTINUE
      ELSE
C curve of constant probability
        XPL(1)=H0
        XPL(2)=H1
        YPL(1)=FACT
        YPL(2)=YPL(1)
        NPL=2
      END IF
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL GRHSCV(XPL,YPL,NPL,HIST,H0,DELH,NH,TX,LTX,TY,LTY,
     +CAPT,LCAPT,NWS)
      END
