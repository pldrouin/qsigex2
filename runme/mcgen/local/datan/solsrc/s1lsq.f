      PROGRAM S1LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=3,MAXN=9,N=9,T0=-4.D0,DELTAT=1.D0)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DIMENSION Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR),HIST(100)
      DIMENSION XIN(MAXNR)
      DIMENSION XPL(2),YPL(2)
      CHARACTER*75 TX,TY,CAPT
      LOGICAL OK
C identify program to user
      WRITE(*,*)' Program S1LSQ generates data'
      WRITE(*,*)' corresponding to 2nd degree polynomial'
      WRITE(*,*)' and fits 1st degree polynomial'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number NEXP of experiments (>>1)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)' Enter X1 for points to be generated'
      WRITE(*,*)' >'
      READ(*,*)XIN(1)
      WRITE(*,*)' Enter X2 for points to be generated'
      WRITE(*,*)' >'
      READ(*,*)XIN(2)
      WRITE(*,*)' Enter X3 for points to be generated'
      WRITE(*,*)' >'
      READ(*,*)XIN(3)
      WRITE(*,*)' Enter SIGMA for points to be generated (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMAG
      WRITE(*,*)' Enter DELTAY to be used in fit (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMAF
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C initialize histogram and prepare graphics
      H1=1.D0
      TX='P(&c@^2#)'
      TY='N(P)'
      LTX=9
      LTY=4
      CAPT(1:45)='Input: x_1#=******, x_2#=******, x_3#=******,'
      CAPT(46:68)=' &s=******, &D@y=******'
      WRITE(CAPT(13:18),'(F6.2)')XIN(1)
      WRITE(CAPT(26:31),'(F6.2)')XIN(2)
      WRITE(CAPT(39:44),'(F6.2)')XIN(3)
      WRITE(CAPT(50:55),'(F6.2)')SIGMAG
      WRITE(CAPT(63:68),'(F6.2)')SIGMAF
      LCAPT=68
      H0=0.D0
      DELH=0.01D0*H1
      NH=100
C curve of constant probability
      FACT=(H1-H0)*DBLE(NEXP)/DBLE(NH)
      XPL(1)=H0
      XPL(2)=H1
      YPL(1)=FACT
      YPL(2)=YPL(1)
      NPL=2
      CALL SMHSIN(HIST,H0,DELH,NH)
C loop over all simulated experiments
      DO 100 IEXP=1,NEXP
C generate data points corresponding to polynomial
        NR=3
        CALL RNSTNR(DELTAY,N)
        DO 20 I=1,N
          T(I)=T0+DBLE(I-1)*DELTAT
          Y(I)=XIN(1)
          IF(NR.GT.1) THEN
            DO 10 K=2,NR
              Y(I)=Y(I)+XIN(K)*T(I)**(K-1)
10          CONTINUE
          END IF
          Y(I)=Y(I)+DELTAY(I)*SIGMAG
          DELTAY(I)=SIGMAF
20      CONTINUE
C perform polynomial fit
        NR=2
        CALL LSQPOL(T,Y,DELTAY,N,NR,X,CX,R,A,SCRAT,OK)
C compute chi-squared probability and enter it in histogram
        P=1.D0-SCCHI2(R,N-NR)
        CALL SMHSFL(HIST,H0,DELH,NH,P,1.D0)
100   CONTINUE
C graphical output
      CALL GRHSCV(XPL,YPL,NPL,HIST,H0,DELH,NH,TX,LTX,TY,LTY,CAPT,
     +LCAPT,NWS)
      END
