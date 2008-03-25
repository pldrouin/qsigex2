      PROGRAM S2ML
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION C(2,2),DPLUS(2,2),X(2),A(2),SIGMA(2)
      DIMENSION HIST(100)
      CHARACTER*75 TX,TY,CAPT
      DIMENSION XSAMPL(10000,2),XBAR(2),SPSQ(2),SPRIME(2)
      PARAMETER(ZERO=0.D0)
C identify program to user
      WRITE(*,*)' Program S2MLC program plots histogram'
      WRITE(*,*)' of sample correlation coefficient'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number NEXP of simulated experiments (>>1)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)
     +' Enter number NPT of points in each experiment (3<=NPT<=10000)'
      WRITE(*,*)' >'
      READ(*,*)NPT
      WRITE(*,*)' Enter a1'
      WRITE(*,*)' >'
      READ(*,*)A(1)
      WRITE(*,*)' Enter a2'
      WRITE(*,*)' >'
      READ(*,*)A(2)
      WRITE(*,*)' Enter sigma1 (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA(1)
      WRITE(*,*)' Enter sigma2 (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA(2)
      WRITE(*,*)' Enter rho (-1.<rho<1.)'
      WRITE(*,*)' >'
      READ(*,*)RHO
C N=2 defines number of variables in multivariate normal distribution
      N=2
C set up covariance matrix
      C(1,1)=SIGMA(1)**2
      C(2,2)=SIGMA(2)**2
      C(1,2)=RHO*SIGMA(1)*SIGMA(2)
      C(2,1)=C(1,2)
C prepare for generation of random numbers
      CALL RNMNPR(C,DPLUS,N)
C initialize histogram
      X0=-1.5D0
      DELX=0.03
      NX=100
      CALL SMHSIN(HIST,X0,DELX,NX)
C loop over all simulation experiments
      DO 30 IEXP=1,NEXP
C generate sample of 2-vectors from bivariate normal
C and compute sample means
        XBAR(1)=ZERO
        XBAR(2)=ZERO
        DO 10 I=1,NPT
          CALL RNMNGN(DPLUS,A,X,N)
          XSAMPL(I,1)=X(1)
          XSAMPL(I,2)=X(2)
          XBAR(1)=XBAR(1)+X(1)
          XBAR(2)=XBAR(2)+X(2)
10      CONTINUE
        XBAR(1)=XBAR(1)/DBLE(NPT)
        XBAR(2)=XBAR(2)/DBLE(NPT)
C compute sample variances and correlation coefficient
        SPSQ(1)=ZERO
        SPSQ(2)=ZERO
        R=ZERO
        DO 20 I=1,NPT
          SPSQ(1)=SPSQ(1)+(XSAMPL(I,1)-XBAR(1))**2
          SPSQ(2)=SPSQ(2)+(XSAMPL(I,2)-XBAR(2))**2
          R=R+(XSAMPL(I,1)-XBAR(1))*(XSAMPL(I,2)-XBAR(2))
20      CONTINUE
        SPSQ(1)=SPSQ(1)/DBLE(NPT)
        SPSQ(2)=SPSQ(2)/DBLE(NPT)
        SPRIME(1)=SQRT(SPSQ(1))
        SPRIME(2)=SQRT(SPSQ(2))
        R=R/(DBLE(NPT)*SPRIME(1)*SPRIME(2))
        CALL SMHSFL(HIST,X0,DELX,NX,R,1.D0)
30    CONTINUE
C prepare texts
      TX='r'
      TY='N(r)'
      WRITE(CAPT(1:60),'(A)')
     +'a_1#=******, a_2#=******, &s_1#=*****, &s_2#=*****, &r=*****'
      WRITE(CAPT(6:11),'(F6.2)')A(1)
      WRITE(CAPT(19:24),'(F6.2)')A(2)
      WRITE(CAPT(33:37),'(F5.2)')SIGMA(1)
      WRITE(CAPT(46:50),'(F5.2)')SIGMA(2)
      WRITE(CAPT(56:60),'(F5.2)')RHO
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL SMHSGR(HIST,X0,DELX,NX,TX,75,TY,75,CAPT,60,NWS)
      END
