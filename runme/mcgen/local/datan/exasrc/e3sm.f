      PROGRAM E3SM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION C(2,2),DPLUS(2,2),X(2),A(2),SIGMA(2)
      CHARACTER*75 TX,TY,CAPT
      DIMENSION XS(1000),YS(1000)
C identify program to user
      WRITE(*,*)' Program E3SM demonstrates the use of SMSDGR'
      WRITE(*,*)' by showing scatter diagram of sample'
      WRITE(*,*)' from bivariate normal distribution'
      WRITE(*,*)' '
C ask for input
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
      WRITE(*,*)' Enter number NPT of points (<1001)'
      WRITE(*,*)' >'
      READ(*,*)NPT
C N=2 defines number of variables in multivariate
C normal distribution
      N=2
C set up covariance matrix
      C(1,1)=SIGMA(1)**2
      C(2,2)=SIGMA(2)**2
      C(1,2)=RHO*SIGMA(1)*SIGMA(2)
      C(2,1)=C(1,2)
C prepare for generation of random numbers
      CALL RNMNPR(C,DPLUS,N)
C generate sample of 2-vectors from bivariate normal
      DO 10 I=1,NPT
        CALL RNMNGN(DPLUS,A,X,N)
        XS(I)=X(1)
        YS(I)=X(2)
10    CONTINUE
C prepare texts
      TX='x_1'
      TY='x_2'
      WRITE(CAPT(1:26),'(A)')'a_1#=******, a_2#=******, '
      WRITE(CAPT(27:60),'(A)')
     *'&s_1#=*****, &s_2#=*****, &r=*****'
      WRITE(CAPT(6:11),'(F6.2)')A(1)
      WRITE(CAPT(19:24),'(F6.2)')A(2)
      WRITE(CAPT(33:37),'(F5.2)')SIGMA(1)
      WRITE(CAPT(46:50),'(F5.2)')SIGMA(2)
      WRITE(CAPT(56:60),'(F5.2)')RHO
C prepare scales
      XA=A(1)-5.D0*SIGMA(1)
      XB=A(1)+5.D0*SIGMA(1)
      YA=A(2)-5.D0*SIGMA(2)
      YB=A(2)+5.D0*SIGMA(2)
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL SMSDGR(XS,YS,NPT,XA,XB,YA,YB,TX,75,TY,75,CAPT,60,NWS)
      END
