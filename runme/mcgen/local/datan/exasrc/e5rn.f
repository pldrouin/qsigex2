      PROGRAM E5RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION C(2,2),DPLUS(2,2),A(2),X(2)
      CHARACTER*3 ANSWER
C identify program to user
      WRITE(*,*)' Program E5RN demonstrates use '
      WRITE(*,*)' of RNMNPR, RNMNGN'
      WRITE(*,*)' '
C show current seeds
      CALL RNE2OT(ISEED1,ISEED2)
      WRITE(*,*)' Current seeds are ',ISEED1,ISEED2
C ask for new seed(s)
      WRITE(*,*)' Do you want to enter new seed(s) (y/n)?'
      WRITE(*,*)' >'
      READ(*,'(A)') ANSWER
      IF(ANSWER(1:1).EQ.'Y' .OR. ANSWER(1:1).EQ.'y') THEN
C read new seeds and initiate generator
        WRITE(*,*)' Enter two integers (>0) as seeds'
        WRITE(*,*)' >'
        READ(*,*) ISEED1,ISEED2
        CALL RNE2IN(ISEED1,ISEED2)
      END IF
C ask for mean and covariance matrix
      WRITE(*,*)' Pairs of correlated random numbers'
      WRITE(*,*)' will be generated.'
      WRITE(*,*)' Enter element C11 (>0.) of covariance matrix'
      WRITE(*,*)' (variance of first number in pair)'
      WRITE(*,*)' >'
      READ(*,*) C(1,1)
      WRITE(*,*)' Enter element C22 (>0.)of covariance matrix'
      WRITE(*,*)' (variance of second number in pair)'
      WRITE(*,*)' >'
      READ(*,*) C(2,2)
      WRITE(*,*)' Enter element C12 of covariance matrix'
      WRITE(*,*)' (covariance of the two numbers)'
      WRITE(*,*)' (C12=C11*C22*rho, -1.<rho<1.) '
      WRITE(*,*)' >'
      READ(*,*) C(1,2)
      C(2,1)=C(1,2)
      WRITE(*,*)' Enter mean value for first number'
      WRITE(*,*)' >'
      READ(*,*)A(1)
      WRITE(*,*)' Enter mean value for second number'
      WRITE(*,*)' >'
      READ(*,*)A(2)
C initiate generator for multivariate normally distributes numbers
      N=2
      CALL RNMNPR(C,DPLUS,N)
C generate and write random numbers
      WRITE(*,*)' Pairs of random numbers are'
      DO 10 I=1,20
        CALL RNMNGN(DPLUS,A,X,N)
        WRITE(*,*)X
10    CONTINUE
      END
