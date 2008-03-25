      PROGRAM E1GAM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)' Program E1GAM demonstrates use of'
      WRITE(*,*)' GGAMMA and GLNGAM'
      WRITE(*,*)' and provides interactively numerical values'
      WRITE(*,*)' of the gamma function and its logarithm'
      WRITE(*,*)' '
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - gamma function'
      WRITE(*,*)' 2 - log of gamma function'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      WRITE(*,*)' Enter x'
      WRITE(*,*)' >'
      READ(*,*)X
      IF(ISWIT.EQ.1) THEN
C gamma
        F=GGAMMA(X)
        WRITE(*,*)' gamma(x) = ',F
      ELSE IF(ISWIT.EQ.2) THEN
C log gamma
        F=GLNGAM(X)
        WRITE(*,*)' log (gamma(x)) = ',F
      END IF
      END
