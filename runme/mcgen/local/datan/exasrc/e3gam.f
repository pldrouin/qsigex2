      PROGRAM E3GAM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)' Program E3GAM demonstrates use of'
      WRITE(*,*)' GBETAF,GINCGM and GINCBT'
      WRITE(*,*)' and provides interactively numerical values'
      WRITE(*,*)' of the beta function, the incomplete gamma'
      WRITE(*,*)' function, and the incomplete beta function'
      WRITE(*,*)' '
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - beta function'
      WRITE(*,*)' 2 - incomplete gamma function'
      WRITE(*,*)' 3 - incomplete beta function'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      IF(ISWIT.EQ.1) THEN
C beta
        WRITE(*,*)' Enter z (>= 0.)'
        WRITE(*,*)' >'
        READ(*,*)Z
        WRITE(*,*)' Enter w (>= 0.)'
        WRITE(*,*)' >'
        READ(*,*)W
        F=GBETAF(Z,W)
        WRITE(*,*)' beta(z,w) = ',F
      ELSE IF(ISWIT.EQ.2) THEN
C incomplete gamma
        WRITE(*,*)' Enter a (>0.)'
        WRITE(*,*)' >'
        READ(*,*)A
        WRITE(*,*)' Enter x (>0.)'
        WRITE(*,*)' >'
        READ(*,*)X
        F=GINCGM(A,X)
        WRITE(*,*)' P(a,x) = ',F
      ELSE IF(ISWIT.EQ.3) THEN
C incomplete beta
        WRITE(*,*)' Enter a, (0.<a<1.) '
        WRITE(*,*)' >'
        READ(*,*)A
        WRITE(*,*)' Enter b, (0.<b<1.)'
        WRITE(*,*)' >'
        READ(*,*)B
        WRITE(*,*)' Enter x, (0.<x<1.)'
        WRITE(*,*)' >'
        READ(*,*)X
        F=GINCBT(A,B,X)
        WRITE(*,*)' I_x (a,b) = ',F
      END IF
      END
