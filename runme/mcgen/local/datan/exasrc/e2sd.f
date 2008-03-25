      PROGRAM E2SD
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)' Program E2SD demonstrates use of'
      WRITE(*,*)' SDSTNR,SCSTNR,SQSTNR,SDNORM,SCNORM,SQNORM,'
      WRITE(*,*)' SDCHI2,SCCHI2,SQCHI2,SDFTST,SCFTST,SQFTST,'
      WRITE(*,*)' SDSTUD,SCSTUD,SQSTUD'
      WRITE(*,*)' and provides interactively numerical values'
      WRITE(*,*)
     +' of statistical functions of a continuous variable'
      WRITE(*,*)' '
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - standardized normal distribution'
      WRITE(*,*)' 2 - normal distribution'
      WRITE(*,*)' 3 - chi-squared distribution'
      WRITE(*,*)' 4 - F distribution'
      WRITE(*,*)' 5 - t distribution'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - probability density'
      WRITE(*,*)' 2 - distribution function'
      WRITE(*,*)' 3 - quantile'
      WRITE(*,*)' >'
      READ(*,*)ISWIT1
      IF(ISWIT.EQ.1) THEN
C standardized normal
        IF(ISWIT1.LE.2) THEN
          WRITE(*,*)' Enter x'
          WRITE(*,*)' >'
          READ(*,*)X
          IF(ISWIT1.EQ.1) THEN
            F=SDSTNR(X)
            WRITE(*,*)' f(x) = ',F
          ELSE IF(ISWIT1.EQ.2) THEN
            F=SCSTNR(X)
            WRITE(*,*)' F(x) = ',F
          END IF
        ELSE IF(ISWIT1.EQ.3) THEN
          WRITE(*,*)' Enter P (0.<P<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          F=SQSTNR(P)
          WRITE(*,*)' x_P = ',F
        END IF
      ELSE IF(ISWIT.EQ.2) THEN
C normal
        IF(ISWIT1.LE.2) THEN
          WRITE(*,*)' Enter x0'
          WRITE(*,*)' >'
          READ(*,*)X0
          WRITE(*,*)' Enter sigma (>0.)'
          WRITE(*,*)' >'
          READ(*,*)SIGMA
          WRITE(*,*)' Enter x'
          WRITE(*,*)' >'
          READ(*,*)X
          IF(ISWIT1.EQ.1) THEN
            F=SDNORM(X,X0,SIGMA)
            WRITE(*,*)' f(x;x0,sigma) = ',F
          ELSE IF(ISWIT1.EQ.2) THEN
            F=SCNORM(X,X0,SIGMA)
            WRITE(*,*)' F(x;x0,sigma) = ',F
          END IF
        ELSE IF(ISWIT1.EQ.3) THEN
          WRITE(*,*)' Enter x0'
          WRITE(*,*)' >'
          READ(*,*)X0
          WRITE(*,*)' Enter sigma (>0.)'
          WRITE(*,*)' >'
          READ(*,*)SIGMA
          WRITE(*,*)' Enter P (0.<P<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          F=SQNORM(P,X0,SIGMA)
          WRITE(*,*)' x_P (x0,sigma)= ',F
        END IF
      ELSE IF(ISWIT.EQ.3) THEN
C chi-squared
        IF(ISWIT1.LE.2) THEN
          WRITE(*,*)' Enter x (>= 0.)'
          WRITE(*,*)' >'
          READ(*,*)X
          WRITE(*,*)' Enter n (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)N
          IF(ISWIT1.EQ.1) THEN
            F=SDCHI2(X,N)
            WRITE(*,*)' f(chi-squared;n) = ',F
          ELSE IF(ISWIT1.EQ.2) THEN
            F=SCCHI2(X,N)
            WRITE(*,*)' F(chi-squared;n) = ',F
          END IF
        ELSE IF(ISWIT1.EQ.3) THEN
          WRITE(*,*)' Enter n (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)N
          WRITE(*,*)' Enter P (0.<P<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          F=SQCHI2(P,N)
          WRITE(*,*)' chi-squared_P (n)= ',F
        END IF
      ELSE IF(ISWIT.EQ.4) THEN
C F distribution
        IF(ISWIT1.LE.2) THEN
          WRITE(*,*)' Enter F (>0.)'
          WRITE(*,*)' >'
          READ(*,*)XF
          WRITE(*,*)' Enter f1 (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)NF1
          WRITE(*,*)' Enter f2 (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)NF2
          IF(ISWIT1.EQ.1) THEN
            F=SDFTST(XF,NF1,NF2)
            WRITE(*,*)' f(F;f1,f2) = ',F
          ELSE IF(ISWIT1.EQ.2) THEN
            F=SCFTST(XF,NF1,NF2)
            WRITE(*,*)' F(F;f1,f2) = ',F
          END IF
        ELSE IF(ISWIT1.EQ.3) THEN
          WRITE(*,*)' Enter f1 (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)NF1
          WRITE(*,*)' Enter f2 (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)NF2
          WRITE(*,*)' Enter P (0.<P<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          F=SQFTST(P,NF1,NF2)
          WRITE(*,*)' F_P (f1,f2)= ',F
        END IF
      ELSE IF(ISWIT.EQ.5) THEN
C t distribution
        IF(ISWIT1.LE.2) THEN
          WRITE(*,*)' Enter t'
          WRITE(*,*)' >'
          READ(*,*)T
          WRITE(*,*)' Enter n (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)N
          IF(ISWIT1.EQ.1) THEN
            F=SDSTUD(T,N)
            WRITE(*,*)' f(t;n) = ',F
          ELSE IF(ISWIT1.EQ.2) THEN
            F=SCSTUD(T,N)
            WRITE(*,*)' F(t;n) = ',F
          END IF
        ELSE IF(ISWIT1.EQ.3) THEN
          WRITE(*,*)' Enter n (>= 1)'
          WRITE(*,*)' >'
          READ(*,*)N
          WRITE(*,*)' Enter P (0.<P<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          F=SQSTUD(P,N)
          WRITE(*,*)' t_P (n)= ',F
        END IF
      END IF
      END
