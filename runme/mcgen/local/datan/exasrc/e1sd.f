      PROGRAM E1SD
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)' Program E1SD demonstrates use of'
      WRITE(*,*)' SDBINM,SCBINM,SDHYPG,SCHYPG,'
      WRITE(*,*)' SDPOIS,SCPOIS,SQPOIS'
      WRITE(*,*)' and provides interactively numerical values'
      WRITE(*,*)' of statistical functions of a discrete variable'
      WRITE(*,*)' '
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - binomial distribution'
      WRITE(*,*)' 2 - hypergeometric distribution'
      WRITE(*,*)' 3 - Poisson distribution'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      IF(ISWIT.EQ.1) THEN
C binomial
        WRITE(*,*)' Choose'
        WRITE(*,*)' 1 - probability'
        WRITE(*,*)' 2 - distribution function (cumulative prob.)'
        WRITE(*,*)' >'
        READ(*,*)ISWIT1
        IF(ISWIT1.EQ.1) THEN
          WRITE(*,*)' Enter p (0.<p<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          WRITE(*,*)' Enter n (>=0)'
          WRITE(*,*)' >'
          READ(*,*)N
          WRITE(*,*)' Enter k (0<=k<=n)'
          WRITE(*,*)' >'
          READ(*,*)K
          F=SDBINM(K,N,P)
          WRITE(*,*)' W(n,k) = ',F
        ELSE IF(ISWIT1.EQ.2) THEN
          WRITE(*,*)' Enter p (0.<p<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          WRITE(*,*)' Enter n (>=0)'
          WRITE(*,*)' >'
          READ(*,*)N
          WRITE(*,*)' Enter K (0<=k<=n)'
          WRITE(*,*)' >'
          READ(*,*)K
          F=SCBINM(K,N,P)
          WRITE(*,*)' P(k<K) = ',F
        END IF
      ELSE IF(ISWIT.EQ.2) THEN
        WRITE(*,*)' Choose'
        WRITE(*,*)' 1 - probability'
        WRITE(*,*)' 2 - distribution function (cumulative prob.)'
        WRITE(*,*)' >'
        READ(*,*)ISWIT1
        IF(ISWIT1.EQ.1) THEN
          WRITE(*,*)' Enter K (>=0)'
          WRITE(*,*)' >'
          READ(*,*)KK
          WRITE(*,*)' Enter N (>=K)'
          WRITE(*,*)' >'
          READ(*,*)NN
          WRITE(*,*)' Enter k (0<=k<=K)'
          WRITE(*,*)' >'
          READ(*,*)K
          WRITE(*,*)' Enter n (k<=n<N)'
          WRITE(*,*)' >'
          READ(*,*)N
          F=SDHYPG(K,N,KK,NN)
          WRITE(*,*)' W(k) = ',F
        ELSE IF(ISWIT1.EQ.2) THEN
          WRITE(*,*)' Enter K (>=0)'
          WRITE(*,*)' >'
          READ(*,*)KK
          WRITE(*,*)' Enter N (>=K)'
          WRITE(*,*)' >'
          READ(*,*)NN
          WRITE(*,*)' Enter kprime (<=K)'
          WRITE(*,*)' >'
          READ(*,*)K
          WRITE(*,*)' Enter n (<=N)'
          WRITE(*,*)' >'
          READ(*,*)N
          F=SCHYPG(K,N,KK,NN)
          WRITE(*,*)' P(k<kprime) = ',F
        END IF
      ELSE IF(ISWIT.EQ.3) THEN
        WRITE(*,*)' Choose'
        WRITE(*,*)' 1 - probability'
        WRITE(*,*)' 2 - distribution function (cumulative prob.)'
        WRITE(*,*)' 3 - quantile'
        WRITE(*,*)' >'
        READ(*,*)ISWIT1
        IF(ISWIT1.EQ.1) THEN
          WRITE(*,*)' Enter k (>=0)'
          WRITE(*,*)' >'
          READ(*,*)K
          WRITE(*,*)' Enter lambda (>0.)'
          WRITE(*,*)' >'
          READ(*,*)ALAMBD
          F=SDPOIS(K,ALAMBD)
          WRITE(*,*)' f(k;lambda) = ',F
        ELSE IF(ISWIT1.EQ.2) THEN
          WRITE(*,*)' Enter K (>=0)'
          WRITE(*,*)' >'
          READ(*,*)KK
          WRITE(*,*)' Enter lambda (>0.)'
          WRITE(*,*)' >'
          READ(*,*)ALAMBD
          F=SCPOIS(KK,ALAMBD)
          WRITE(*,*)' P(k<K) = ',F
        ELSE IF(ISWIT1.EQ.3) THEN
          WRITE(*,*)' Enter K (>=0)'
          WRITE(*,*)' >'
          READ(*,*)KK
          WRITE(*,*)' Enter P (0.<P<1.)'
          WRITE(*,*)' >'
          READ(*,*)P
          F=SQPOIS(KK,P)
          WRITE(*,*)' lambda_P (K) = ',F
        END IF
      END IF
      END
