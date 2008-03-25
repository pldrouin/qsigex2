      PROGRAM E2GAM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*10 FORM
      PARAMETER(NMAX=11)
      DIMENSION IRES(NMAX+1)
      DATA FORM /'(**X,15I6)'/
C identify program to user
      WRITE(*,*)' Program E2GAM demonstrates use of GBINCO'
      WRITE(*,*)' and computes and prints '
      WRITE(*,*)' top of Pascal''s triangle'
      WRITE(*,*)' '
C loop over N
      DO 20 N=0,NMAX
C loop over K
        DO 10 K=0,N
C compute binomial coefficient
          IRES(K+1)=INT(GBINCO(N,K))
10      CONTINUE
C compute format
        NX=(NMAX-N)*3+1
        WRITE(FORM(2:3),'(I2)')NX
C write one line of triangle
        WRITE(*,FORM)(IRES(J),J=1,N+1)
20    CONTINUE
      END
