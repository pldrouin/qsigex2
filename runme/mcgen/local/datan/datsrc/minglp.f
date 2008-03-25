      DOUBLE PRECISION FUNCTION MINGLP(X,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /CMINGH/ T(100),HIST(100),DELTAT,NT,NEVENT
      DIMENSION X(N)
      PARAMETER(ZERO=0.D0)
      FNORM=DBLE(NEVENT)*DELTAT
      MINGLP=ZERO
      DO 10 I=1,NT
C GI is the value of the probability density of the population
C at T(I) (by replacing the RHS of the following statement it can
C be changed from normal to any desired distribution)
        GI=SDNORM(T(I),X(1),X(2))
C normalize to number of events in sample
        GI=FNORM*GI
        ALAM=GI
        IF(ALAM.GT.ZERO) THEN
          NI=NINT(HIST(I))
          ALNLAM=LOG(ALAM)
          ALNNI=GLNGAM(DBLE(NI+1))
          MINGLP=MINGLP+ALNNI-HIST(I)*ALNLAM+ALAM
        END IF
   10 CONTINUE
      END
