      PROGRAM E1TIM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(36),ETA(50),CONETA(50),A(21,6)
      DIMENSION ATA1(6,6),ATA1AT(6,21),SCRAT(6,6)
      DATA Y /38.7D0,50.3D0,45.6D0,46.4D0,43.7D0,42.0D0,
     +21.8D0,21.8D0,51.3D0,39.5D0,26.9D0,23.2D0,
     +19.8D0,24.4D0,17.1D0,29.3D0,43.0D0,35.9D0,
     +19.6D0,33.2D0,38.8D0,35.3D0,23.4D0,14.9D0,
     +15.3D0,17.7D0,16.5D0, 6.6D0, 9.5D0, 9.1D0,
     + 3.1D0, 9.3D0, 4.7D0, 6.1D0, 7.4D0,15.1D0/
C identify program to user
      WRITE(*,*)' Program E1TIM demonstrates use of TIMSER'
      WRITE(*,*)' '
C initialize
      N=36
      K=2
      L=2
      P=0.9
C perform time series analysis
      CALL TIMSER(Y,N,K,L,P,ETA,CONETA,A,ATA1,ATA1AT,SCRAT)
C output results
      WRITE(*,'(3(A,I3),A,F10.5)')' N = ',N,', K = ',K,', L = ',L,
     +', P = ',P
      WRITE(*,*)'  I   Y         ETA       CONETA'
      DO 10 I=-K+1,N+K,1
        IF(I.LT. 1.OR. I.GT.N) THEN
          WRITE(*,'(I4,10X,2F10.5)')I,ETA(I+K),CONETA(I+K)
        ELSE
          WRITE(*,'(I4,3F10.5)')I,Y(I),ETA(I+K),CONETA(I+K)
        END IF
10    CONTINUE
      END
