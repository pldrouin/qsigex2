      PROGRAM E9MTX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (N=3,FRAC=0.D0,ALAM=1.D-3)
      LOGICAL OK
      DIMENSION A(N,N),B(N,1),X1(N,1),X2(N,1)
      DATA A/1.D0,2.D0,1.D0,2.D0,1.D0,1.D0,3.D0,-2.D0,2.D0/
      DATA B/1.D0,0.D0,0.D0/
C identify program to user
      WRITE(*,*)' Program E9MTX demonstrates use of MTXMAR'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E9MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write input data
      WRITE(*,'(A,I2,A,I2)')' N = ',N
C write initial matrices
      WRITE(*,*)' A ='
      CALL MTXWRT(A,N,N)
      WRITE(*,*)' B ='
      CALL MTXWRT(B,N,1)
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXMAR
      NN=N
      CALL MTXMAR(A,B,ALAM,X1,X2,N,NN,FRAC,OK)
      WRITE(*,'(A)')
     +' CALL MTXMAR(A,B,ALAM,X1,X2,N,N,FRAC,OK)  yields '
      WRITE(*,*)' OK =',OK
      WRITE(*,*)' X1 ='
      CALL MTXWRT(X1,N,1)
      WRITE(*,*)' X2 ='
      CALL MTXWRT(X2,N,1)
      END
