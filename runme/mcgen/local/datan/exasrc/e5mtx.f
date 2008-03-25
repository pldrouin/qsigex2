      PROGRAM E5MTX
C demonstrates solution of matrix equation by Gauss algorithm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (N=3,M=3)
      DIMENSION A(N,N),B(N,M)
      DATA A/1.D0,2.D0,1.D0,2.D0,1.D0,1.D0,3.D0,-2.D0,2.D0/
      DATA B/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/
C identify program to user
      WRITE(*,*)' Program E5MTX demonstrates use of MTXEQU'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E5MTX'
      WRITE(*,'(A)')' -----------------------------------------'
C write input data
      WRITE(*,'(A,I2,A,I2)')' N = ',N,', M = ',M
C write initial matrices
      WRITE(*,*)' A ='
      CALL MTXWRT(A,N,N)
      WRITE(*,*)' B ='
      CALL MTXWRT(B,N,M)
      WRITE(*,'(A)')' -----------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXEQU
      CALL MTXEQU(A,B,N,M)
      WRITE(*,'(A)')' CALL MTXEQU(A,B,N,M) yields '
      WRITE(*,*)' B ='
      CALL MTXWRT(B,N,N)
      END
