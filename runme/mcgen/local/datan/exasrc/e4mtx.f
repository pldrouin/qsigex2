      PROGRAM E4MTX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (N=6,L=5,LP=3)
      DIMENSION V(N),C(N)
      DATA V/1.D0,2.D0,0.D0,4.D0,3.D0,4.D0/
      DATA C/1.D0,2.D0,0.D0,4.D0,3.D0,4.D0/
C identify program to user
      WRITE(*,*)' Program E4MTX demonstrates use of'
      WRITE(*,*)' MTXHSD and MTXHST'
      WRITE(*,*)' for Householder transformations'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E4MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write input data
      WRITE(*,'(A,I2,A,I2,A,I2)')' N = ',N,', LP = ',LP,', L = ',L
C write initial vector
      WRITE(*,*)' V ='
      CALL MTXWRT(V,1,N)
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXHSD
      CALL MTXHSD(V,UP,B,N,LP,L)
      WRITE(*,'(A)')' CALL MTXHSD(U,UP,B,N,LP,L) yields '
      WRITE(*,'(A,F10.5,A,F10.5)')' UP = ',UP,', B = ',B
      WRITE(*,*)' '
C demonstrate MTXHST
      CALL MTXHST(V,UP,B,C,N,LP,L)
      WRITE(*,*)' With the above values of V, UP and B'
      WRITE(*,'(A)')' CALL MTXHST(V,UP,B,C,N,LP,L) yields '
      WRITE(*,*)' C ='
      CALL MTXWRT(C,1,N)
      END
