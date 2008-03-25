      PROGRAM E6MTX
C demonstrates use of Cholesky routines
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION B(3,3),C(3,2),A(3,3),U(3,3),R(3,3),S(3,3)
      DATA B/1.D0,3.D0,5.D0,7.D0,11.D0,13.D0,17.D0,19.D0,23.D0/
      DATA C /1.D0,1.D0,1.D0,1.D0,1.D0,1.D0/
C identify program to user
      WRITE(*,*)' Program E6MTX demonstrates use of'
      WRITE(*,*)' MTXCHL,MTXCHI and MTXCHM'
      WRITE(*,*)' for Cholesky decomposition and inversion'
      WRITE(*,*)' '
C construct symmetric, positive definite matrix X
      N=3
      M=2
      CALL MTXMAT(B,B,A,N,N,N)
      WRITE(*,*)' Output produced by program E6MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write input data
      WRITE(*,*)' A ='
      CALL MTXWRT(A,N,N)
      WRITE(*,*)' '
      WRITE(*,*)' C ='
      CALL MTXWRT(C,N,M)
      WRITE(*,*)' '
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXCHL
      CALL MTXCHL(A,U,N)
      WRITE(*,'(A)')' CALL MTXCHL(A,U,N) yields U = '
      CALL MTXWRT(U,N,N)
      WRITE(*,*)' '
C perform check by multiplying U with its transposed
      CALL MTXMAT(U,U,R,N,N,N)
      WRITE(*,*)' check of decomposition: R = U^T U ='
      CALL MTXWRT(R,N,N)
      WRITE(*,*)' '
C demonstrate MTXCHM
      CALL MTXCHM(U,C,R,N,M)
      WRITE(*,'(A)')' CALL MTXCHM(U,C,R,N,M)  yields  R ='
      CALL MTXWRT(R,N,M)
      WRITE(*,*)' '
C demonstrate MTXCHI
C (first save original matrix A)
      CALL MTXTRA(A,S,N,N)
      CALL MTXCHI(A,U,N)
      WRITE(*,'(A)')' CALL MTXCHI(A,U,N) yields  A ='
      CALL MTXWRT(A,N,N)
      WRITE(*,*)' '
C perform check by multiplying original matrix A with inverse
      CALL MTXMLT(S,A,R,N,N,N)
      WRITE(*,*)' check of inversion: R = A**(-1) A ='
      CALL MTXWRT(R,N,N)
      WRITE(*,*)' '
      END
