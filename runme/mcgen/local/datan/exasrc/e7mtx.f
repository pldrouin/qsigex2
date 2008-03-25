      PROGRAM E7MTX
C demonstrates solution of matrix equation
C by singular value decomposition (SVD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (N=3,M=3,FRAC=0.D0)
      LOGICAL OK
      DIMENSION A(N,N),B(N,M),X(N,M),R(N)
      DATA A/1.D0,2.D0,1.D0,2.D0,1.D0,1.D0,3.D0,-2.D0,2.D0/
      DATA B/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/
C identify program to user
      WRITE(*,*)' Program E7MTX demonstrates use of'
      WRITE(*,*)' MTXSVD for singular value decomposition'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E7MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write input data
      WRITE(*,'(A,I2,A,I2)')' N = ',N,', M = ',M
C write initial matrices
      WRITE(*,*)' A ='
      CALL MTXWRT(A,N,N)
      WRITE(*,*)' B ='
      CALL MTXWRT(B,N,M)
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXSVD
      NN=N
      CALL MTXSVD(A,B,X,R,N,NN,M,FRAC,OK)
      WRITE(*,'(A)')' CALL MTXSVD(A,B,X,R,N,N,M,FRAC,OK) yields '
      WRITE(*,*)' OK =',OK
      WRITE(*,*)' X ='
      CALL MTXWRT(X,N,N)
      WRITE(*,*)' R ='
      CALL MTXWRT(R,1,N)
      END
