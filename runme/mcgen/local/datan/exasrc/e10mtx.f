      PROGRAM E10MTX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (N=3,L=1,M=3,FRAC=0.D0)
      LOGICAL OK
      DIMENSION A(M,N),B(M),D(L),E(L,N),X(N),A2(M,N-L)
      DATA A/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/
      DATA B/89.D0,31.D0,62.D0/
      DATA D/180.D0/
      DATA E/1.D0,1.D0,1.D0/
C identify program to user
      WRITE(*,*)' Program E10MTX demonstrates use of MTXLSC'
      WRITE(*,*)' to solve a least squares problem'
      WRITE(*,*)' with linear constraints'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E10MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write input data
      WRITE(*,'(A,I2,A,I2)')' N = ',N
C write initial matrices
      WRITE(*,*)' A ='
      CALL MTXWRT(A,M,N)
      WRITE(*,*)' B ='
      CALL MTXWRT(B,M,1)
      WRITE(*,*)' D ='
      CALL MTXWRT(D,1,L)
      WRITE(*,*)' E ='
      CALL MTXWRT(E,L,N)
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXLSC
      CALL MTXLSC(A,B,E,D,X,R,A2,M,N,L,FRAC,OK)
      WRITE(*,'(A)')
     +' CALL MTXLSC(A,B,E,D,X,R,A2,M,N,L,FRAC,OK)   yields '
      WRITE(*,*)' OK =',OK
      WRITE(*,*)' X ='
      CALL MTXWRT(X,1,N)
      WRITE(*,'(A,F15.10)')' R =',R
      END
