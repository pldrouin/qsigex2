      PROGRAM E3MTX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (K=2)
      DIMENSION U(K),W(K)
      DATA U/3.D0,4.D0/
      DATA W/1.D0,1.D0/
C identify program to user
      WRITE(*,*)' Program E3MTX demonstrates use of'
      WRITE(*,*)' MTXGVD,MTXGVT and MTXGVA'
      WRITE(*,*)' for Givens transformations'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E3MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write initial vectors
      WRITE(*,*)' U ='
      CALL MTXWRT(U,1,K)
      WRITE(*,*)' W ='
      CALL MTXWRT(W,1,K)
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXGVD
      CALL MTXGVD(U(1),U(2),C,S)
      WRITE(*,'(A)')' CALL MTXGVD(U(1),U(2),C,S) yields '
      WRITE(*,'(A,F10.5,A,F10.5)')' C = ',C,', S = ',S
      WRITE(*,*)' '
C demonstrate MTXGVT
      CALL MTXGVT(U(1),U(2),C,S)
      WRITE(*,*)' With the above values of C and S'
      WRITE(*,'(A)')' CALL MTXGVT(U(1),U(2),C,S) yields '
      WRITE(*,*)' U ='
      CALL MTXWRT(U,1,K)
      WRITE(*,*)' '
C demonstrate MTXGVA
      CALL MTXGVA(W(1),W(2),C,S)
      WRITE(*,'(A)')' CALL MTXGVA(W(1),W(2),C,S) yields '
      WRITE(*,'(A,F10.5,A,F10.5)')' C = ',C,', S = ',S
      WRITE(*,*)' and W ='
      CALL MTXWRT(W,1,K)
      END
