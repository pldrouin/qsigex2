      PROGRAM E1MTX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (I=1,J=2,K=3,FACT=0.5D0)
      DIMENSION A(J,K),B(J,K),C(K,J),R(J,K),S(J,J),T(K,K)
      DIMENSION U(K),V(K),W(J),X(K),Z(J)
      DATA A/1.D0,2.D0,2.D0,1.D0,3.D0,3.D0/
      DATA B/2.D0,1.D0,3.D0,5.D0,1.D0,4.D0/
      DATA C/1.D0,3.D0,2.D0,5.D0,4.D0,3.D0/
      DATA U/0.D0,3.D0,4.D0/
      DATA V/3.D0,1.D0,2.D0/
      DATA W/5.D0,2.D0/
C identify program to user
      WRITE(*,*)' Program E1MTX demonstrates use of'
      WRITE(*,*)' simple routines for matrix and vector algebra'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E1MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write initial matrices and vectors
      WRITE(*,*)' J = 2, K = 3, FACT = 0.5'
      WRITE(*,*)' A ='
      CALL MTXWRT(A,J,K)
      WRITE(*,*)' B ='
      CALL MTXWRT(B,J,K)
      WRITE(*,*)' C ='
      CALL MTXWRT(C,K,J)
      WRITE(*,*)' U ='
      CALL MTXWRT(U,1,K)
      WRITE(*,*)' V ='
      CALL MTXWRT(V,1,K)
      WRITE(*,*)' W ='
      CALL MTXWRT(W,1,J)
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXTRA
      CALL MTXTRA(A,R,J,K)
      WRITE(*,'(A)')' CALL MTXTRA(A,R,J,K) yields R ='
      CALL MTXWRT(R,J,K)
      WRITE(*,*)' '
C demonstrate MTXADD
      CALL MTXADD(A,B,R,J,K)
      WRITE(*,'(A)')' CALL MTXADD(A,B,R,J,K) yields R ='
      CALL MTXWRT(R,J,K)
      WRITE(*,*)' '
C demonstrate MTXSUB
      CALL MTXSUB(A,B,R,J,K)
      WRITE(*,'(A)')' CALL MTXSUB(A,B,R,J,K) yields R ='
      CALL MTXWRT(R,J,K)
      WRITE(*,*)' '
C demonstrate MTXMLT
      CALL MTXMLT(A,C,S,J,K,J)
      WRITE(*,'(A)')' CALL MTXMLT(A,C,S,J,K,J) yields S ='
      CALL MTXWRT(S,J,J)
      WRITE(*,*)' '
C demonstrate MTXMBT
      CALL MTXMBT(A,B,S,J,K,J)
      WRITE(*,'(A)')' CALL MTXMBT(A,B,S,J,K,J) yields S ='
      CALL MTXWRT(S,J,J)
      WRITE(*,*)' '
C demonstrate MTXMAT
      CALL MTXMAT(A,B,T,K,J,K)
      WRITE(*,'(A)')' CALL MTXMAT(A,B,T,K,J,K) yields T ='
      CALL MTXWRT(T,K,K)
      WRITE(*,*)' '
C demonstrate MTXUNT
      CALL MTXUNT(R,J)
      WRITE(*,'(A)')' CALL MTXUNT(R,J) yields R ='
      CALL MTXWRT(R,J,J)
      WRITE(*,*)' '
C demonstrate MTXZER
      CALL MTXZER(R,J,K)
      WRITE(*,'(A)')' CALL MTXZER(R,J,K) yields R ='
      CALL MTXWRT(R,J,K)
      WRITE(*,*)' '
C demonstrate MTXMSC
      CALL MTXMSC(A,R,FACT,J,K)
      WRITE(*,'(A)')' CALL MTXMSC(A,R,FACT,J,K) yields R ='
      CALL MTXWRT(R,J,K)
      WRITE(*,*)' '
C demonstrate MTXTRP
      CALL MTXTRP(A,R,J,K)
      WRITE(*,'(A)')' CALL MTXTRP(A,R,J,K) yields R ='
      CALL MTXWRT(R,K,J)
      WRITE(*,*)' '
C demonstrate MTXCPV
      CALL MTXCPV(W,Z,J)
      WRITE(*,'(A)')' CALL MTXCPV(W,Z,J) yields Z ='
      CALL MTXWRT(Z,1,J)
      WRITE(*,*)' '
C demonstrate MTXADV
      CALL MTXADV(U,V,X,K)
      WRITE(*,'(A)')' CALL MTXADV(U,V,X,K) yields X ='
      CALL MTXWRT(X,1,K)
      WRITE(*,*)' '
C demonstrate MTXSBV
      CALL MTXSBV(U,V,X,K)
      WRITE(*,'(A)')' CALL MTXSBV(U,V,X,K) yields X ='
      CALL MTXWRT(X,1,K)
      WRITE(*,*)' '
C demonstrate MTXDOT
      CALL MTXDOT(U,V,DOT,K)
      WRITE(*,'(A,G20.13)')
     +' CALL MTXDOT(U,V,DOT,K) yields DOT =',DOT
      WRITE(*,*)' '
C demonstrate MTXNRV
      CALL MTXNRV(U,SC,K)
      WRITE(*,'(A,G20.13)')' CALL MTXNRV(U,SC,K) yields SC =',SC
      WRITE(*,*)' '
C demonstrate MTXMSV
      CALL MTXMSV(U,X,FACT,K)
      WRITE(*,'(A)')' CALL MTXMSV(U,X,FACT,K) yields X ='
      CALL MTXWRT(X,1,K)
      WRITE(*,*)' '
C demonstrate MTXZRV
      CALL MTXZRV(X,K)
      WRITE(*,'(A)')' CALL MTXZRV(X,K) yields X ='
      CALL MTXWRT(X,1,K)
      WRITE(*,*)' '
      END
