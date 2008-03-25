      PROGRAM E2MTX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (I=1,J=2,K=3,FACT=0.5D0)
      DIMENSION A(J,K),D(J,J),R(J,K),S(J,J)
      DIMENSION U(K),W(J),X(K),Z(J)
      DIMENSION LIST(K)
      DATA A/1.D0,2.D0,2.D0,1.D0,3.D0,3.D0/
      DATA D/0.D0,1.D0,2.D0,3.D0/
      DATA U/0.D0,3.D0,4.D0/
      DATA W/5.D0,2.D0/
      DATA LIST/1,0,1/
C identify program to user
      WRITE(*,*)' Program E2MTX demonstrates use of routines'
      WRITE(*,*)' for manipulation of submatrices and subvectors'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E2MTX'
      WRITE(*,'(A)')' ------------------------------------------'
C write initial matrices and vectors
      WRITE(*,*)' J = 2, K = 3, LIST = (1,0,1), FACT = 0.5'
      WRITE(*,*)' A ='
      CALL MTXWRT(A,J,K)
      WRITE(*,*)' D ='
      CALL MTXWRT(D,J,J)
      WRITE(*,*)' U ='
      CALL MTXWRT(U,1,K)
      WRITE(*,*)' W ='
      CALL MTXWRT(W,1,J)
      WRITE(*,'(A)')' ------------------------------------------'
      WRITE(*,*)' '
C demonstrate MTXGSM
      CALL MTXGSM(A,S,J,K,J,J,1,2)
      WRITE(*,'(A)')' CALL MTXGSM(A,S,J,K,J,J,1,2) yields S ='
      CALL MTXWRT(S,J,J)
      WRITE(*,*)' '
C demonstrate MTXPSM
C (first save and later restore original matrix A)
      CALL MTXTRA(A,R,J,K)
      CALL MTXPSM(A,D,J,K,J,J,1,1)
      WRITE(*,'(A)')' CALL MTXPSM(A,D,J,K,J,J,1,1) yields A ='
      CALL MTXWRT(A,J,K)
      WRITE(*,*)' '
      CALL MTXTRA(R,A,J,K)
C demonstrate MTXGCL
      CALL MTXGCL(A,X,J,K,2)
      WRITE(*,'(A)')' CALL MTXGCL(A,X,J,K,2) yields X ='
      CALL MTXWRT(X,J,1)
      WRITE(*,*)' '
C demonstrate MTXPCL
C (first save and later restore original matrix A)
      CALL MTXTRA(A,R,J,K)
      CALL MTXPCL(A,W,J,K,1)
      WRITE(*,'(A)')' CALL MTXPCL(A,W,J,K,1) yields A ='
      CALL MTXWRT(A,J,K)
      WRITE(*,*)' '
      CALL MTXTRA(R,A,J,K)
C demonstrate MTXGRW
      CALL MTXGRW(A,X,J,K,2)
      WRITE(*,'(A)')' CALL MTXGRW(A,X,J,K,2) yields X ='
      CALL MTXWRT(X,1,K)
      WRITE(*,*)' '
C demonstrate MTXPRW
C (first save and later restore original matrix A)
      CALL MTXTRA(A,R,J,K)
      CALL MTXPRW(A,U,J,K,1)
      WRITE(*,'(A)')' CALL MTXPRW(A,U,J,K,1) yields A ='
      CALL MTXWRT(A,J,K)
      WRITE(*,*)' '
C demonstrate MTXGSV
      CALL MTXGSV(U,Z,K,J,LIST)
      WRITE(*,'(A)')' CALL MTXGSV(U,Z,K,J,LIST) yields Z ='
      CALL MTXWRT(Z,1,J)
      WRITE(*,*)' '
C demonstrate MTXPSV
      CALL MTXPSV(U,W,K,J,LIST)
      WRITE(*,'(A)')' CALL MTXPSV(U,W,K,J,LIST) yields U ='
      CALL MTXWRT(U,1,K)
      WRITE(*,*)' '
      END
