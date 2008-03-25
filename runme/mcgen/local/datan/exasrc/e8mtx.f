      PROGRAM E8MTX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MMAX=3,NMAX=3)
      PARAMETER (EPSILN=1.D-12, FRAC=1.D-10)
      LOGICAL OK
      DIMENSION A(MMAX,NMAX),B(MMAX,MMAX),X(NMAX,MMAX)
      DIMENSION R(MMAX),D(NMAX),U(MMAX,MMAX),
     +V(NMAX,NMAX),S(MMAX,MMAX)
      DIMENSION C1(MMAX),C2(MMAX),C3(MMAX),C(NMAX)
      DATA C1 /1.D0,2.D0,1.D0/
      DATA C2 /2.D0,1.D0,1.D0/
      DATA C3 /3.D0,-2.D0,2.D0/
C identify program to user
      WRITE(*,*)' Program E8MTX demonstrates use of'
      WRITE(*,*)' MTXDEC solving matrix equations in 9 different'
      WRITE(*,*)' cases using singular value decomposition'
      WRITE(*,*)' '
      WRITE(*,*)' Output produced by program E8MTX'
      DO 30 ISWIT=1,3
        DO 20 ISWIT1=1,3
          WRITE(*,'(A)')' ----------------------------------------'
          IF(ISWIT.EQ.1) THEN
            WRITE(*,*)' N=M (CASE 1)'
          ELSE IF(ISWIT.EQ.2) THEN
            WRITE(*,*)' N<M (CASE 2)'
          ELSE IF(ISWIT.EQ.3) THEN
            WRITE(*,*)' N>M (CASE 3)'
          END IF
          IF(ISWIT1.EQ.1) THEN
            WRITE(*,*)'  A has full rank (CASE a)'
          ELSE IF(ISWIT1.EQ.2) THEN
            WRITE(*,*)'  A has smaller than full rank (CASE b)'
          ELSE IF(ISWIT1.EQ.3) THEN
            WRITE(*,*)
     +      '  A has smaller than full pseudorank (CASE c)'
          END IF
C prepare input
          IF(ISWIT.EQ.1) THEN
C case 1
            M=3
            N=3
            CALL MTXPCL(A,C1,M,N,1)
            CALL MTXPCL(A,C2,M,N,2)
            CALL MTXPCL(A,C3,M,N,3)
            IF(ISWIT1.EQ.2) THEN
              CALL MTXPCL(A,C2,M,N,3)
            END IF
            IF(ISWIT1.EQ.3) THEN
              CALL MTXCPV(C2,C,M)
              C(3)=C(3)+EPSILN
              CALL MTXPCL(A,C,M,N,3)
            END IF
          ELSE IF(ISWIT.EQ.2) THEN
C case 2
            M=3
            N=2
            CALL MTXPCL(A,C1,M,N,1)
            CALL MTXPCL(A,C2,M,N,2)
            IF(ISWIT1.EQ.2) THEN
              CALL MTXPCL(A,C1,M,N,2)
            END IF
            IF(ISWIT1.EQ.3) THEN
              CALL MTXCPV(C1,C,M)
              C(3)=C(3)+EPSILN
              CALL MTXPCL(A,C,M,N,2)
            END IF
          ELSE IF(ISWIT.EQ.3) THEN
C case 3
C (the case (M=2, N=3)is treated by constructing adding a line of
C  zeroes to the original 2x3 matrix A and tresting it as a
C  3x3 matrix)
            M=3
            N=3
            CALL MTXZER(A,M,N)
            CALL MTXPRW(A,C1,M,N,1)
            CALL MTXPRW(A,C2,M,N,2)
            IF(ISWIT1.EQ.2) THEN
              CALL MTXPRW(A,C1,M,N,2)
            ELSE IF(ISWIT1.EQ.3) THEN
              CALL MTXCPV(C1,C,M)
              C(3)=C(3)+EPSILN
              CALL MTXPRW(A,C,M,N,2)
            END IF
          END IF
C write initial matrix and its dimensions
          WRITE(*,'(A,G8.1,A,G8.1)')
     +    '  EPSILN = ',EPSILN,', FRAC = ',FRAC
          WRITE(*,'(A,I2,A,I2)')'  M = ',M,', N = ',N
          WRITE(*,*)' A ='
          CALL MTXWRT(A,M,N)
          WRITE(*,'(A)')
     +    ' ------------------------------------------'
          WRITE(*,*)' '
C demonstrate MTXDEC
          NORG=N
          CALL MTXDEC(A,B,X,R,M,N,FRAC,OK,D,U,V)
          WRITE(*,'(A)')
     +    ' CALL MTXDEC(A,B,X,R,M,N,FRAC,OK,D,U,V) yields'
          NSING=N
          WRITE(*,*)' number of non zero singular values is ',NSING
          N=NORG
          WRITE(*,*)' OK =',OK
          WRITE(*,*)' X ='
          CALL MTXWRT(X,N,M)
          WRITE(*,*)' R ='
          CALL MTXWRT(R,1,M)
          WRITE(*,*)' D ='
          CALL MTXWRT(D,1,N)
C perform singular value analysis (SVA)
          IF(NSING.LT.NORG) THEN
            DO 5 I=NSING+1,NORG
              D(I)=0.D0
5           CONTINUE
            WRITE(*,*)' after singular value analysis (SVA): D ='
            CALL MTXWRT(D,1,N)
          END IF
          WRITE(*,*)' U ='
          CALL MTXWRT(U,M,M)
          WRITE(*,*)' V ='
          CALL MTXWRT(V,N,N)
C perform check by forming matrix product U*D*V^T
          CALL MTXZER(S,M,M)
          DO 10 I=1,N
            S(I,I)=D(I)
10        CONTINUE
          CALL MTXMLT(U,S,B,M,M,N)
          CALL MTXMBT(B,V,U,M,N,N)
          WRITE(*,*)' check of SVD: U*D*V^T ='
          CALL MTXWRT(U,M,N)
20      CONTINUE
30    CONTINUE
      END
