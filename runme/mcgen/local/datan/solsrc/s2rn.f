      PROGRAM S2RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(1000),HIST(50)
      CHARACTER*5 TX,TY,CAPT
      DATA TX/'x'/,TY/'N(x)'/,CAPT/' '/
C identify program to user
      WRITE(*,*)' Program S2RN demonstrates use of RNTRIT and RNTRIR'
      WRITE(*,*)' which are subroutines generating random numbers'
      WRITE(*,*)' following a triangular distribution'
      WRITE(*,*)' '
C let user choose
      WRITE(*,*)' Enter number N (1<= N <= 1000)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter A'
      WRITE(*,*)' >'
      READ(*,*)A
      WRITE(*,*)' Enter B'
      WRITE(*,*)' >'
      READ(*,*)B
      WRITE(*,*)' Enter C (A<C<B)'
      WRITE(*,*)' >'
      READ(*,*)C
      WRITE(*,*)' Do you want'
      WRITE(*,*)' 1 - numerical output'
      WRITE(*,*)' 2 - graphical output'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      WRITE(*,*)' Do you want'
      WRITE(*,*)' 1 - generation by transformation method'
      WRITE(*,*)' 2 - generation by rejection method'
      WRITE(*,*)' >'
      READ(*,*)ITYPE
      IF (ITYPE.EQ.1) CALL RNTRIT(A,B,C,R,N)
      IF (ITYPE.EQ.2) CALL RNTRIR(A,B,C,R,N)
      IF(ISWIT.EQ.1) THEN
C numerical output
        WRITE(*,*)' Random numbers are'
        CALL MTXWRT(R,1,N)
      ELSE IF(ISWIT.EQ.2) THEN
C graphical output
C initialize histogram
        X0=A
        DELX=.02D0*(B-A)
        NX=50
        CALL SMHSIN(HIST,X0,DELX,NX)
C fill histogram
        DO 10 I=1,N
          CALL SMHSFL(HIST,X0,DELX,NX,R(I),1.D0)
10      CONTINUE
C ask for number of workstation
        CALL GRNBWS()
        WRITE(*,*)' Please, enter number of workstation:'
        WRITE(*,*)' >'
        READ(*,*)NWS
C display of histogram
        CALL SMHSGR(HIST,X0,DELX,NX,TX,5,TY,5,CAPT,5,NWS)
      END IF
      END
C ------------------------------------------------------------------
      SUBROUTINE RNTRIT(A,B,C,R,N)
C generation of triangularly distributed random numbers
C by transformation method
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(N)
      PARAMETER(ONE=1.D0)
        BA=B-A
        CA=C-A
      XLIM=CA/BA
      CALL RNECUY(R,N)
      DO 10 I=1,N
        IF(R(I).LE.XLIM) THEN
          R(I)=A+SQRT(BA*CA*R(I))
        ELSE
          R(I)=B-SQRT(BA*(B-C)*(ONE-R(I)))
        END IF
10    CONTINUE
      END
C ------------------------------------------------------------------
      SUBROUTINE RNTRIR(A,B,C,R,N)
C generation of triangularly distributed random numbers
C by rejection method
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(N),RR(2)
      DO 20 I=1,N
10      CALL RNECUY(RR,2)
        R1=A+RR(1)*(B-A)
        IF(R1.LE.C) THEN
          H=(R1-A)/(C-A)
        ELSE
          H=(B-R1)/(B-C)
        END IF
        IF(RR(2).GE.H) GO TO 10
        R(I)=R1
20    CONTINUE
      END
