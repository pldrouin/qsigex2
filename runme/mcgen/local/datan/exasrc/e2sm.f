      PROGRAM E2SM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION HIST(1000),SAMPLE(1)
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program E1SM demonstrates the use of'
      WRITE(*,*)' subroutines SMHSIN, SMHSFL, SMHSGR'
      WRITE(*,*)' on a sample taken from the '
      WRITE(*,*)' standardized normal distribution'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number N of elements in sample (>0)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter lower edge X0 of histogram'
      WRITE(*,*)' >'
      READ(*,*)X0
      WRITE(*,*)' Enter bin width DELX of histogram'
      WRITE(*,*)' >'
      READ(*,*)DELX
      WRITE(*,*)' Enter number NX of histogram bins (<1001)'
      WRITE(*,*)' >'
      READ(*,*)NX
C define texts
      TX='x'
      LTX=1
      TY='N(x)'
      LTY=4
      CAPT='N = ******'
      LCAPT=10
      WRITE(CAPT(5:10),'(I6)')N
C initialize histogram
      CALL SMHSIN(HIST,X0,DELX,NX)
C fill histogram
      DO 10 I=1,N
        CALL RNSTNR(SAMPLE,1)
        CALL SMHSFL(HIST,X0,DELX,NX,SAMPLE(1),DBLE(1.))
10    CONTINUE
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL SMHSGR(HIST,X0,DELX,NX,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
