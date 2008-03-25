      PROGRAM E4SM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)' Program E4SM demonstrates use of SMERSS'
      WRITE(*,*)' by computing error of Poisson parameter'
      WRITE(*,'(A,/)')
     +'  derived from a small sample with background'
C ask for input
      WRITE(*,*)' Enter number K of observed events (>=0)'
      WRITE(*,*)' >'
      READ(*,*)K
      WRITE(*,*)' Enter confidence limit CONFID (0.<CONFID<1.)'
      WRITE(*,*)' >'
      READ(*,*)CONFID
      WRITE(*,*)
     +' Enter Poisson Parameter ALBACK of background (>=0.)'
      WRITE(*,*)' >'
      READ(*,*)ALBACK
      CALL SMERSS(K,CONFID,ALBACK,ALSMIN,ALSPLS,ALSUPR)
      WRITE(*,*)' ALSMIN = ',ALSMIN
      WRITE(*,*)' ALSPLS = ',ALSPLS
      WRITE(*,*)' ALSUPR = ',ALSUPR
      END
