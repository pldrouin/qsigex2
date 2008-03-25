      PROGRAM E5SM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)' Program E5SM demonstrates use of SMERQS'
      WRITE(*,*)' by computing errors of ratio'
      WRITE(*,*)' of signal events and reference events'
      WRITE(*,'(A,/)')'  in the presence of background'
C ask for input
      WRITE(*,*)' Enter number K of observed events (>0)'
      WRITE(*,*)' >'
      READ(*,*)K
      WRITE(*,*)
     +' Enter number ID of observed reference events (>0)'
      WRITE(*,*)' >'
      READ(*,*)ID
      WRITE(*,*)' Enter confidence limit CONFID (0.<CONFID<1.)'
      WRITE(*,*)' >'
      READ(*,*)CONFID
      WRITE(*,*)' Enter ratio RBACK (>=0.)'
      WRITE(*,*)' >'
      READ(*,*)RBACK
      CALL SMERQS(K,ID,CONFID,RBACK,RSMIN,RSPLS,RSUPR)
      WRITE(*,*)' RSMIN = ',RSMIN
      WRITE(*,*)' RSPLS = ',RSPLS
      WRITE(*,*)' RSUPR = ',RSUPR
      END
