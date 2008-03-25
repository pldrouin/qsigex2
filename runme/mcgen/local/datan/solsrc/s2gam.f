      PROGRAM S2GAM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION XPL(1000),YPL(1000)
      CHARACTER*75 TX,TY,CAPT
      PARAMETER(NPL=1000)
C identify program to user
      WRITE(*,*)' Program S2GAM provides interactively graphs'
      WRITE(*,*)' of the beta function the incomplete'
      WRITE(*,*)' gamma function and incomplete beta function'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - beta function'
      WRITE(*,*)' 2 - incomplete gamma function'
      WRITE(*,*)' 3 - incomplete beta function'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      IF(ISWIT.EQ.1) THEN
C beta
        WRITE(*,*)' Enter z (> 0.)'
        WRITE(*,*)' >'
        READ(*,*)Z
        XMIN=0.D0
          XMAX=2.D0
        YMIN=0.D0
          YMAX=5.D0
        TX='w'
        LTX=1
        TY='B(z,w)'
        LTY=6
        CAPT=' z = ******'
        WRITE(CAPT(6:11),'(F6.3)')Z
        LCA=11
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 10 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          YPL(I)=GBETAF(Z,XPL(I))
10      CONTINUE
      ELSE IF(ISWIT.EQ.2) THEN
C incomplete gamma
        WRITE(*,*)' Enter a (> 0.)'
        WRITE(*,*)' >'
        READ(*,*)A
        XMIN=0.D0
        XMAX=10.D0
        YMIN=0.D0
        YMAX=1.D0
        TX='x'
        LTX=1
        TY='P(a,x)'
        LTY=6
        CAPT=' a = ******'
        WRITE(CAPT(6:11),'(F6.3)')A
        LCA=11
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 20 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          YPL(I)=GINCGM(A,XPL(I))
20      CONTINUE
      ELSE IF(ISWIT.EQ.3) THEN
C incomplete beta
        WRITE(*,*)' Enter a (> 0.)'
        WRITE(*,*)' >'
        READ(*,*)A
        WRITE(*,*)' Enter b (> 0.)'
        WRITE(*,*)' >'
        READ(*,*)B
        XMIN=0.D0
        XMAX=1.D0
        YMIN=0.D0
        YMAX=1.D0
        TX='w'
        LTX=1
        TY='I_x#(a,b)'
        LTY=9
        CAPT=' a = ******, b = ******'
        WRITE(CAPT(6:11),'(F6.3)')A
        WRITE(CAPT(18:23),'(F6.3)')B
        LCA=23
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 30 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          YPL(I)=GINCBT(A,B,XPL(I))
30      CONTINUE
      END IF
C graphics
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
      CALL GRVWWC(-0.15D0,0.9D0,0.15D0,0.85D0)
      CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
      CALL GRFRAM
      CALL GRBOUN
      CALL GRSCLX(TX,LTX)
      CALL GRSCLY(TY,LTY)
      CALL GRTXTC(1.D0,CAPT,LCA)
      CALL GRSTCL(2)
      CALL GRPLIN(NPL,XPL,YPL)
      CALL GRCLSE
      END
