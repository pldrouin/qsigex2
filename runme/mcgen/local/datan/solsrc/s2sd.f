      PROGRAM S2SD
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION XPL(1000),YPL(1000)
      CHARACTER*75 TX,TY,CAPT
      PARAMETER(NPL=1000)
C identify program to user
      WRITE(*,*)' Program S2D provides interactively graphs of'
      WRITE(*,*)' statistical functions of a continuous variable'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - standardized normal distribution'
      WRITE(*,*)' 2 - normal distribution'
      WRITE(*,*)' 3 - chi-squared distribution'
      WRITE(*,*)' 4 - F distribution'
      WRITE(*,*)' 5 - t distribution'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - probability density'
      WRITE(*,*)' 2 - distribution function'
      WRITE(*,*)' >'
      READ(*,*)ISWIT1
      IF(ISWIT.EQ.1) THEN
C standardized normal
        XMIN=-3.D0
        XMAX=3.D0
        YMIN=0.D0
        IF(ISWIT1.EQ.1) THEN
          YMAX=0.5D0
        ELSE
          YMAX=1.D0
        END IF
        TX='x'
        LTX=1
        IF(ISWIT1.EQ.1) THEN
          TY='&f@(x)'
        ELSE IF(ISWIT1.EQ.2) THEN
          TY='&F@(x)'
        END IF
        LTY=6
        CAPT='standardized normal distribution'
        LCA=32
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 10 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          IF(ISWIT1.EQ.1) THEN
            YPL(I)=SDSTNR(XPL(I))
          ELSE IF(ISWIT1.EQ.2) THEN
            YPL(I)=SCSTNR(XPL(I))
          END IF
10      CONTINUE
      ELSE IF(ISWIT.EQ.2) THEN
C normal
        WRITE(*,*)' Enter x0'
        WRITE(*,*)' >'
        READ(*,*)X0
        WRITE(*,*)' Enter sigma (> 0.)'
        WRITE(*,*)' >'
        READ(*,*)SIGMA
        XMIN=-3.D0*SIGMA+X0
        XMAX=3.D0*SIGMA+X0
        YMIN=0.D0
        IF(ISWIT1.EQ.1) THEN
          YMAX=1.2D0*SDNORM(X0,X0,SIGMA)
        ELSE
          YMAX=1.D0
        END IF
        TX='x'
        LTX=1
        IF(ISWIT1.EQ.1) THEN
          TY='f(x)'
        ELSE IF(ISWIT1.EQ.2) THEN
          TY='F(x)'
        END IF
        LTY=4
        CAPT='normal distribution with x_0# = ******, &s@ = ******'
        WRITE(CAPT(33:38),'(F6.2)')X0
        WRITE(CAPT(47:52),'(F6.2)')SIGMA
        LCA=52
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 20 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          IF(ISWIT1.EQ.1) THEN
            YPL(I)=SDNORM(XPL(I),X0,SIGMA)
          ELSE IF(ISWIT1.EQ.2) THEN
            YPL(I)=SCNORM(XPL(I),X0,SIGMA)
          END IF
20      CONTINUE
      ELSE IF(ISWIT.EQ.3) THEN
C chi-squared
        WRITE(*,*)' Enter n (>= 1)'
        WRITE(*,*)' >'
        READ(*,*)N
        XMIN=0.D0
        XMAX=SQCHI2(.999D0,N)
        IF(N.EQ.1) XMIN=.00001D0
        YMIN=0.D0
        IF(ISWIT1.EQ.1) THEN
          IF(N.EQ.1) YMAX=2.D0
          IF(N.EQ.2) YMAX=.5D0
          IF(N.GT.2) YMAX=1.2D0*SDCHI2(DBLE(N-1),N)
        ELSE IF(ISWIT1.EQ.2) THEN
          YMAX=1.D0
        END IF
        TX='&c^2'
        LTX=4
        IF(ISWIT1.EQ.1) THEN
          TY='f(&c@^2#)'
        ELSE IF(ISWIT1.EQ.2) THEN
          TY='F(&c@^2#)'
        END IF
        LTY=9
        CAPT='&c@^2# distribution with n = ***'
        WRITE(CAPT(30:32),'(I3)')N
        LCA=32
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 30 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          IF(ISWIT1.EQ.1) THEN
            YPL(I)=SDCHI2(XPL(I),N)
          ELSE IF(ISWIT1.EQ.2) THEN
            YPL(I)=SCCHI2(XPL(I),N)
          END IF
30      CONTINUE
      ELSE IF(ISWIT.EQ.4) THEN
C F distribution
        WRITE(*,*)' Enter f1 (>= 1)'
        WRITE(*,*)' >'
        READ(*,*)NF1
        WRITE(*,*)' Enter f2 (>= 1)'
        WRITE(*,*)' >'
        READ(*,*)NF2
        XMIN=-0.D0
        XMAX=3.D0
        YMIN=0.D0
        YMAX=1.D0
        TX='F'
        LTX=1
        IF(ISWIT1.EQ.1) THEN
          TY='f(F)'
        ELSE IF(ISWIT1.EQ.2) THEN
          TY='F(F)'
        END IF
        LTY=4
        CAPT='F distribution with f_1# = ***, f_2# = ***'
        WRITE(CAPT(28:30),'(I3)')NF1
        WRITE(CAPT(40:42),'(I3)')NF2
        LCA=42
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 40 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          IF(ISWIT1.EQ.1) THEN
            YPL(I)=SDFTST(XPL(I),NF1,NF2)
          ELSE IF(ISWIT1.EQ.2) THEN
            YPL(I)=SCFTST(XPL(I),NF1,NF2)
          END IF
40      CONTINUE
      ELSE IF(ISWIT.EQ.5) THEN
C t distribution
        WRITE(*,*)' Enter n (>= 1)'
        WRITE(*,*)' >'
        READ(*,*)N
        XMIN=-3.D0
        XMAX=3.D0
        YMIN=0.D0
        IF(ISWIT1.EQ.1) THEN
          YMAX=0.5D0
        ELSE
          YMAX=1.D0
        END IF
        TX='t'
        LTX=1
        IF(ISWIT1.EQ.1) THEN
          TY='f(t)'
        ELSE IF(ISWIT1.EQ.2) THEN
          TY='F(t)'
        END IF
        LTY=4
        CAPT='t distribution with n = ***'
        WRITE(CAPT(25:27),'(I3)')N
        LCA=27
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 50 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          IF(ISWIT1.EQ.1) THEN
            YPL(I)=SDSTUD(XPL(I),N)
          ELSE IF(ISWIT1.EQ.2) THEN
            YPL(I)=SCSTUD(XPL(I),N)
          END IF
50      CONTINUE
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
