C
C       dev and test new functions
C


C
        SUBROUTINE CJY01(Z, CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1)
C
C       ========================================================
C        Purpose: Compute complex Bessel Functions J0(z), J1(z),
C                 Y0(z), Y1(z), and their derivatives
C        Input  : z    --- Complex argument of Jn(z) and Yn(z)
C        Output : CBJ0 --- J0(z)
C                 CDJ0 --- J0'(z)
C                 CBJ1 --- J1(z)
C                 CDJ1 --- J1'(z)
C                 CBY0 --- Y0(z)
C                 CDY0 --- Y0'(z)
C                 CBY1 --- Y1(z)
C                 CDY1 --- Y1'(z)
C       ========================================================
C
         IMPLICIT DOUBLE PRECISION (A,B,E,P,R,W)
         IMPLICIT COMPLEX*16 (C,Z)
         DIMENSION A(12),B(12),A1(12),B1(12)
         PI=3.141592653589793D0
         EL=0.5772156649015329D0
         EPS=1.0D-15
C
         RP2=2.0D0/PI
         CI=(0.0D0,1.0D0)
         A0=CDABS(Z)
         Z2=Z*Z
         Z1=Z
         IF (A0.EQ.0.0D0) THEN
C                             <<< Treat z=0 as a special case
            CBJ0=(1.0D0,0.0D0)
            CBJ1=(0.0D0,0.0D0)
            CDJ0=(0.0D0,0.0D0)
            CDJ1=(0.5D0,0.0D0)
            CBY0=-(1.0D300,0.0D0)
            CBY1=-(1.0D300,0.0D0)
            CDY0=(1.0D300,0.0D0)
            CDY1=(1.0D300,0.0D0)
            RETURN
         ENDIF
C
C
         IF(REAL(Z).LT.0.0) Z1=-Z
         IF (A0.LE.12.0D0) THEN
C                             <<< Use (5.2.1)-(5.2.4) for |z| ≤ 12
            CBJ0=(1.0D0,0.0D0)
            CR=(1.0D0,0.0D0)
            DO 10 K=1,40
C                             <<< Cakculate (5.2.1)
               CR=-0.25D0*CR*Z2/(K*K)
               CBJ0=CBJ0+CR
               IF(CDABS(CR).LT.CDABS(CBJ0)*EPS) GO TO 15
10          CONTINUE
15          CBJ1=(1.0D0,0.0D0)
            CR=(1.0D0,0.0D0)
            DO 20 K=1,40
C                             <<< Calculate (5_2.2)
               CR=-0.25D0*CR*Z2/(K*(K+1.0D0))
               CBJ1=CBJ1+CR
               IF(CDABS(CR).LT.CDABS(CBJ1)*EPS) GO TO 25
20          CONTINUE
25          CBJ1=0.5D0*Z1*CBJ1
            W0=0.0D0
            CR=(1.0D0,0.0D0)
            CS=(0.0D0,0.0D0)
            DO 30 K=1,40
C                             <<< Calculate (5.2.3)
               W0=W0+1.0D0/K
               CR=-0.25D0*CR/(K*K)*Z2
               CP=CR*W0
               CS=CS+CP
               IF(CDABS(CP).LT.CDABS(CS)*EPS) GO TO 35
30          CONTINUE
35          CBY0=RP2*(CDLOG(Z1/2.0D0)+EL)*CBJ0-RP2*CS
            W1=0.0D0
            CR=(1.0D0,0.0D0)
            CS=(1.0D0,0.0D0)
            DO 40 K=1,40
C                             <<< Calculate (5.2.4)
               W1=W1+1.0D0/K
               CR=-0.25D0*CR/(K*(K+1))*Z2
               CP=CR*(2.0D0*W1+1.0D0/(K+1.0D0))
               CS=CS+CP
               IF(CDABS(CP).LT.CDABS(CS)*EPS)GO TO 45
40          CONTINUE
45          CBY1=RP2*((CDLOG(Z1/2.0D0)+EL)*CBJ1-1.0D0/Z1-0.25D0*Z1*CS)
         ELSE
C                          <<< Use (5.2.5) and (5.2.6) for |z| > 12
            DATA A/  -.703125D-01,.112152099609375D+00,
     &               -.5725014209747314D+00,.6074042001273483D+01,
     &               -.1100171402692467D+03,.3038090510922384D+04,
     &               -.1188384262567832D+06,.6252951493434797D+07,
     &               -.4259392165047669D+09,.3646840080706556D+11,
     &               -.3833534661393944D+13,.4854014686852901D+15/
            DATA B/  .732421875D-01,-.2271080017089844D+00,
     &               .1727727502584457D+01,-.2438052969955606D+02,
     &               .5513358961220206D+03,-.1825775547429318D+05,
     &               .8328593040162893D+06,-.5006958953198893D+08,
     &               .3836255180230433D+10,-.3649010818849833D+12,
     &               .4218971570284096D+14,-.5827244631566907D+16/
            DATA A1/ .1171875D+00,-.144195556640625D+00,
     &               .6765925884246826D+00,-.6883914268109947D+01,
     &               .1215978918765359D+03,-.3302272294480852D+04,
     &               .1276412726461746D+06,-.6656367718817688D+07,
     &               .4502786003050393D+09,-.3833857520742790D+11,
     &               .4011838599133198D+13,-.5060568503314727D+15/
            DATA B1/ -.1025390625D+00,.2775764465332031D+00,
     &               -.1993531733751297D+01,.2724882731126854D+02,
     &               -.6038440767050702D+03,.1971837591223663D+05,
     &               -.8902978767070678D+06,.5310411010968522D+08,
     &               -.4043620325107754D+10,.3827011346598605D+12,
     &               -.4406481417852278D+14,.6065091351222699D+16/
            K0=12
            IF (A0.GE.35.0D0) K0=10
            IF (A0.GE.50.0D0) K0=8
            CT1=Z1-0.25D0*PI
            CP0=(1.0D0,0.0D0)
            DO 50 K=1,K0
50             CP0=CP0+A(K)*Z1**(-2*K)
C                                <<< Cakculate (5.2.9)
            CQ0=-0.125D0/Z1
            Do 55 K=1,K0
55             CQ0=CQ0+B(K)*Z1**(-2*K-1)
C                                <<< Calculate (5.2.10)
            CU=CDSQRT(RP2/Z1)
            CBJ0=CU*(CP0*CDCOS(CT1)-CQ0*CDSIN(CT1))
            CBY0=CU*(CP0*CDSIN(CT1)+CQ0*CDCOS(CT1))

            CT2=Z1-0.75D0*PI
            CP1=(1.0D0,0.0D0)
            DO 60 K=1,K0
60             CP1=CP1+A1(K)*Z1**(-2*K)
C                                <<< Calculate (5.2.11)
            CQ1=0.375D0/Z1
            DO 65 K=1,K0
65             CQ1=CQ1+B1(K)*Z1**(-2*K-1)
C                                <<< Calculate (5.2.12)
            CBJ1=CU*(CP1*CDCOS(CT2)-CQ1*CDSIN(CT2))
            CBY1=CU*(CP1*CDSIN(CT2)+CQ1*CDCOS(CT2))
         ENDIF

         IF (REAL(Z).LT.0.0) THEN
C                                <<< Apppy (5.4.2)
            IF(DIMAG(Z).LT.0.0) CBY0=CBY0-2.0D0*CI*CBJ0
            IF(DIMAG(Z).GT.0.0) CBY0=CBY0+2.0D0*CI*CBJ0
            IF(DIMAG(Z).LT.0.0) CBY1=-(CBY1-2.0D0*CI*CBJ1)
            IF(DIMAG(Z).GT.0.0) CBY1=-(CBY1+2.0D0*CI*CBJ1)
            CBJ1=-CBJ1
         ENDIF
C                                <<< Calculate the derivatives
         CDJ0=-CBJ1
         CDJ1=CBJ0-1.0D0/Z*CBJ1
         CDY0=-CBY1
         CDY1=CBY0-1.0D0/Z*CBY1
         RETURN
       END
C           CJY01(Z, CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1)
C


C
        SUBROUTINE TEST_PSI()
         IMPLICIT NONE
         INTEGER M,N,DATAX(7)
         DOUBLE PRECISION X,Y,RET
         COMPLEX*16 Z, CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1
         DOUBLE PRECISION BJ0,DJ0,BJ1,DJ1, BY0,DY0,BY1,DY1
         COMPLEX*16 DATAZ(2)
         DATA DATAX/ 0,1,5,10, 25,50,100 /
         DATA DATAZ/ (4.0,2.0), (20.0,10.0) /
C
         PRINT *, '[TEST_PSI]'
         PRINT *, '  Test Real X inpit:'
         PRINT *, '  ref: CoSF: Table 5.7, 5.8'
         PRINT *, 'Z,   CBJ0,CDJ0,CBJ1,CDJ1,    CBY0,CDY0,CBY1,CDY1'
         DO 999 N=1,7
            ! N = 7
            M = DATAX(N)
            Z = M
            CALL CJY01(Z, CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1)
            PRINT *, M
     &            ,DREAL(CBJ0),DREAL(CDJ0), DREAL(CBJ1),DREAL(CDJ1)
     &            ,DREAL(CBY0),DREAL(CDY0), DREAL(CBY1),DREAL(CDY1)
            X = M
            CALL JY01A(X, BJ0,DJ0,BJ1,DJ1, BY0,DY0,BY1,DY1)
            PRINT *, M
     &            ,BJ0,DJ0,BJ1,DJ1
     &            ,BY0,DY0,BY1,DY1
999      CONTINUE
         PRINT *, ''
C
         PRINT *, '  Test Complex Z inpit:'
         PRINT *, '  ref: CoSF: Table 5.9, 5.10, 5.11, 5.12'
         PRINT *, 'Z,   CBJ0,CDJ0,CBJ1,CDJ1,    CBY0,CDY0,CBY1,CDY1'
         DO 990 N=1,2
            Z = DATAZ(N)
            CALL CJY01(Z, CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1)
            PRINT *, Z
     &            ,DIMAG(CBJ0),DIMAG(CDJ0), DIMAG(CBJ1),DIMAG(CDJ1)
     &            ,DIMAG(CBY0),DIMAG(CDY0), DIMAG(CBY1),DIMAG(CDY1)
990      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE


C
        PROGRAM MAIN
         IMPLICIT NONE
C
         CALL TEST_PSI()
C
        END PROGRAM
