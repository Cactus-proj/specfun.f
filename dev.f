C
C       dev and test new functions
C


C
        SUBROUTINE CJYNA(N,Z,  NM,CBJ,CDJ,CBY,CDY)
C
C       ========================================================
C        Purpose: Compute Bessel functions Jn(z) and Yn(z) and
C                 their derivatives for a complex argument
C        Input  : z --- Complex argument of Jn(z)and Yn(z)
C                 n --- order of Jn(z) and Yn(z)
c        Output : CBJ(n) --- Jn(z)
C                 CDJ(n) --- Jn'(z)
C                 CBY(n) --- Yn(z)
C                 CDY(n) --- Yn'(z)
C                 NM --- Highest order computed
C        Routines called:
C             (1) CJY01 to calculate J0(z), J1(z), Y0(z), Y1(z)
C             (2) MSTA1 and MSTA2 to calculate the starting
C                 point for backward recurrence
C       ========================================================
C
         IMPLICIT DOUBLE PRECISION (A,B,E,P,R,W,Y)
         IMPLICIT COMPLEX*16 (C,Z)
         DIMENSION CBJ(0:N),CDJ(0:N),CBY(0:N),CDY(0:N)
         PI=3.141592653589793D0

         A0=CDABS(Z)
         NM=N
         IF(A0.LT.1.0D-100) THEN
C                                <<< Treat z=0 as a special case
            DO 5 K=0,N
               CBJ(K)= (0.0D0,0.0D0)
               CDJ(K)= (0.0D0,0.0D0)
               CBY(K)=-(1.0D+300,0.0D0)
               CDY(K)= (1.0D+300,0.0D0)
5           CONTINUE
            CBJ(0)=(1.0D0,0.0D0)
            CDJ(1)=(0.5D0,0.0D0)
            RETURN
         ENDIF


         CALL CJY01(Z, CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1)
C                                <<< Calculate J0, J1, Y0, and Y1
         CBJ(0)=CBJ0
         CBJ(1)=CBJ1
         CBY(0)=CBY0
         CBY(1)=CBY1
         CDJ(0)=CDJ0
         CDJ(1)=CDJ1
         CDY(0)=CDY0
         CDY(1)=CDY1
         IF (N.LE.1) RETURN

C                                <<< Calculate J0 using forward
C                                    recurrence for N < |z|/4
         IF (N.LT.INT(0.25*A0)) THEN
            CJ0=CBJ0
            CJ1=CBJ1
            DO 70 K=2,N
               CJK=2.0D0*(K-1.0D0)/Z*CJ1-CJ0
               CBJ(K)=CJK
               CJ0=CJ1
               CJ1=CJK
70          CONTINUE
         ELSE
C                                <<< Otherwse, use backward recurrence
            M=MSTA1(A0,200)
            IF (M.LT.N) THEN
               NM=M
            ELSE
               M=MSTA2(A0,N,15)
            ENDIF
            CF2=(0.0D0,0.0D0)
            CF1=(1.0D-100,0.0D0)
            DO 75 K=M,0,-1
               CF=2.0D0*(K+1.0D0)/Z*CF1-CF2
               IF (K.LE.NM) CBJ(K)=CF
               CF2=CF1
               CF1=CF
75          CONTINUE
            IF(CDABS(CBJ0).GT.CDABS(CBJ1)) THEN
               CS=CBJ0/CF
            ELSE
               CS=CBJ1/CF2
            ENDIF
            DO 80 K=0,NM
80             CBJ(K)=CS*CBJ(K)
         ENDIF
C                                <<< Calculate the derivative of Jn
         DO 85 K=2,NM
85          CDJ(K)=CBJ(K-1)-K/Z*CBJ(K)
C  <<< Note:
C        The folowing part calcuatas Yn using mixed recurrence described above.
C        It can be replaced by a simpler aigorithm based on (5.4.19) and (5.4.20)
C
         YA0=CDABS(CBY0)
C                                <<< Determine the turning point B
C                                      approximatety using fonward recurrence
         LB=0
         CG0=CBY0
         CG1=CBY1
         DO 90 K=2,NM
            CYK=2.0D0*(K-1.0D0)/Z*CG1-CG0
            YAK=CDABS(CYK)
            YA1=CDABS(CG0)
            IF ((YAK.LT.YA0).AND.(YAK.LT.YA1)) LB=K
            CBY(K)=CYK
            CG0=CG1
            CG1=CYK
90       CONTINUE
         IF((LB.LE.4).OR.(DIMAG(Z).EQ.0.0D0)) GO TO 125

95       IF(LB.EQ.LB0) GO TO 125
         CH2=(1.0D0,0.0D0)
         CH1=(0.0D0,0.0D0)
         LB0=LB
         DO 100 K=LB,1,-1
            CH0=2.0D0*K/Z*CH1-CH2
            CH2=CH1
            CH1=CH0
100      CONTINUE
C                                <<< Calculato Puz and Pz
         CP12=CH0
         CP22=CH2
         CH2=(0.0D0,0.0D0)
         CH1=(1.0D0,0.0D0)
         DO 105 K=LB,1,-1
            CH0=2.0D0*K/Z*CH1-CH2
            CH2=CH1
            CH1=CH0
105      CONTINUE
C                                <<< Calculate Pi, and Pz
         CP11=CH0
         CP21=CH2
         IF(LB.EQ.NM) CBJ(LB+1) = 2.0D0*LB/Z*CBJ(LB) - CBJ(LB-1)
         IF( CDABS(CBJ(0)) .GT. CDABS(CBJ(1)) ) THEN
            CBY(LB+1) = (CBJ(LB+1)*CBY0 - 2.0D0*CP11/(PI*Z)) / CBJ(0)
            CBY(LB  ) = (CBJ(LB  )*CBY0 + 2.0D0*CP12/(PI*Z)) / CBJ(0)
         ELSE
            CBY(LB+1) = (CBJ(LB+1)*CBY1 - 2.0D0*CP21/(PI*Z)) / CBJ(1)
            CBY(LB  ) = (CBJ(LB  )*CBY1 + 2.0D0*CP22/(PI*Z)) / CBJ(1)
         ENDIF
         CYL2=CBY(LB+1)
         CYL1=CBY(LB)
C                                <<< Calculate Yk using backward
C                                      recurronce tor k <= B
         DO 110 K=LB-1,0,-1
            CYLK=2.0D0*(K+1.0D0)/Z*CYL1-CYL2
            CBY(K)=CYLK
            CYL2=CYL1
            CYL1=CYLK
110      CONTINUE
         CYL1=CBY(LB)
         CYL2=CBY(LB+1)
C                                <<< Calculate Yk using forward
C                                      recurrence for k > B
         DO 115 K=LB+1,N-1
            CYLK=2.0D0*K/Z*CYL2-CYL1
            CBY(K+1)=CYLK
            CYL1=CYL2
            CYL2=CYLK
115      CONTINUE
C                                <<< Checkif B is accurate
C                                      If not, use new B and repeat
         DO 120 K=2,NM
            WA=CDABS(CBY(K))
            IF( WA .LT. CDABS(CBY(K-1)) ) LB=K
120      CONTINUE
         GO TO 95

125      CONTINUE
C                                <<< Calculate the derivative of Yn
         DO 130 K=2,NM
130         CDY(K)=CBY(K-1)-K/Z*CBY(K)
         RETURN
        END
C
C

C
        SUBROUTINE TEST_CJYNA()
         IMPLICIT NONE
         INTEGER I,J,K, M,N, NM, DATAX(7)
         DOUBLE PRECISION X,Y,RET
         COMPLEX*16 Z,CBJ(0:100),CDJ(0:100),CBY(0:100),CDY(0:100)
         DOUBLE PRECISION BJ0,DJ0,BJ1,DJ1, BY0,DY0,BY1,DY1
         COMPLEX*16 DATAZ(2)
         DATA DATAX/ 0,1,5,10, 25,50,100 /
         DATA DATAZ/ (4.0,2.0), (20.0,10.0) /
C
         PRINT *, '[TEST_CJYNA]'
         PRINT *, '  Test Real X inpit:'
         PRINT *, '  ref: CoSF: Table 5.7, 5.8'
         PRINT *, 'N,Z,  NM,CBJ,CDJ,CBY,CDY'
         DO 999 I=1,7
            M = DATAX(I)
            ! ref
            X = M
            CALL JY01A(X, BJ0,DJ0,BJ1,DJ1, BY0,DY0,BY1,DY1)
            Z = M
            N = 0
            CALL CJYNA(N,Z, NM,CBJ,CDJ,CBY,CDY)
            PRINT *, N,M,NM,
     &         DREAL(CBJ(NM)),DREAL(CDJ(NM)),
     &         DREAL(CBY(NM)),DREAL(CDY(NM))
            PRINT *, N,M,999, BJ0,DJ0,BY0,DY0
            N = 1
            CALL CJYNA(N,Z, NM,CBJ,CDJ,CBY,CDY)
            PRINT *, N,M,NM,
     &         DREAL(CBJ(NM)),DREAL(CDJ(NM)),
     &         DREAL(CBY(NM)),DREAL(CDY(NM))
            PRINT *, N,M,999, BJ1,DJ1,BY1,DY1
999      CONTINUE
         PRINT *, ''
C
         PRINT *, '  Test Complex Z inpit:'
         PRINT *, '  ref: CoSF: Table 5.9, 5.10, 5.11, 5.12'
         PRINT *, 'N, NM,CBJ,CDJ,CBY,CDY'
         DO 990 I=1,2
            Z = DATAZ(I)
            PRINT *, 'Z =',Z
            DO 991 J=1,7
               N = DATAX(J)
               CALL CJYNA(N,Z, NM,CBJ,CDJ,CBY,CDY)
               PRINT *, N, NM,CBJ(NM),CDJ(NM),CBY(NM),CDY(NM)
991         CONTINUE
990      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
C


C
        PROGRAM MAIN
         IMPLICIT NONE
C
         CALL TEST_CJYNA()
C
        END PROGRAM
