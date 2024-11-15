C
C       dev and test new functions
C

C
         SUBROUTINE STVLV(V,X,SLV)
C
C       ========================================================
C        Purpose: Compute Modified Struve Function Lv(x)
C        Input  : v   --- Order of Lv(x)  ( |v| ≤ 20 )
C                 x   --- Argument of Lv(x)  ( x ≥ 0 )
C        output : SLV --- Lv(x)
C        Routine called: GAMMA2 to compute the gamma function
C       ========================================================
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         PI=3.141592653589793D0
C
         IF (X.EQ.0.0D0) THEN
C                             <<<Treat x=0 as a special case
            IF ((V.GT.-1.0D0).OR.(INT(V)-V.EQ.0.5D0)) THEN
               SLV=0.0D0
            ELSE IF (V.LT.-1.0D0) THEN
               SLV=(-1)**(INT(0.5D0-V)-1)*1.0D+300
            ELSE IF (V.EQ.-1.0D0) THEN
               SLV=2.0D0/PI
            ENDIF
            RETURN
         ENDIF
C
C
         IF (X.LE.40.0D0) THEN
C                             <<<Use (11.2.1)when xs20
            V0=V+1.5D0
            CALL GAMMA2(V0,GA)
            S=2.0D0/(DSQRT(PI)*GA)
            R1=1.0D0
            DO 10 K=1,100
               VA=K+1.5D0
               CALL GAMMA2(VA,GA)
               VB=V+K+1.5D0
               CALL GAMMA2(VB,GB)
               R1=R1*(0.5D0*X)**2
               R2=R1/(GA*GB)
               S=S+R2
               IF(DABS(R2/S).LT.1.0D-12) GO TO 15
10          CONTINUE
15          SLV=(0.5D0*X)**(V+1.0D0)*S
         ELSE
C                                <<<Use(11.2.16)when x>20
            SA=-1.0D0/PI*(0.5D0*X)**(V-1.0D0)
            V0=V+0.5D0
            CALL GAMMA2(V0,GA)
            S=-DSQRT(PI)/GA
            R1=-1.0D0
            DO 20 K=1,12
               VA=K+0.5D0
               CALL GAMMA2(VA,GA)
               VB=-K+V+0.5D0
               CALL GAMMA2(VB,GB)
               R1=-R1/(0.5*X)**2
               S=S+R1*GA/GB
20          CONTINUE
            S0=SA*S
            U=DABS(V)
C                                <<<Calculate 1,(x) using fonward recurence
            N=INT(U)
            U0=U-N
            DO 35 L=0,1
               VT=U0+L
               R=1.0D0
               BIV=1.0D0
               DO 25 K=1,16
                  R=-0.125*R*(4.0D0*VT*VT-(2.0D0*K-1.0D0)**2)/(K*X)
                  BIV=BIV+R
                  IF(DABS(R).LT.DABS(BIV)*1.0D-15) GO TO 30
25             CONTINUE
30             IF (L.EQ.0) BIV0=BIV
35          CONTINUE
            BF0=BIV0
            BF1=BIV
            DO 40 K=2,N
               BF=-2.0D0*(K-1.0D0+U0)/X*BF1+BF0
               BF0=BF1
               BF1=BF
40          CONTINUE
            IF (N.EQ.0) BIV=BIV0
            IF (N.GT.1) BIV=BF
            SLV=DEXP(X)/DSQRT(2.0D0*PI*X)*BIV+S0
         ENDIF
         RETURN
        END
C
C


C
        SUBROUTINE TEST_STVLV()
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION V,X,Y,Z,RET
         PRINT *, '[TEST_STVLV]'
         PRINT *, 'V =', 0, ', X =', 0, ', RET =', 0.0
         DO 111 M=-4,3
            V = M
            DO 222 N=0,21
               X = N
               CALL STVLV(V, X, RET)
               PRINT *, V, ',', X, ',', RET
222         CONTINUE
            PRINT *, ''
111      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
C



C
        PROGRAM MAIN
         IMPLICIT NONE
         INTEGER M,N, NM
         DOUBLE PRECISION X,V,RET
C
         CALL TEST_STVLV()
C
        END PROGRAM
