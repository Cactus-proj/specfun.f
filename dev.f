C
C       dev and test new functions
C


C
        SUBROUTINE STVL0(X,SL0)
C
C       ===================================================
C        Purpose: Compute Modified Struve Function L0(x)
C        Input  : x   --- Argument of L0(x)  ( x ≥ 0 )
C        output : SL0 --- L0(x)
C       ===================================================
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         PI=3.141592653589793D0
         EPS=1.0D-12
C
         S=1.0D0
         R=1.0D0
         IF (X.LE.20.0D0) THEN
C                             <<< Use (11.2.2) when x ≤ 20
            A0=2.0D0*X/PI
            DO 10 K=1,60
               R=R*(X/(2.0D0*K+1.0D0))**2
               S=S+R
               IF(DABS(R/S).LT.EPS) GO TO 15
10          CONTINUE
15          SL0=A0*S
         ELSE
C                             <<< Use (11.2.17) when x > 20
            KM=INT(0.5*(X+1.0))
            IF (X.GE.50.0D0) KM=25
            DO 20 K=1,KM
               R=R*((2.0D0*K-1.0D0)/X)**2
               S=S+R
               IF (DABS(R/S).LT.EPS) GO TO 25
20          CONTINUE
25          A1=DEXP(X)/DSQRT(2.0D0*PI*X)
            R=1.0D0
            BI0=1.0D0
C                             <<< Cakculale I0(x) using (6.2.1)
            DO 30 K=1,16
               R=0.125D0*R*(2.0D0*K-1.0D0)**2/(K*X)
               BI0=BI0+R
               IF(DABS(R/BI0).LT.EPS) GO TO 35
30          CONTINUE
35          BI0=A1*BI0
            SL0=-2.0D0/(PI*X)*S+BI0
         ENDIF
         RETURN
         END
C           STVL0(X,SL0)
C

        SUBROUTINE TEST_STVL0()
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION X,Y,Z,RET
         PRINT *, '[TEST_STVL0]'
         PRINT *, 'X =', 0, ', RET =', 0.0
         DO 999 N=0,40
            X = N
            CALL STVL0(X, RET)
            PRINT *, X, ', ', RET
999      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE


C
        PROGRAM MAIN
         IMPLICIT NONE
         INTEGER M,N, NM
         DOUBLE PRECISION X,V,RET
C
         CALL TEST_STVL0()
C
        END PROGRAM
