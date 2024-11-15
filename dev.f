C
C       dev and test new functions
C


         SUBROUTINE STVL1(X,SL1)
C
C
C        Purpose: compute modified struve function L1(x)
C        Input; Argument of L1(x)(x ≥0)x
C        Output: SL1 L1(x)C
C
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
         PI=3.141592653589793D0
         EPS=1.0D-12
C
         R=1.0D0
         IF (X.LE.20.0D0)THEN
C <<<Use(11.2.3) when xs20
            S=0.0D0
            DO 10 K=1,60
               R=R*X*X/(4.0D0*K*K-1.0D0)
               S=S+R
               IF(DABS(R/S).LT.EPS) GO TO 15
10          CONTINUE
15          SL1=2.0D0/PI*S
         ELSE
C <<<Use(11.2.18)when x>20
            S=1.0D0
            KM=INT(0.5*X)
            IF (X.GT.50.0D0) KM=25
            DO 20 K=1,KM
               R=R*(2.0D0*K+3.0D0)*(2.0D0*K+1.0D0)/(X*X)
               S=S+R
               IF(DABS(R/S).LT.EPS) GO TO 25
20          CONTINUE
25          SL1=2.0D0/PI*(-1.0D0+1.0D0/(X*X)+3.0D0*S/X**4)
            A1=DEXP(X)/DSQRT(2.0D0*PI*X)
            R=10D0
            BI1=1.0D0
C <<<Calculate 4(x)uslng(6.2.1)
            DO 30 K=1,16
               R=-0.125D0*R*(4.0D0-(2.0D0*K-1.0D0)**2)/(K*X)
               BI1=BI1+R
               IF(DABS(R).LT.DABS(BI1)*EPS) GO TO 35
30          CONTINUE
35          SL1=SL1+A1*BI1
         ENDIF
         RETURN
         END
C           STVL1(X,SL1)
C


C
        SUBROUTINE TEST_STVL1()
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION X,Y,Z,RET
         PRINT *, '[TEST_STVL1]'
         PRINT *, 'X =', 0, ', RET =', 0.0
         DO 999 N=0,40
            X = N
            CALL STVL1(X, RET)
            PRINT *, X, ', ', RET
999      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
C


C
        PROGRAM MAIN
         IMPLICIT NONE
         INTEGER M,N, NM
         DOUBLE PRECISION X,V,RET
C
         CALL TEST_STVL1()
C
        END PROGRAM
