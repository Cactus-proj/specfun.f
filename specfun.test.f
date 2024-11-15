C
C       Test set for NEW functions in specfun.f
C


C       ========================================================================
C        11. STRUVE FUNCTIONS
C       ========================================================================
C
C           STVH0(X,SHO):     Compute struve function H0(x)
C           STVH1(X,SH1):     Compute:struve function H1(x)
C           STVHV(V,X,HV):    Compute struve function Hv(x) with
C                 arbitrary order v ( -8.0 ≤ v ≤ 12.5 )
C           STVL0(X,SL0):     Compute modified struve function L0(x)
C           STVL1(X,SL1):     Compute modified struve function L1(x)
C           STVLV(V,X,SLV):   Compute modified struve function Lv(x)
C

        SUBROUTINE TEST_STVH0()
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION X,Y,Z,RET
         PRINT *, '[TEST_STVH0]'
         PRINT *, 'X =', 0, ', RET =', 0.0
         DO 999 N=0,21
            X = N
            CALL STVH0(X, RET)
            PRINT *, X, ', ', RET
999      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
C
        SUBROUTINE TEST_STVH1()
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION X,Y,Z,RET
         PRINT *, '[TEST_STVH1]'
         PRINT *, 'X =', 0, ', RET =', 0.0
         DO 999 N=0,21
            X = N
            CALL STVH1(X, RET)
            PRINT *, X, ', ', RET
999      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE

C
        PROGRAM MAIN
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION X,Y,Z,RET
C
         PRINT *, 'Test set for NEW functions in specfun.f'
         PRINT *, '[TEST BEGIN]'
         PRINT *, ''
C
         CALL TEST_STVH0()
         CALL TEST_STVH1()
C
         PRINT *, ''
         PRINT *, '[TEST END]'
        END PROGRAM
