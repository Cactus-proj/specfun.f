C
C       Test set for NEW functions in specfun.f
C


C
C       -----------------------
C        11. STRUVE FUNCTIONS
C       -----------------------
C
C
        SUBROUTINE TEST_CJY01()
         IMPLICIT NONE
         INTEGER M,N,DATAX(7)
         DOUBLE PRECISION X,Y,RET
         COMPLEX*16 Z, CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1
         DOUBLE PRECISION BJ0,DJ0,BJ1,DJ1, BY0,DY0,BY1,DY1
         COMPLEX*16 DATAZ(2)
         DATA DATAX/ 0,1,5,10, 25,50,100 /
         DATA DATAZ/ (4.0,2.0), (20.0,10.0) /
C
         PRINT *, '[TEST_CJY01]'
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
C       -----------------------
C        5.  BESSEL FUNCTIONS
C       -----------------------
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
        SUBROUTINE TEST_STVHV()
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION V,X,Y,Z,RET
         PRINT *, '[TEST_STVHV]'
         PRINT *, 'V =', 0, ', X =', 0, ', RET =', 0.0
         DO 111 M=-4,3
            V = M
            DO 222 N=0,21
               X = N
               CALL STVHV(V, X, RET)
               PRINT *, V, ',', X, ',', RET
222         CONTINUE
            PRINT *, ''
111      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
C
        SUBROUTINE TEST_STVL0()
         IMPLICIT NONE
         INTEGER M,N
         DOUBLE PRECISION X,Y,Z,RET
         PRINT *, '[TEST_STVL0]'
         PRINT *, 'X =', 0, ', RET =', 0.0
         DO 999 N=0,21
            X = N
            CALL STVL0(X, RET)
            PRINT *, X, ', ', RET
999      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
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
         INTEGER M,N
         DOUBLE PRECISION X,Y,Z,RET
C
         PRINT *, 'Test set for NEW functions in specfun.f'
         PRINT *, '[TEST BEGIN]'
         PRINT *, ''
C
         CALL TEST_CJY01()
         CALL TEST_CJYNA()
C
         CALL TEST_STVH0()
         CALL TEST_STVH1()
         CALL TEST_STVHV()
         CALL TEST_STVL0()
         CALL TEST_STVL1()
         CALL TEST_STVLV()
C
         PRINT *, ''
         PRINT *, '[TEST END]'
        END PROGRAM
