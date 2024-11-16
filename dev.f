C
C       dev and test new functions
C

C
         SUBROUTINE CSPHJY(N,Z, NM,CSJ,CDJ,CSY,CDY)
C
C       =======================================================
C        Purpose: compute spherical Bessel functions and their
C                 derivatives with a complex argument
C        Input :  z --- complex argument
C                 n --- order of jn(z)
C        output:  CSJ(n) --- jn(z)
C                 CDJ(n) --- jn'(z)
C                 CSY(n) --- yn(z)
C                 cpx(n) --- yn'(z)
C                 NM --- Highest order computed
C        Routines called:
C                 MSTA1,and MSTA2 to calculate the starting
C                 point for backward recurrence
C       =======================================================
C
         IMPLICIT COMPLEX*16 (C,Z)
         DOUBLE PRECISION A0
         DIMENSION CSJ(0:N),CDJ(0:N),CSY(0:N),CDY(0:N)

         A0=CDABS(Z)
         NM=N
         IF (A0.LT.1.0D-60) THEN
C                             <<<  Treat z=0 as a special case
            DO 10 K=0,N
               CSJ(K)=0.0D0
               CDJ(K)=0.0D0
               CSY(K)=-1.0D+300
10             CDY(K)=1.0D+300
            CSJ(0)=(1.0D0,0.0D0)
            CDJ(1)=(.3333333333333333D0,0.0D0)
            RETURN
         ENDIF
C

         CSJ(0)=CDSIN(Z)/Z
         CSJ(1)=(CSJ(0)-CDCOS(Z))/Z
         IF (N.GE.2) THEN
C                             <<< Calculate jn(z) by backward recurrence
            CSA=CSJ(0)
            CSB=CSJ(1)
            M=MSTA1(A0,200)
            IF (M.LT.N) THEN
               NM=M
            ELSE
               M=MSTA2(A0,N,15)
            ENDIF
            CF0=0.0D0
            CF1=1.0D0-100
            DO 15 K=M,0,-1
               CF=(2.0D0*K+3.0D0)*CF1/Z-CF0
               IF (K.LE.NM) CSJ(K)=CF
               CF0=CF1
15             CF1=CF
            IF (CDABS(CSA).GT.CDABS(CSB)) CS=CSA/CF
            IF (CDABS(CSA).LE.CDABS(CSB)) CS=CSB/CF0
            DO 20 K=0,NM
20             CSJ(K)=CS*CSJ(K)
         ENDIF

         CDJ(0)=(CDCOS(Z)-CDSIN(Z)/Z)/Z
         DO 25 K=1,NM
C                             <<< Calculate the derivatve ot jn(z)
25          CDJ(K)=CSJ(K-1)-(K+1.0D0)*CSJ(K)/Z
         CSY(0)=-CDCOS(Z)/Z
         CSY(1)=(CSY(0)-CDSIN(Z))/Z
         CDY(0)=(CDSIN(Z)+CDCOS(Z)/Z)/Z
         CDY(1)=(2.0D0*CDY(0)-CDCOS(Z))/Z
         DO 30 K=2,NM
C                             <<< Calculate yn(z) using (8.1.31) or (8.1.32)
            IF (CDABS(CSJ(K-1)).GT.CDABS(CSJ(K-2))) THEN
               CSY(K)=(CSJ(K)*CSY(K-1)-1.0D0/(Z*Z))/CSJ(K-1)
            ELSE
               CSY(K)=(CSJ(K)*CSY(K-2)-(2.0D0*K-1.0D0)/Z**3)/CSJ(K-2)
            ENDIF
30       CONTINUE
         DO 35 K=2,NM
C                             <<< Calcuiate the derivative ot yn(z)
35          CDY(K)=CSY(K-1)-(K+1.0D0)*CSY(K)/Z
         RETURN
         END
C
C



C
        SUBROUTINE TEST_CSPHJY()
         IMPLICIT NONE
         INTEGER I,J,K, M,N, NM
         DOUBLE PRECISION X,Y, RET
         DOUBLE PRECISION SJ(0:100),DJ(0:100),SY(0:100),DY(0:100)
         COMPLEX*16 Z, CSJ(0:100),CDJ(0:100),CSY(0:100),CDY(0:100)

         INTEGER     DATA_X(6), DATA_NN(15)
         COMPLEX*16  DATA_Z(2)
         DATA DATA_X / 1,5,10, 25,50,100 /
         DATA DATA_NN/ 0,1,2,3, 4,5,6,7, 8,9, 10,20,30,50, 100 /
         DATA DATA_Z / (4.0,2.0), (20.0,10.0) /
C
         PRINT *, '[TEST_CSPHJY]'
         PRINT *, '  Test Real X inpit:'
         PRINT *, '  ref: CoSF: Table 8.1, 8.2'
         DO 990 I=1,6
            X = DATA_X(I)
            Z = X
            PRINT *, '[X/Z =', X, ']'
            PRINT *, 'REF:    N, Z,     NM,      CSJ, CDJ, CSY, CDY;;'
            DO 991 J=1,15
               N = DATA_NN(J)
               CALL CSPHJY(N,Z, NM,CSJ,CDJ,CSY,CDY)
               ! REF
               CALL SPHJ(N,X, NM,SJ,DJ)
               CALL SPHY(N,X, NM,SY,DY)
               PRINT *, '   ', N,DREAL(Z),NM
     &            ,DREAL(CSJ(NM)),DREAL(CDJ(NM))
     &            ,DREAL(CSY(NM)),DREAL(CDY(NM))
               PRINT *, 'REF', N,X, NM
     &            ,SJ(NM),DJ(NM),SY(NM),DY(NM), ' ;;'
991         CONTINUE
            PRINT *, ''
990      CONTINUE
         PRINT *, ''
C

         PRINT *, '  Test Complex Z inpit:'
         PRINT *, '  ref: CoSF: Table 8.3~8.6'
         DO 995 I=1,2
            Z = DATA_Z(I)
            PRINT *, '[Z =', Z, ']'
            PRINT *, 'REF:    N,     NM,      CSJ, CDJ, CSY, CDY;;'
            DO 996 J=1,15
               N = DATA_NN(J)
               CALL CSPHJY(N,Z, NM,CSJ,CDJ,CSY,CDY)
               PRINT *, N,NM
     &            ,CSJ(NM),CDJ(NM)
     &            ,CSY(NM),CDY(NM)
996         CONTINUE
            PRINT *, ''
995      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
C
C


C
        PROGRAM MAIN
         IMPLICIT NONE
C
         CALL TEST_CSPHJY()
C
        END PROGRAM
