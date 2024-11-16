# specfun.f

Special functions `specfun.f` from SciPy.

## Background and Project Aim

The SciPy project rewrote specfun in C/C++ and deleted it
([last commit for `specfun.f` ](https://github.com/scipy/scipy/blob/23ed5fed8f9450446fc0aad0acb5002d4d7f84f7/scipy/special/specfun/specfun.f)),
then further rewrote it as a header-only C++ library ([scipy/xsf](https://github.com/scipy/xsf)).

- This project aims to keep the original CoSF `specfun.f` version as a reference for testing purposes.
- This project does *not accept* any modifications for precision, errors, etc.
- This project will add algorithms that are only given in the CoSF book, but not included with `specfun.f`.
- For a refactored version using modern fortran, see: [jacobwilliams/specfun](https://github.com/jacobwilliams/specfun)

## Missing functions

```fortran
C
C       -----------------------
C        5.  BESSEL FUNCTIONS
C       -----------------------
C
C           CJY01(Z,  CBJ0,CDJ0,CBJ1,CDJ1, CBY0,CDY0,CBY1,CDY1):
C                   Compute complex Bessel Functions J0(z), J1(z),
C                   Y0(z), Y1(z), and their derivatives
C           CJYNA(N,Z,NM,  CBJ,CDJ,CBY,CDY)
C                   Compute Bessel functions Jn(z) and Yn(z) and
C                   their derivatives for a complex argument
C
C       ------------------------------------
C        8.  SPHERICAL BESSEL FUNCTIONS
C       ------------------------------------
C
C           CSPHJY(N,Z,NM,  CSJ,CDJ,CSY,CDY)
C           CSPHIK(N,Z,NM,  CSI,CDI,CSK,CDK)
C
C       -----------------------
C        11. STRUVE FUNCTIONS
C       -----------------------
C
C           STVH0(X,SHO):     Compute struve function H0(x)
C           STVH1(X,SH1):     Compute:struve function H1(x)
C           STVHV(V,X,HV):    Compute struve function Hv(x) with
C                 arbitrary order v ( -8.0 ≤ v ≤ 12.5 )
C           STVL0(X,SL0):     Compute modified struve function L0(x)
C           STVL1(X,SL1):     Compute modified struve function L1(x)
C           STVLV(V,X,SLV):   Compute modified struve function Lv(x)
C
```

### TODO

```fortran
C       8.  SPHERICAL BESSEL FUNCTIONS
C       8.1 SPHERICAL BESSEL FUNCTIONS
C       8.3 MODIFIED SPHERICAL BESSEL FUNCTIONS
CSPHJY(N,Z,NM,  CSJ,CDJ,CSY,CDY)
CSPHIK(N,Z,NM,  CSI,CDI,CSK,CDK)
```


## License

```fortran
C       SPDX-License-Identifier: BSD-3-Clause
```

```fortran
C       COMPUTATION OF SPECIAL FUNCTIONS
C
C          Shanjie Zhang and Jianming Jin
C
C       Copyrighted but permission granted to use code in programs.
C       Buy their book "Computation of Special Functions", 1996, John Wiley & Sons, Inc.
```
