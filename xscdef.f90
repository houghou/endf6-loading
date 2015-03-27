MODULE xscdef
IMPLICIT NONE
  TYPE :: xsc_
      ! --------------------------------------------------
      ! Cross Section components
      ! Refering to the individual XS.
      ! 
      ! Notes:
      ! * The size of the first dimension of 'dat' should
      ! be the number of data points, and the second
      ! dimension should have the size 2.  Therefore, for
      ! example, dat(:,1) should be the energies, and
      ! dat(:, 2) should be the actual XS corresponding to
      ! that energy.
      ! * itype(:) is the interpolation scheme.  The
      ! interpolation scheme for the range dat(i, 2) to
      ! dat(i+1, 2) is itype(i)
      ! --------------------------------------------------
      INTEGER :: np              ! Number of points
      REAL (KIND(1.d0)), ALLOCATABLE :: dat(:, :)
      INTEGER, ALLOCATABLE :: itype(:)
  END TYPE
END MODULE
