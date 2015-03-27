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
CONTAINS
    SUBROUTINE load_endf6_xs(u_xs, req_mt, xsc, amass)
        ! ======================================================================
        ! Purposes:
        ! This SUBROUTINE loads cross sections from ENDF6 format databases.
        ! It also optinoally loads the atomic weight from the same database.
        !
        ! Arguments:
        ! * u_xs: Fortran IO unit number of the database file.
        ! * req_mt: MT number (of the ENDF/VI format) requested.
        ! * xsc: A variable of type 'xsc_' used for storing loaded cross section
        ! data.
        ! * amass: When specified, will be used for storing the atomic mass read
        ! from the database.
        !
        ! Variables:
        ! * intp = Interpolation scheme.  Ref to ENDF6 manual.
        ! * nbt = Number of points that use that interpolation scheme.  Ref to
        ! ENDF6 manual.
        ! * nr = Number of interpolation regions.  Ref to ENDF6 manual.
        ! * np = Number of points.  Ref to ENDF6 manual.
        !
        ! Notes:
        ! * MF number is not specified in the list of arguments, becase this
        ! SUBROUTINE is intended for cross section only, and MF should always be
        ! 23.
        ! * The ENDF6 format data file should have been opened before entering
        ! this SUBROUTINE.  This SUBROUTINE does not open or close it.
        ! ======================================================================
        INTEGER, INTENT (IN) :: u_xs
        INTEGER, INTENT (IN) :: req_mt
        TYPE (xsc_), INTENT (INOUT) :: xsc
        REAL (kind(1.D0)), INTENT (OUT), OPTIONAL :: amass
        INTEGER :: mf, mt, nr, np
        INTEGER, ALLOCATABLE :: intp(:), nbt(:)
        INTEGER :: i
        CHARACTER (LEN=11) :: ctmp  ! Used for reading atomic mass.
        LOGICAL :: lopened
        
        INQUIRE(UNIT=u_xs, OPENED=lopened)
        IF (.NOT. lopened) &
            ERROR STOP 'load_endf6_xs: Cross Section file not opened.'
        REWIND (u_xs)
        DO
            READ (u_xs, '(11x, a11, 48x, I2, I3)') ctmp, mf, mt
            IF (mf/=23 .OR. mt/=req_mt) THEN
                CYCLE
            ELSE
                IF (present(amass)) READ (ctmp, '(e11.0)') amass
                READ (u_xs, '(44X, i11, i11)') nr, np
                xsc%np = np
                ALLOCATE (xsc%dat(np,2), xsc%itype(np-1))
                ALLOCATE (nbt(nr), intp(nr))
                ! Set up the interpolation scheme
                READ (u_xs, '(6i11)')(nbt(i), intp(i), i=1, nr)
                DO i = nr, 1, -1
                    ! This is a simpler implementation.  The next loop will
                    ! partially rewrite the itype array.
                    xsc%itype(1:nbt(i)-1) = intp(i)
                END DO
                ! Loads the Cross Section data
                READ (u_xs, '(6E11.0)')(xsc%dat(i,1), xsc%dat(i,2), i=1, np)
                ! Convert the energy unit from [eV] to [MeV].
                xsc%dat(:, 1) = xsc%dat(:, 1)/1.D6
                EXIT
            END IF
        END DO
    END SUBROUTINE
END MODULE

