! Fortran90 program to import NetCDF files from the IRIS EMC
! The goal is to import the data to pass it to specfem3d_globe
! This is the first version of the program intended to be used
! with the IRIS EMC data. It is not intended to be used with
! other data sets.
! This is a SUPER early prototype that will be converted to a module
! in the future, and will be used to import external tomography
! models.
! Currently: this program reads a netcdf model file and prints
! out the dimensions and variables.  It is intended to be used
! as a template for reading netcdf files.

! The program check that the variable's dimensions
! are in the correct order, as the 3D array might be stored
! in a different order than the dimensions index points to.

! Compile on Mac with :
! gfortran-12 ncmodel_import.f90 -o ncmodel_import -I/opt/homebrew/Cellar/netcdf-fortran/4.6.0/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.0/lib -lnetcdff

! Compile on chinook04 with :
! > module purge && module load GCC/11.3.0 OpenMPI/4.1.4 netCDF-Fortran/4.5.4
! > gfortran ncmodel_import.f90 -o ncmodel_import -I/usr/local/pkg/MPI/GCC/11.3.0/OpenMPI/4.1.4/netCDF-Fortran/4.5.4/include -lnetcdff

program read_ncmodel
    use netcdf
    implicit none

    ! Define variables
    ! ----------------
    integer :: ncid, varid, dimid, status ! standard netcdf variables
    integer :: i, j, k ! looping integers
    integer :: latid, lonid, depid ! dimension ids
    integer :: latlen, lonlen, deplen ! dimension lengths
    integer :: dimlens(3) ! dimension lengths (for array allocation purposes)
    integer :: nvars, ndims, ngatts, unlimdimid ! netcdf file info
    integer :: dimids(3) ! dimension ids
    integer, dimension(:), allocatable :: varids ! variable ids
    integer :: varorderdims(3) ! variable dimension order
    ! Define arrays
    ! -------------
    real, dimension(:), allocatable :: lat, lon, dep ! lat, lon, dep arrays
    real, dimension(:,:,:), allocatable :: vp, vs, rho ! model arrays

    ! Define strings
    ! --------------
    character(len=100) :: varname ! variable name
    character(len=100) :: dimname ! dimension name
    character(len=100) :: attname ! attribute name
    character(len=100) :: attvalue ! attribute value
    character(len=100) :: latname, lonname, depname ! dimension names
    character(len=100), dimension(:), allocatable :: varnames ! variable names
    ! Define expected dimension names
    ! IDEA : Later, will be read from Parfile OR looked up in a hardcoded
    ! dictionnary of 'standard' variable names for lat, lon, depths
    ! see mod_dimnames.f90 for reference
    character(len=100) :: latname_exp = 'latitude'
    character(len=100) :: lonname_exp = 'longitude'
    character(len=100) :: depname_exp = 'depth'
    ! Define expected variable names
    ! IDEA : Later, will be read from Parfile
    character(len=100) :: vpname_exp = 'vp'
    character(len=100) :: vsname_exp = 'vs'
    character(len=100) :: rhoname_exp = 'rho'

    ! Define other variables
    ! ----------------------
    logical :: islat, islon, isdep ! flags for dimension names
    integer :: dimlen ! dimension length
    integer :: varndim ! variable number of dimensions
    integer :: varid_vp, varid_vs, varid_rho ! variable ids


    ! Open netcdf file with write access
    ! ----------------
    call check_status(nf90_open('model.nc', nf90_nowrite, ncid))

    call list_dims(ncid, latid, lonid, depid, latlen, lonlen, deplen)

    ! Check that expected dimensions are present and in correct order
    print *, 'latid = ', latid
    print *, 'lonid = ', lonid
    print *, 'depid = ', depid

    ! Check that the expected variable names are present
    call check_varnames(ncid, varid_vp, varid_vs, varid_rho)

    ! Debug prints for coding
    ! print *, 'varid_vp = ', varid_vp
    ! print *, 'varid_vs = ', varid_vs
    ! print *, 'varid_rho = ', varid_rho

    ! Check that the variable's dimensions are in the correct order
    call check_dimorder(ncid, varid_vp, latid, lonid, depid, varorderdims)
    dimlens = (/latlen, lonlen, deplen/)

    print *, 'Index order of dimensions for stored values: ', varorderdims

    ! Allocate variables vp, vs, rho based on dimension lengths determined above
    allocate(vp(dimlens(varorderdims(1)), dimlens(varorderdims(2)), dimlens(varorderdims(3))))
    allocate(vs(dimlens(varorderdims(1)), dimlens(varorderdims(2)), dimlens(varorderdims(3))))
    allocate(rho(dimlens(varorderdims(1)), dimlens(varorderdims(2)), dimlens(varorderdims(3))))

    ! Set the vill_value of the nc variables (vp, vs, rho) to 0 with
    ! nf90_def_var_fill
    ! NOT WORKING.
    ! call check_status(nf90_def_var_fill(ncid, varid_vp, nf90_fill, 0.0))


    ! Read vp, vs, rho. Replace missing values with 0
    call check_status(nf90_get_var(ncid, varid_vp, vp))
    call check_status(nf90_get_var(ncid, varid_vs, vs))
    call check_status(nf90_get_var(ncid, varid_rho, rho))

    print *, 'vp min = ', minval(vp)
    print *, 'vp max = ', maxval(vp)
    print *, 'vs min = ', minval(vs)
    print *, 'vs max = ', maxval(vs)
    print *, 'rho min = ', minval(rho)
    print *, 'rho max = ', maxval(rho)


    ! Close netcdf file
    call check_status(nf90_close(ncid))

    contains

    ! Subroutine to check status of netcdf operation
    ! --------------------------------
    subroutine check_status(status)
        integer, intent(in) :: status
        if (status /= nf90_noerr) then
            write(*,*) 'Error: ', nf90_strerror(status)
            stop
        endif
    end subroutine check_status

    ! Subroutine to return dimension ids, names, and lengths
    ! ---------------
    subroutine list_dims(ncid, latid, lonid, depid, latlen, lonlen, deplen)

        implicit none
        integer, intent(in) :: ncid
        integer :: dimid, status
        integer :: dimlen
        character(len=100) :: dimname
        integer :: ndims
        integer, intent(out) :: latid, lonid, depid
        integer, intent(out) :: latlen, lonlen, deplen
        logical :: islat

        ! Get number of dimensions
        call check_status(nf90_inquire(ncid, ndimensions=ndims))
        write(*,*) 'Number of dimensions: ', ndims


        do dimid = 1, ndims
            call check_status(nf90_inquire_dimension(ncid, dimid, dimname, dimlen))
            write(*,*) 'Dimension', dimid, ': ', trim(dimname), ' =', dimlen
            ! Assign dimension ids and lengths
            if (trim(dimname) == latname_exp) then
                latid = dimid
                latlen = dimlen
            elseif (trim(dimname) == lonname_exp) then
                lonid = dimid
                lonlen = dimlen
            elseif (trim(dimname) == depname_exp) then
                depid = dimid
                deplen = dimlen
            endif
        enddo

    end subroutine list_dims

    ! Subroutine to check that the expected variable names are present
    ! ----------------
    subroutine check_varnames(ncid, varid_vp, varid_vs, varid_rho)
        implicit none
        integer, intent(in) :: ncid
        integer :: varid, status
        integer :: nvars
        character(len=100) :: varname
        integer, intent(out) :: varid_vp, varid_vs, varid_rho

        ! Get number of variables
        call check_status(nf90_inquire(ncid, nvariables=nvars))
        write(*,*) 'Number of variables: ', nvars

        do varid = 1, nvars
            call check_status(nf90_inquire_variable(ncid, varid, varname))
            write(*,*) 'Variable', varid, ': ', trim(varname)
            ! Assign variable ids
            if (trim(varname) == vpname_exp) then
                varid_vp = varid
            elseif (trim(varname) == vsname_exp) then
                varid_vs = varid
            elseif (trim(varname) == rhoname_exp) then
                varid_rho = varid
            endif
        enddo

    end subroutine check_varnames

    ! Subroutine to check the dimension order of a variable
    ! ----------------
    subroutine check_dimorder(ncid, varid, latid, lonid, depid, varorderdims)
        implicit none
        integer, intent(in) :: ncid
        integer, intent(in) :: varid
        integer, intent(in) :: latid, lonid, depid
        integer, intent(in out) :: varorderdims(3)
        integer :: dimids(3)
        integer :: status
        integer :: varndim
        integer :: dimid
        character(len=100) :: dimname
        logical :: islat, islon, isdep

        ! Get number of dimensions
        call check_status(nf90_inquire_variable(ncid, varid, ndims=varndim))
        write(*,*) 'Number of dimensions stored in variables: ', varndim

        ! Get dimension ids
        call check_status(nf90_inquire_variable(ncid, varid, dimids=dimids))

        ! Check that the expected dimensions are present
        do dimid = 1, varndim
            call check_status(nf90_inquire_dimension(ncid, dimids(dimid), dimname))
            write(*,*) 'Dimension', dimid, ': ', trim(dimname)
            ! Assign dimension ids and lengths
            if (trim(dimname) == latname_exp) then
                islat = .true.
            elseif (trim(dimname) == lonname_exp) then
                islon = .true.
            elseif (trim(dimname) == depname_exp) then
                isdep = .true.
            endif
            ! Write Variable dimension order to varorderdims
            varorderdims(dimid) = dimids(dimid)
        enddo

        if (.not. islat .or. .not. islon .or. .not. isdep) then
            write(*,*) 'Error: expected dimensions not present'
            stop
        endif
    end subroutine check_dimorder

end program read_ncmodel
