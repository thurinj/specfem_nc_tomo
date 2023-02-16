    ! Create a module to hold alternate string version of latitude, longitude and depth
    ! for string comparison
    ! ----------------
    module dim_names
        character(100) :: latnames(9)
        character(100) :: lonnames(9)
        character(100) :: depnames(9)

        ! Example of how to use this module
        ! --------------------------------
        ! use dim_names
        ! if (any(latnames .eq. 'latitude')) then
        !     print *, 'Found latitude'

        contains 
        subroutine init()
            latnames(1) = 'latitude'
            latnames(2) = 'lat'
            latnames(3) = 'y'
            latnames(4) = 'ydim'
            latnames(5) = 'y_dim'
            latnames(6) = 'y-dim'
            latnames(7) = 'y_dim'
            latnames(8) = 'y-dim'
            latnames(9) = 'ydim'

            lonnames(1) = 'longitude'
            lonnames(2) = 'lon'
            lonnames(3) = 'x'
            lonnames(4) = 'xdim'
            lonnames(5) = 'x_dim'
            lonnames(6) = 'x-dim'
            lonnames(7) = 'x_dim'
            lonnames(8) = 'x-dim'
            lonnames(9) = 'xdim'

            depnames(1) = 'depth'
            depnames(2) = 'dep'
            depnames(3) = 'z'
            depnames(4) = 'zdim'
            depnames(5) = 'z_dim'
            depnames(6) = 'z-dim'
            depnames(7) = 'z_dim'
            depnames(8) = 'z-dim'
            depnames(9) = 'zdim'
        end subroutine init
    end module dim_names