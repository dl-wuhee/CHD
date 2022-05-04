module mesh
    use precision
    use const
    use channel 
    use array
    use config, only : con_dx
    implicit none
    integer(kind=di) :: nl
    real(kind=dp) :: dl
    real(kind=dp), ALLOCATABLE, DIMENSION(:) :: x
contains
    subroutine gen_mesh()
        implicit none
        integer(kind=di) :: i
        !dl = ten * five
        !nl = nint(l / dl)
        dl = con_dx
        nl = nint(l / dl) + di_1
        call initial_array(x, nl, zero)
        do i = di_1, nl
            x(i) = zero + (i - di_1) * dl
        end do
    end subroutine gen_mesh

    subroutine del_mesh()
        implicit none
        call close_array(x)
    end subroutine del_mesh
end module mesh
