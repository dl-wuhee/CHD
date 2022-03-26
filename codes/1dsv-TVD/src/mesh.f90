module mesh
    use precision
    use const
    use channel 
    use array
    implicit none
    integer(kind=di) :: nl
    real(kind=dp) :: dl
    real(kind=dp), ALLOCATABLE, DIMENSION(:) :: x
contains
    subroutine gen_mesh()
        implicit none
        integer(kind=di) :: i
        dl = five
        nl = nint(l / dl)
        call initial_array(x, nl+di_1, zero)
        do i = di_1, nl + di_1
            x(i) = zero + (i - 1) * dl
        end do
    end subroutine gen_mesh

    subroutine del_mesh()
        implicit none
        call close_array(x)
    end subroutine del_mesh
end module mesh
