module mesh
    use precision
    use channel 
    use array
    implicit none
    integer(kind=di) :: nl
    real(kind=dp) :: dl
    real(kind=dp), ALLOCATABLE, DIMENSION(:) :: x
contains
    subroutine gen_mesh()
        implicit none
        integer(kind=fi) :: alloc_err
        dl = five
        nl = round(l / dl)
        initial_array(x, nl, zero)
        do i = 1, nl + 1
            x[i] = zero + (i - 1) * dl
        end do
    end subroutine gen_mesh
end module mesh
