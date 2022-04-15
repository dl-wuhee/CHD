module mesh
    use precision
    use const
    use geometry
    use array
    use config, only : con_h
    implicit none
    integer(kind=di) :: nx, ny
    real(kind=dp) :: h
    real(kind=dp), ALLOCATABLE, DIMENSION(:,:) :: x, y
contains
    subroutine gen_mesh()
        implicit none
        integer(kind=di) :: i, j
        h = con_h
        nx = nint(lx / h) + 1
        ny = nint(ly / h) + 1
        call initial_array(x, nx, ny, zero)
        call initial_array(y, nx, ny, zero)
        do i = 1, nx
          do j = 1, ny
            x(i, j) = zero + (i - 1) * h
            y(i, j) = zero + (j - 1) * h
          end do
        end do
    end subroutine gen_mesh

    subroutine del_mesh()
        implicit none
        call close_array(x)
        call close_array(y)
    end subroutine del_mesh
end module mesh
