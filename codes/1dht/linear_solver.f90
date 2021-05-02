module linear_solver
    use array
    implicit none
contains
    subroutine tdma(a, b, c, d, x, n)
        implicit none
        real(kind=8), dimension(:), intent(in) :: a, b, c, d
        real(kind=8), dimension(:), intent(inout) :: x
        real(kind=8), dimension(:), allocatable :: u, l, y
        integer(kind=4) :: n
        integer(kind=4) :: i

        call initial_array(u, n, 0.0_8)
        call initial_array(l, n, 0.0_8)
        call initial_array(y, n, 0.0_8)

        y(1) = d(1)
        u(1) = b(1)

        do i = 2, n
            l(i) = a(i) / u(i-1)
            u(i) = b(i) - l(i) * c(i-1)
            y(i) = d(i) - l(i) * y(i-1)
        end do

        x(n) = y(n) / u(n)
        do i = n-1, 1, -1
            x(i) = (y(i) - c(i) * x(i+1)) / u(i)
        end do

        call close_array(u)
        call close_array(l)
        call close_array(y)
    end subroutine tdma

end module linear_solver
