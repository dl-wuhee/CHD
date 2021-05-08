module implicit_method
    use array
    use linear_solver
    implicit none
contains
    subroutine cranknicolson(coeff, nx, t, a, b, c, d)
        implicit none
        integer(kind=4), intent(in) :: nx
        real(kind=8), dimension(nx), intent(inout) :: t
        real(kind=8), dimension(:), intent(inout) :: a, b, c, d
        real(kind=8), intent(in) :: coeff
        integer(kind=4) :: i

        do i = 1, nx-2
            a(i) = 0.5 * coeff
            b(i) = -1 - coeff
            c(i) = a(i)
            d(i) = -t(i+1) - 0.5 * coeff * (t(i+2) - 2 * t(i+1) + t(i))
        end do
        d(1) = d(1) - a(1) * t(1)
        d(nx-2) = d(nx-2) - a(nx-2) * t(nx)

        call tdma(a, b, c, d, t(2:nx-1), nx-2)
    end subroutine cranknicolson

    subroutine implicitcompactpade(coeff, nx, t, a, b, c, d)
        implicit none
        integer(kind=4), intent(in) :: nx
        real(kind=8), dimension(nx), intent(inout) :: t
        real(kind=8), dimension(:), intent(inout) :: a, b, c, d
        real(kind=8), intent(in) :: coeff
        integer(kind=4) :: i

        do i = 1, nx-2
            a(i) = 1 - 6 * coeff
            b(i) = 10 + 12 * coeff
            c(i) = a(i)
            d(i) = (t(i+2) + 10 * t(i+1) + t(i)) + 6 * coeff * (t(i+2) - 2 * t(i+1) + t(i))
        end do
        d(1) = d(1) - a(1) * t(1)
        d(nx-2) = d(nx-2) - a(nx-2) * t(nx)

        call tdma(a, b, c, d, t(2:nx-1), nx-2)
    end subroutine implicitcompactpade

end module implicit_method

