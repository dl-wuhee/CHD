module explicit_method
    implicit none
contains
    subroutine ftcs(coeff, nx, t)
        implicit none
        integer(kind=4), intent(in) :: nx
        real(kind=8), dimension(nx), intent(inout) :: t
        real(kind=8), intent(in) :: coeff
        integer(kind=4) :: i

        do i = 2, nx - 1
            t(i) = t(i) + coeff * (t(i+1) - 2 * t(i) + t(i-1))
        end do
    end subroutine ftcs

    subroutine rungekutta3(coeff, nx, t)
        implicit none
        integer(kind=4), intent(in) :: nx
        real(kind=8), dimension(nx), intent(inout) :: t
        real(kind=8), intent(in) :: coeff

        real(kind=8) :: t1, t2
        real(kind=8) :: r
        integer(kind=4) :: i

        do i = 2, nx - 1
            r = coeff * (t(i+1) - 2 * t(i) + t(i-1))
            t1 = t(i) + r
            t2 = 0.75 * t(i) + 0.25 * t1 + 0.25 * r
            t(i) = 1.0_8 / 3.0_8 * t(i) + 2.0_8 / 3.0_8 * t2 + 2.0_8 / 3.0_8 * r 
        end do
    end subroutine rungekutta3
end module explicit_method
