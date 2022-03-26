module fio
    use const
    implicit none
contains
    subroutine output(x, h, v, a, q, nl, t_cur)
        implicit none
        real(kind=dp), DIMENSION(*), intent(in) :: x, h, v, a, q
        real(kind=dp) :: t_cur
        integer(kind=di) :: nl
        integer(kind=di) :: i
        write(*, "(A70)") repeat("+", 70)
        write(*, "('#', A3, ',', 5(A10, ','), A10)")"i", "t", "x", "h", "v", "area", "discharge" 
        do i = di_1, nl+di_1
            write(*, "(I4, ',', 5(F10.3, ','), F10.3)") i, t_cur, x(i), h(i), v(i), a(i), q(i)
        end do
        write(*, "(A70)") repeat("-", 70)
        write(*, *)
    end subroutine output
end module fio
