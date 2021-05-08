module residual
    implicit none
contains
    function checkresidual(t, to, n) result(l_res)
        implicit none
        real(kind=8), dimension(:), intent(in) :: t, to
        integer(kind=4), intent(in) :: n
        logical :: l_res
        real(kind=8) :: diff_max, diff_i
        real(kind=8), parameter :: eps = 1.0e-9, eps_pre = 1.0e-6
        integer(kind=4) :: i

        diff_max = -9999.9999
        do i = 2, n-1
            diff_i = abs(t(i) - to(i)) / (abs(to(i)) + eps)
            if (diff_i > diff_max) then
                diff_max = diff_i
            end if
        end do
        if (diff_max <= eps_pre) then
            l_res = .true.
        else
            l_res = .false.
        end if
    end function checkresidual
end module residual
