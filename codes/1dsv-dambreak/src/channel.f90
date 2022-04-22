module channel
    use const 
    implicit none
    real(kind=dp) :: l, b, s0, n
contains
    subroutine init_channel()
        !l = 2000_dp;
        l = 100.0_dp;
        b = 1.0_dp;
        n = 0.0_dp;
    end subroutine init_channel

    function area(h) result(a)
        implicit none
        real(kind=dp), intent(in) :: h
        real(kind=dp) :: a
        a = b * h
    end function

    function wet_perimeter(h) result(p)
        implicit none
        real(kind=dp), intent(in) :: h
        real(kind=dp) :: p
        p = b + two * h
    end function wet_perimeter

    function hydraulic_radius(h) result (r)
        implicit none
        real(kind=dp), intent(in) :: h
        real(kind=dp) :: r
        r = area(h) / wet_perimeter(h)
    end function hydraulic_radius

    function friction_slope(q, r, a) result (sf)
        implicit none
        real(kind=dp), intent(in) :: r, q, a
        real(kind=dp) :: sf
        sf = (q * n / a) ** two / r ** four_third 
    end function friction_slope
end module channel 
