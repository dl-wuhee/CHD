module openchannel
  use const
  implicit none
contains
  function area(zs, zb) result (area)
    implicit none
    real(kind=8), intent(in) :: zs, zb
    real(kind=8) :: h, area
    h = z - zb
    area = (d_b0 + d_m * h) * h
  end function area

  function width(zs, zb) result(width)
    implicit none
    real(kind=8), intent(in) :: zs, zb
    real(kind=8) :: h, width
    h = zs - zb
    width = d_b0 + 2.0_8 * h * d_m
  end function width

  function wetPerimeter(zs, zb) result(wetPerimeter)
    implicit none
    real(kind=8), intent(in) :: zs, zb
    real(kind=8) :: h, width
    h = zs - zb
    wetPerimeter = d_b0 + 2.0_8 * h * sqrt(1.0_8 + d_m**2.0_8)
  end function wetPerimeter

  function flowModulus(zs, zb) result(flowModulus)
    implicit none
    real(kind=8), intent(in) :: zs, zb
    real(kind=8) :: h, area1, wetPerimeter1, flowModulus
    area1 = area(zs, zb)
    wetPerimeter1 = wetPerimeter(zs, zb)
    flowModulus = area1**(5.0_8/3.0_8) / (wetPerimeter**(2.0_8/3.0_8) * d_n)
  end function flowModulus

end module openchannel
