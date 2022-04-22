module geometry
  use precision
  implicit none

  public lx, ly

  real(kind=dp), parameter :: &
    lx = 75.0_dp, &
    ly = 50.0_dp
end module geometry