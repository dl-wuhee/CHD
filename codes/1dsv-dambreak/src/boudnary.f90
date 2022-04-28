module boundary
  use const
  use unsteady 
  use channel
  implicit none
contains
  subroutine set_bc(up_h, up_v, down_h, down_v)
    implicit none
    real(kind=dp), intent(inout) :: up_h, up_v, down_h, down_v
    up_h = two / ten
    up_v = one / ten
    down_h = zero
    down_v = zero
  end subroutine set_bc 

  ! to do
  ! z=z(t), Q=Q(t)
  ! 1. upstream z, downstream Q
  ! 2. upstream Q, downstream z
  ! 3. upstream Q, downstream z=f(Q)
  ! 4. upstream z, downstream Q=f(t)

end module boundary
