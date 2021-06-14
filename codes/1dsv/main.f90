program main
  use array
  use preissmann
  implicit none
  real(kind=8), dimension(:), allocatable :: &
    d_X, d_Z, d_Zb, d_h, d_A, d_B, d_K, d_Q, d_dKdZ
  real(kind=8) :: currentT
  integer(kind=4) :: nT, nX
  integer(kind=4) :: i, j

  nX = nint(totalL / deltaX) + 1
  nT = nint(totalT / deltaT)

  ! initialization
  call initial_array(d_X, nX, 0.0_8)
  call initial_array(d_Z, nX, 0.0_8)
  call initial_array(d_Zb, nX, 0.0_8)
  call initial_array(d_h, nX, 0.0_8)
  call initial_array(d_A, nX, 0.0_8)
  call initial_array(d_B, nX, 0.0_8)
  call initial_array(d_K, nX, 0.0_8)
  call initial_array(d_Q, nX, 0.0_8)


  call initialCoeffs(nX)
  do j = 1, nt
    call resetCoeffs(nX)

    currentT = j * deltaT

    call geometryPars(d_Z, d_Zb, d_A, d_B, d_K, d_dKdZ, nX)
    call preissmannSolver(d_Z, d_A, d_B, d_K, d_Q, d_dKdZ, nX, currentT)
  end do
  call delCoeffs()



  call close_array(d_X)
  call close_array(d_Z)
  call close_array(d_Zb)
  call close_array(d_h)
  call close_array(d_A)
  call close_array(d_B)
  call close_array(d_K)
  call close_array(d_Q)





end program main
