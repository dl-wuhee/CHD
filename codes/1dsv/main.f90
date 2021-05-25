program main
  use array
  implicit none
  real(kind=8), dimension(:), allocatable :: &
    d_X, d_Z, d_Zb, d_h, d_A, d_B, d_K, d_Q!, d_dKdZ
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


  do j = 1, nt
    currentT = j * deltaT
    ! step 1, calculate coefficients
    ! A1i, B1i, C1i, D1i, E1i
    ! A2i, B2i, C2i, D2i, E2i
    do i = 1, nx
    end do

    ! step 2, calculate coefficients
    ! Fi, Gi, Hi, Ii, Ji
    do i = 1, nx
    end do

    !do i = 1, nx
      !! calculate A1i, ..., E2i
      !! calculate Fi, Gi, Hi, Ii, Ji
    !end do

    ! step 3, calculate deltaQ and deltaZ
    do i = nx, 1
    end do

    ! step 4, update Q and Z
    do i = 1, nx
    end do
  end do



  call close_array(d_X)
  call close_array(d_Z)
  call close_array(d_Zb)
  call close_array(d_h)
  call close_array(d_A)
  call close_array(d_B)
  call close_array(d_K)
  call close_array(d_Q)





end program main
