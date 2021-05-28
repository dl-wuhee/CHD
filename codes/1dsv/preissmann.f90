module preissmann
  use array
  use const
  implicit none
  real(kind=8), dimension(:), allocatable :: &
    d_A1i, d_B1i, d_C1i, d_D1i, d_E1i, &
    d_A2i, d_B2i, d_C2i, d_D2i, d_E2i
  real(kind=8), dimension(:), allocatable :: &
    d_Fi, d_Gi, d_Hi, d_Ii, d_Ji
  real(kind=8), dimension(:), allocatable :: &
    d_dQi, d_dZi
contains
  subroutine initialCoeffs(nx)
    implicit none
    integer(kind=4), intent(in) :: nx
    call initial_array(d_A1i, nx, 0.0_8)
    call initial_array(d_B1i, nx, 0.0_8)
    call initial_array(d_C1i, nx, 0.0_8)
    call initial_array(d_D1i, nx, 0.0_8)
    call initial_array(d_E1i, nx, 0.0_8)
    call initial_array(d_A2i, nx, 0.0_8)
    call initial_array(d_B2i, nx, 0.0_8)
    call initial_array(d_C2i, nx, 0.0_8)
    call initial_array(d_D2i, nx, 0.0_8)
    call initial_array(d_E2i, nx, 0.0_8)

    call initial_array(d_Fi, nx, 0.0_8)
    call initial_array(d_Gi, nx, 0.0_8)
    call initial_array(d_Hi, nx, 0.0_8)
    call initial_array(d_Ii, nx, 0.0_8)
    call initial_array(d_Ji, nx, 0.0_8)

    call initial_array(d_dQi, nx, 0.0_8)
    call initial_array(d_dZi, nx, 0.0_8)
  end subroutine initialCoeffs

  subroutine resetCoeffs(nX)
    implicit none
    integer(kind=4), intent(in) :: nx
    d_A1i = 0.0_8
    d_B1i = 0.0_8
    d_C1i = 0.0_8
    d_D1i = 0.0_8
    d_E1i = 0.0_8
    d_A2i = 0.0_8
    d_B2i = 0.0_8
    d_C2i = 0.0_8
    d_D2i = 0.0_8
    d_E2i = 0.0_8

    d_Fi = 0.0_8
    d_Gi = 0.0_8
    d_Hi = 0.0_8
    d_Ii = 0.0_8
    d_Ji = 0.0_8

    d_dQi = 0.0_8
    d_dZi = 0.0_8
  end subroutine resetCoeffs

  subroutine delCoeffs()
    implicit none
    call close_array(d_A1i)
    call close_array(d_B1i)
    call close_array(d_C1i)
    call close_array(d_D1i)
    call close_array(d_E1i)
    call close_array(d_A2i)
    call close_array(d_B2i)
    call close_array(d_C2i)
    call close_array(d_D2i)
    call close_array(d_E2i)

    call close_array(d_Fi)
    call close_array(d_Gi)
    call close_array(d_Hi)
    call close_array(d_Ii)
    call close_array(d_Ji)

    call close_array(d_dQi)
    call close_array(d_dZi)
  end subroutine delCoeffs

  subroutine calCoeffs(d_Z, d_A, d_B, d_K, d_Q, d_dKdZ,nX)
    implicit none
    real(kind=8), dimension(:), intent(in) :: &
      d_Z, d_A, d_B, d_K, d_Q, d_dKdZ
    integer(kind=4), intent(in) :: nX
    integer(kind=4) :: i
    real(kind=8) :: dt_dx, thetaDt_dx, fourDt_dx, fourThetaDt_dx, gDt, gThetaDt, twoGThetaDt
    real(kind=8) :: sumB, diffQ
    real(kind=8) :: sumA, diffZ, diffQ2_A, sumAQQ_K2
    real(kind=8) :: Q_A_i, Q2_A2_i, QQ_K2_i, AQ_K2_i, A_K_i
    real(kind=8) :: Q_A_i1, Q2_A2_i1, QQ_K2_i1, AQ_K2_i1, A_K_i1
    real(kind=8), parameter :: k_One = 1.0_8, k_Two = 2.0_8, k_Four = 4.0_8


    dt_dx = deltaT / deltaX
    fourDt_dx = k_Four * dt_dx
    thetaDt_dx = d_theta * dt_dx
    fourThetaDt_dx = k_Four * thetaDt_dx
    gDt = d_g * deltaT
    gThetaDt = d_theta * gDt
    twoGThetaDt = k_Two * gThetaDt

    do i = 1, nX
      sumB = d_B(i+1) + d_B(i)
      diffQ = d_Q(i+1) - d_Q(i)
      d_A1i(i) = - fourThetaDt_dx / sumB
      d_B1i(i) = k_One - fourThetaDt_dx / sumB**k_Two * d_m * diffQ
      d_C1i(i) = - d_A1i(i)
      d_D1i(i) = d_B1i(i)
      d_E1i(i) = fourDt_dx / sumB * diffQ

      sumA = d_A(i+1) + d_A(i)
      diffZ = d_Z(i+1) - d_Z(i)
      diffQ2_A = k_Two * (d_Q(i)**k_Two / d_A(i) - d_Q(i+1)**k_Two / d_A(i+1))
      Q_A_i = d_Q(i) / d_A(i) 
      Q_A_i1 = d_Q(i+1) / d_A(i+1)
      Q2_A2_i = Q_A_i ** k_Two
      Q2_A2_i1 = Q_A_i1 ** k_Two
      QQ_K2_i = d_Q(i) * abs(d_Q(i)) / d_K(i)**k_Two
      QQ_K2_i1 = d_Q(i+1) * abs(d_Q(i+1)) / d_K(i+1)**k_Two
      AQ_K2_i = d_A(i) * abs(d_Q(i)) / d_K(i)**k_Two
      AQ_K2_i1 = d_A(i+1) * abs(d_Q(i+1)) / d_K(i+1)**k_Two
      sumAQQ_K2 = AQ_K2_i * d_Q(i) + AQ_K2_i1 * d_Q(i+1)
      A_K_i = d_A(i) / d_K(i)
      A_K_i1 = d_A(i+1) / d_K(i+1)

      d_A2i(i) = k_One - fourThetaDt_dx * Q_A_i + twoGThetaDt * AQ_K2_i
      d_B2i(i) = thetaDt_dx * &
        (k_Two * Q2_A2_i * d_B(i) - d_g * (sumA - diffZ * d_B(i))) &
        + gThetaDt * QQ_K2_i * (d_B(i) - k_Two * A_K_i * d_dKdZ(i))
      d_C2i(i) = k_One + fourThetaDt_dx * Q_A_i1 + twoGThetaDt * AQ_K2_i1
      d_D2i(i) = thetaDt_dx * &
        (-k_Two * Q2_A2_i1 * d_B(i+1) + d_g * (sumA + diffZ * d_B(i+1))) &
        + gThetaDt * QQ_K2_i1 * (d_B(i+1) - k_Two * A_K_i1 * d_dKdZ(i+1)
      d_E2i(i) = dt_dx * (diffQ2_A - d_g * sumA * diffZ) - gDt * sumAQQ_K2
    end do
  end subroutine calCoeffs

  function upbondary(ct) result(dQ)
    implicit none
    real(kind=8), intent(in) :: ct
    real(kind=8) :: dQ
    ! to do

  end function downboundary

  subroutine chasingCoeffs(nX, ct)
    implicit none
    real(kind=8), intent(in) :: ct
    integer(kind=4), intent(in) :: nX
    integer(kind=4) :: i
    real(kind=4) :: alpha1, alpha2, alpha3
    ! Upstream boundary condition, Q=Q(t)
    d_Fi(1) = 0
    d_dQi(1) = upboudary(ct) ! return deltaQ at current time
    d_Gi(1) = d_dQi(1)
    do i = 1, nx-1
      alpha1 = d_A1i(i) * d_Fi(i) + d_B1i(i)
      d_Hi(i) = -d_C1i(i) / alpha1
      d_Ii(i) = -d_D1i(i) / alpha1
      d_Ji(i) = (d_E1i(i) - d_A1i(i) * d_Gi(i)) / alpha1
      alpha2 =  d_A2i(i) * d_Fi(i) + d_B2i(i)
      alpha3 = alpha2 * d_Hi(i) + d_C2i(i)
      d_Fi(i+1) = -(alpha2 * d_Ii(i) + d_D2i(i)) / alpha3
      d_Gi(i+1) = (d_E2i - alpha2 * d_Ii(i) - d_A2i(i) * d_Gi(i)) / alpha3
    end do
  end subroutine chasingCoeffs

  function downbondary(ct) result(dZ)
    implicit none
    real(kind=8), intent(in) :: ct
    real(kind=8) :: dZ
    ! to do


  end function downboundary

  subroutine chasing(nX, ct)
    implicit none
    real(kind=8), intent(in) :: ct
    integer(kind=4), intent(in) :: nX
    integer(kind=4) :: i
    ! Downstream boundary condition, Z=Z(t)
    d_dZi(nX) = downboundary(ct) ! return deltaZ at current time
    d_dQi(nX) = d_Fi(nX) * d_dZ(nX) + d_Gi(nX)
    do i = nX-1, 2, -1
      d_dZi(i) = d_Hi(i) * d_dQi(i+1) + d_Ii(i) * d_dZi(i+1) + d_Ji(i)
      d_dQi(i) = d_Fi(i) * d_dZi(i) + d_Gi(i)
    end do
    d_dZi(1) = d_Hi(1) * d_dQi(2) + d_Ii(1) * d_dZi(2) + d_Ji(1)
  end subroutine chasing

  subroutine updateZQ(nX, d_Z, d_Q)
    implicit none
    integer(kind=4), intent(in) :: nX
    real(kind=8), dimension(:), intent(inout) :: d_Z, d_Q
    integer(kind=4) :: i
    do i = 1, nX
      d_Z(i) = d_Z(i) + d_dZi(i)
      d_Q(i) = d_Q(i) + d_dQi(i)
    end do
  end subroutine updateZQ

  subroutine preissmannSolver(d_Z, d_A, d_B, d_K, d_Q, d_dKdZ, nX, ct)
    implicit none
    real(kind=8), dimension(:), intent(in) :: &
      d_A, d_B, d_K, d_dKdZ
    real(kind=8), dimension(:), intent(inout) :: &
      d_Z, d_Q
    integer(kind=4), intent(in) :: nX
    real(kind=8), intent(in) :: ct

    ! step 1, calculate coefficients
    ! A1i, B1i, C1i, D1i, E1i
    ! A2i, B2i, C2i, D2i, E2i
    call calCoeffs(d_Z, d_A, d_B, d_K, d_Q, d_dKdZ, nX)

    ! step 2, calculate coefficients
    ! Fi, Gi, Hi, Ii, Ji
    call chasingCoeffs(nX, currentT)

    ! step 3, calculate deltaQ and deltaZ
    call chasing(nX, currentT)

    ! step 4, update Q and Z
    call updateZQ(nX, d_Z, d_Q)
  end subroutine preissmannSolver


end module preissmann
