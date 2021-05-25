module preissmann
  use array
  use const
  implicit none
  real(kind=8), dimension(:), allocatable :: &
    d_A1i, d_B1i, d_C1i, d_D1i, d_E1i, &
    d_A2i, d_B2i, d_C2i, d_D2i, d_E2i
contains
  subroutine initialCoeffs()
    implicit none
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
  end subroutine initialCoeffs

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
  end subroutine delCoeffs

  subroutine calCoeffs(d_Z, d_A, d_B, d_K, d_Q, nX)
    implicit none
    real(kind=8), dimension(:), intent(in) :: &
      d_Z, d_A, d_B, d_K, d_Q
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
        + gThetaDt * QQ_K2_i * (d_B(i) - k_Two * A_K_i * dK_dZ_i)
      d_C2i(i) = k_One + fourThetaDt_dx * Q_A_i1 + twoGThetaDt * AQ_K2_i1
      d_D2i(i) = thetaDt_dx * &
        (-k_Two * Q2_A2_i1 * d_B(i+1) + d_g * (sumA + diffZ * d_B(i+1))) &
        + gThetaDt * QQ_K2_i1 * (d_B(i+1) - k_Two * A_K_i1 * dK_dZ_i1
      d_E2i(i) = dt_dx * (diffQ2_A - d_g * sumA * diffZ) - gDt * sumAQQ_K2
    end do
  end subroutine calCoeffs

end module preissmann
