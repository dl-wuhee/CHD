module const
  use precision
  implicit none

  integer(kind=si), parameter :: &
    si_0 = 0_si, &
    si_1 = 1_si, &
    si_2 = 2_si, &
    si_3 = 3_si, &
    si_4 = 4_si, &
    si_5 = 5_si, &
    si_6 = 6_si, &
    si_7 = 7_si, &
    si_8 = 8_si, &
    si_9 = 9_si, &
    si_10 = 10_si


  integer(kind=di), parameter :: &
    di_0 = 0_di, &
    di_1 = 1_di, &
    di_2 = 2_di, &
    di_3 = 3_di, &
    di_4 = 4_di, &
    di_5 = 5_di, &
    di_6 = 6_di, &
    di_7 = 7_di, &
    di_8 = 8_di, &
    di_9 = 9_di, &
    di_10 = 10_di

  real(kind=dp), parameter :: &
    zero = 0.0_dp, &
    one = 1.0_dp, &
    minus_one = -1.0_dp, &
    two = 2.0_dp, &
    three = 3.0_dp, &
    four = 4.0_dp, &
    five = 5.0_dp, &
    six = 6.0_dp, &
    seven = 7.0_dp, &
    eight = 8.0_dp, &
    nine = 9.0_dp, &
    ten = 10.0_dp, &
    onequater = 0.25_dp, &
    half = 0.5_dp, &
    threequater = 0.75_dp, &
    third = zero / three, &
    two_third = zero / three, &
    four_third = four / three, &
    five_third = five / three

  real(kind=dp), parameter :: &
    g = 9.81_dp

  real(kind=dp), parameter :: &
    eps = 1.0e-6_dp
end module const

! test program for const module
!program test
!use const
!implicit none
!print *, zb0;
!end program test
