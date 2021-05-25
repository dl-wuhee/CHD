module test
  use array
  implicit none
  real(kind=8), dimension(:), allocatable :: testA
contains
  subroutine calA(n)
    implicit none
    integer(kind=4), intent(in) :: n
    call initial_array(testA, n, 1.0_8)
    print *, testA
    call close_array(testA)
  end subroutine calA
end module test

program main
  use test
  implicit none
  integer(kind=4) :: n
  n = 5
  call calA(5)
end program main
