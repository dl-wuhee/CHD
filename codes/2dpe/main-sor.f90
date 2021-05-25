program main
  use array
  implicit none
  real(kind=8), dimension(:, :), allocatable :: x, y, f
  real(kind=8) :: h, l
  integer(kind=4) :: n, i, j, k
  real(kind=8) :: maxdiff, fdiff
  real(kind=8) :: beta

  beta = 1.8_8

  l = 2.0_8
  n = 40_4
  h = l / (n-1)

  call initial_array(f, n, n, 0.0_8)
  do i = 1,n 
    do j = 1,n
      f(i, j) = 0.0_8
    end do
  end do
  do j = 10, 30
    f(1, j) = 1.0_8
  end do

  k = 0
  do
    maxdiff = -999.0
    do i = 2, n-1
      do j = 2, n-1
        fdiff = f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1) - 4.0_8 * f(i, j)
        if (abs(fdiff) > maxdiff) then
          maxdiff = abs(fdiff)
        end if
      end do
    end do
    if (abs(maxdiff) < 1.0e-6_8) then 
      !print *, k
      exit
    end if
    k = k + 1
    if (k > 5000) then
      exit
    end if
    print *, maxdiff, k

    do i = 2, n-1
      do j = 2, n-1
        f(i, j) = beta * 0.25_8 * (f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1)) + (1.0_8 - beta) * f(i,j)
      end do
    end do
  end do


  call close_array(f)
end program main
