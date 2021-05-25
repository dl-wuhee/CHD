program main
  use array
  implicit none
  real(kind=8), dimension(:, :), allocatable :: x, y, fold, fnew
  real(kind=8) :: h, l
  integer(kind=4) :: n, i, j, k
  real(kind=8) :: maxdiff, fdiff

  l = 2.0_8
  n = 40_4
  h = l / (n-1)

  call initial_array(fold, n, n, 0.0_8)
  call initial_array(fnew, n, n, 0.0_8)
  do i = 1,n 
    do j = 1,n
      fold(i, j) = 0.0_8
    end do
  end do
  do j = 10, 30
    fold(1, j) = 1.0_8
  end do
  fnew = fold

  k = 0
  do
    maxdiff = -999.0
    do i = 2, n-1
      do j = 2, n-1
        fdiff = fnew(i+1,j) + fnew(i-1,j) + fnew(i, j+1) + fnew(i, j-1) - 4.0_8 * fnew(i, j)
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
        fnew(i, j) = 0.25_8 * (fold(i+1,j) + fold(i-1,j) + fold(i, j+1) + fold(i, j-1))
      end do
    end do
    fold = fnew
  end do


  call close_array(fold)
  call close_array(fnew)
end program main
