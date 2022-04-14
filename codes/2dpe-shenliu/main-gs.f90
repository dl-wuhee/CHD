program main
  use array
  implicit none
  real(kind=8), dimension(:, :), allocatable :: x, y, f
  real(kind=8) :: h, l
  integer(kind=4) :: n, i, j, k
  real(kind=8) :: maxdiff, fdiff
  character(len=255) :: resultfile

  l = 2.0_8
  n = 40_4
  h = l / (n-1)

  call initial_array(x, n, n, 0.0_8)
  call initial_array(y, n, n, 0.0_8)
  call initial_array(f, n, n, 0.0_8)
  do i = 1,n 
    do j = 1,n
      x(i, j) = (i-1) * h
      y(i, j) = (j-1) * h
      f(i, j) = 0.0_8
    end do
  end do
  !do j = 10, 30
    !f(1, j) = 1.0_8
  !end do
  do i = 1, n
    f(1, i) = 15.0_8
    f(n, i) = 15.0_8
    f(i, 1) = 15.0_8
  end do
  do i = 16, 24
    do j = 38, 40
      f(i, j) = 0.0
    end do
  end do
  do i = 1, 15
    f(i, n) = 15 - i + 1
  end do
  do i = 26, 39
    f(i, n) = i - 25 
  end do
  do j = 30, 38
    f(27, j) = 0.0
  end do

  k = 0
  do
    maxdiff = -999.0
    do i = 2, n-1
      do j = 2, n-1
        if ( i >= 16 .and. i <= 24 .and. j >= 38 .and. j<= 40) then
          continue
        else
          if (i == 17 .and. j >= 30 .and. j <= 38) then
            continue
          else
            fdiff = f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1) - 4.0_8 * f(i, j)
            if (abs(fdiff) > maxdiff) then
              maxdiff = abs(fdiff)
            end if
          end if
        end if
    end do
  end do
  if (abs(maxdiff) < 1.0e-6_8) then 
    !print *, k
    print *, maxdiff, k
    exit
  end if
  k = k + 1
  if (k > 500000) then
    print *, "Failed to conveger!"
    exit
  end if

    do i = 2, n-1
      do j = 2, n-1
        if ( i >= 16 .and. i <= 24 .and. j >= 38 .and. j<= 40) then
          continue
        else
          if (i == 17 .and. j >= 30 .and. j <= 38) then
            continue
          else
            f(i, j) = 0.25_8 * (f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1))
          end if
        end if
      end do
    end do
  end do

  resultfile = "result.dat"
  open(10, file=resultfile, status="unknown", action="write")
  do i = 1, n
    do j = 1,n
      write(10, fmt="(3(1x, f10.6))") x(i,j), y(i,j), f(i, j) 
    end do
    write(10, fmt=*)
  end do
  close(10)

  call close_array(f)
end program main
