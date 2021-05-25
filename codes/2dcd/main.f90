program main
  use array
  implicit none
  integer(kind=4) :: n, nt
  real(kind=8):: rnt
  real(kind=8) :: l, h
  real(kind=8) :: t_total, dt, dtold, ct
  real(kind=8) :: u, d
  real(kind=8), dimension(:, :), allocatable :: x, y, f
  real(kind=8), dimension(:), allocatable :: pt
  integer(kind=4) :: i, j, k, ii

  call initial_array(pt, 5_4, 0.0_8)
  do i = 1, 5
    pt(i) = 0.1_8 * i
  end do

  n = 33_4
  l = 2.0_8
  h = l / (n -1)

  u = -1.0_8
  d = 0.025_8
  dt = 0.125_8 * h**2.0_8 / d
  dtold = dt
  !dt = 1.0e-2_8
  
  t_total = 0.5_8
  rnt = t_total /dt
  nt = floor(rnt) 
  !print *, t_total, dt, rnt, nt

  call initial_array(f, n, n, 0.0_8)
  call initial_array(x, n, n, 0.0_8)
  call initial_array(y, n, n, 0.0_8)
  do i = 1, n
    do j = 1, n
      x(i, j) = (i-1) * h
      y(i, j) = (j-1) * h
      f(i, j) = 0.0_8
    end do
  end do
  do j = 12, 21
    f(n, j) = 1.0_8
  end do

  ct = 0.0
  do !k = 1, nt
    dt = dtold
    if (abs(ct - t_total) < 1.0e-6) then
      exit
    end if

    do i = 1, 5
      if (ct < pt(i) .and. ct + dt > pt(i)) then
        dt = pt(i) - ct
        exit
      end if
    end do
    ct = ct + dt

    do i = 2, n-1
      do j = 2, n-1
        f(i, j) = f(i, j) - &
          0.5*u*dt/h * (f(i+1,j)-f(i-1,j)) + &
          d*dt/h/h * &
          (f(i+1,j) + f(i-1,j) + &
          f(i,j+1) + f(i,j-1)-4.0_8 * f(i,j))
      end do
    end do
    do j = 1, n
      f(1, j) = f(2, j)
      f(j, 1) = f(j, 2)
      f(j, n) = f(j, n-1)
    end do

    !print *, ct, dt

<<<<<<< Updated upstream
    do ii = 1, 5
      if (abs(ct - pt(ii)) < 1.0e-6) then
        print *, "#", pt(ii)
=======
    do k = 1, 5
      if (abs(ct - pt(k)) < 1.0e-6) then
        print *, "#", pt(k)
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
        do i = 1, n
          do j = 1, n
            write(*, *)x(i, j), y(i, j), f(i,j)
          end do
          write(*, *)
        end do
      end if
    end do
  end do




  call close_array(f)
  call close_array(x)
  call close_array(y)
end program main
