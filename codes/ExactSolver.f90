subroutine newton_star(hl, hr, vl, vr, hstar, vstar)
  implicit none
  real(kind=8), intent(in) :: hl, hr, vl, vr
  real(kind=8), intent(inout) :: hstar, vstar
  real(kind=8) :: h, hnew
  real(kind=8) :: al, ar, f, dfdh, fl, fr, cstar
  integer(kind=4), parameter :: n = 50
  real(kind=8), parameter :: g = 9.81_8, half = 0.5_8, quad = 0.25_8, eps = 1.0e-6_8

  integer(kind=4) :: i

  al = (g * hl) ** half
  ar = (g * hr) ** half
  h = (half * (al + ar) - quad * (vr - vl)) ** 2.0_8 / g

  do i = 1, n
    call evaluate()
    hnew = h - f / dfdh
    if (abs((hnew - h) / h) < eps) then
      exit
    end if
    h = hnew
  end do

  hstar = hnew
  cstar = (g * hstar)**half
  h = hstar
  call evaluate()
  vstar = half * (vl + vr) + 0.5 * (fr - fl)
contains
  subroutine evaluate()
    implicit none
    real(kind=4) :: dfldh, gl, dfrdh, gr
    ! left wave
    if (h <= hl) then 
      ! rarefaction wave
      fl = 2.0_8 * ( (g * h)**half - (g * hl)**half)
      dfldh = g * (g * h)**(-1.0_8 * half)
    else
      ! shock wave
      fl = (h - hl) * (half * g * (h + hl) / (h * hl))**half
      gl = (half * g * (h + hl) / (h * hl))**half
      dfldh = gl - g * (h - hl) / (4.0_8 * h**2.0_8 * gl) 
    end if

    ! right wave
    if (h < hr) then
      ! rarefaction wave
      fr = 2.0_8 * ((g * h)**half - (g * hr)**half)
      dfrdh = g * (g * h) ** (-1.0_8 * half)
    else
      ! shock wave
      fr = (h -hr) * (half * g * (h + hr) / (h * hr))**half
      gr = (half * g * (h + hr) / (h * hr))**half
      dfrdh = gr - g * (h - hr) / (4.0_8 * h**2.0_8 * gr) 
    end if

    f = fl + fr + vr - vl
    dfdh = dfldh + dfrdh
  end subroutine evaluate
end subroutine newton_star

subroutine waves(hl, vl, hr, vr, hstar, vstar, sl, staill, sr, stailr)
  implicit none
  real(kind=8), intent(in) :: hl, hr, vl, vr, hstar, vstar
  real(kind=8), intent(inout) :: sl, staill, sr, stailr
  real(kind=8), parameter :: g = 9.81_8, half = 0.5_8
  real(kind=8) :: ql, qr
  ! left wave
  if (hstar <= hl) then
    ! rarefaction wave
    sl = vl - (g * hl)**half
    staill = vstar - (g * hstar)**half
  else
    ! shock wave
    ql = (half * (hstar + hl) * hstar / hl**2.0_8)**half
    sl = vl - ql * (g * hl)**half
    staill = sl
  end if
  ! right wave
  if (hstar <= hl) then
    ! rarefaction wave
    sr = vr + (g * hr)**half
    stailr = vstar + (g * hstar)**half
  else
    ! shock wave
    qr = (half * (hstar + hr) * hstar / hr**2.0_8)**half
    sr = vr + qr * (g * hr)**half
    staill = sr
  end if
end subroutine waves

subroutine profile(nx, x, h, v, t, xl, xr, xltail, xrtail, hl, vl, hr, vr, hstar, vstar, sl, staill, sr, stailr)
  implicit none
  real(kind=8), intent(in) :: t
  real(kind=8), intent(in) :: xl, xr, xltail, xrtail
  real(kind=8), intent(in) :: hl, vl, hr, vr, hstar, vstar
  real(kind=8), intent(in) :: sl, staill, sr, stailr
  real(kind=8), dimension(*), intent(inout) :: x, h, v
  real(kind=8), parameter :: g = 9.81_8, half = 0.5_8
  integer(kind=4), intent(in) :: nx
  integer(kind=4) :: i
    do i = 1, nx
      if (x(i) <= xl) then
        h(i) = hl
        v(i) = vl
      end if
      if (x(i) >= xr) then
        h(i) = hr
        v(i) = vr
      end if
      if (x(i) >= xltail .and. x(i) <= xrtail) then
        h(i) = hstar
        v(i) = vstar
      end if
      if (x(i) >= xl .and. x(i) < xltail) then
        h(i) = ((vl + 2.0_8 * (g * hl)**half - x(i) / t)/3.0_8)**half / g
        v(i) = (vl + 2.0_8 * (g * hl)**half + 2.0_8 * x(i) / t) / 3.0_8
      end if
      if (x(i) >= xrtail .and. x(i) < xr) then
        h(i) = ((-vR + 2.0_8 * (g * hr)**half + x(i) / t)/3.0_8)**half / g
        v(i) = (vr - 2.0_8 * (g * hr)**half + 2.0_8 * x(i) / t) / 3.0_8
      end if
    end do
end subroutine profile

program exactsolver
  implicit none
  real(kind=8), parameter :: g = 9.81_8, half = 0.5_8
  real(kind=8) :: t, total_t, delta_t, start_t
  real(kind=8) :: total_x, delta_x,  ori_x
  real(kind=8) :: xl, xltail, xr, xrtail
  real(kind=8) :: hl, hr
  real(kind=8) :: vl, vr
  real(kind=8) :: hstar, vstar
  real(kind=8) :: sl, staill, sr, stailr
  real(kind=8), dimension(:), allocatable :: x, h, v
  integer(kind=4) :: nx, nt
  integer(kind=4) :: i, j

  vl = 10.0_8
  vr = 0.0_8
  hl = 0.2_8
  hr = 0.5_8
  ori_x =0.0_8
  delta_x = 0.1_8
  nx = 200
  total_x = nx * delta_x

  start_t = 0.0_8
  total_t = 2.0_8
  nt = 5
  delta_t = total_t / nt
  t = 0.0_8

  allocate(x(1:nx))
  allocate(h(1:nx))
  allocate(v(1:nx))
  do i = 1, nx
    x(i) = ori_x + (i-1) * delta_x
    if (x(i) < ori_x) then
      h(i) = hl
      v(i) = vl
    else
      h(i) = hr
      v(i) = vr
    end if
  end do

  call newton_star(hl, hr, vl, vr, hstar, vstar)
  call waves(hl, vl, hr, vr, hstar, vstar, sl, staill, sr, stailr)

  do j = 1, nt
    t = j * delta_t
    xl = sl * t
    xltail = staill * t

    xr = sr * t
    xrtail = stailr * t

    call profile(nx, x, h, v, t, xl, xr, xltail, xrtail, hl, vl, hr, vr, hstar, vstar, sl, staill, sr, stailr)

  end do

  print *, "#", t
  do i = 1, nx
    print *, x(i), h(i), v(i)
  end do

  deallocate(x)
  deallocate(h)
  deallocate(v)
end program exactsolver
