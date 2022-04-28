module solver
  use precision
  use const
  use channel
  use mesh
  use array
  use time
  use log
  use config
  use unsteady
  use boundary
  use fio
  use output
  implicit none

  abstract interface
    subroutine solve_method(lambda)
      real(kind=8), intent(in) :: lambda
    end subroutine solve_method
  end interface
  !type solver
  !procedure(), pointer :: solve_method_ptr => null()
  !integer(kind=fi) :: index_solve_method
  !real(kind=dp) :: h, a, xi, r, pf, &
  !q, v, sf
  !real(kind=dp), dimension(2) :: vec_u, vec_f, vec_g, &
  !vec_u_1, vec_u_2, vec_u_previous, &
  !end type

  !type(solver), allocatable, dimension(:) :: solver_data

  procedure(solve_method), pointer :: solve_method_ptr => null()
  real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
    h, a, xi, r, c
  real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
    vel, q, cel
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
    vec_u, vec_f
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
    vec_d, vec_e_1, vec_e_2
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
    vec_u_o, vec_u_1, vec_u_2
  real(kind=dp) :: cfl
  ! used in MacCormack TVD scheme
  real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
    v_mid, c_mid
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
    speed, speed_mid, epsi, psi, alfa, phi, visco_d
contains
  subroutine init_solver()
    implicit none
    !call initial_array(solver_data)
    solve_method_ptr => solver_maccormack

    cfl = eight / ten 

    call initial_array(h, nl, zero)
    call initial_array(a, nl, zero)
    call initial_array(xi, nl, zero)
    call initial_array(r, nl, zero)
    call initial_array(v, nl, zero)
    call initial_array(q, nl, zero)
    call initial_array(c, nl, zero)

    call initial_array(vec_u, nl, di_2, zero)
    call initial_array(vec_f, nl, di_2, zero)
    call initial_array(vec_d, nl, di_2, zero)
    call initial_array(vec_e, nl, di_2, zero)
    call initial_array(vec_u_o, nl, di_2, zero)
    call initial_array(vec_u_1, nl, di_2, zero)
    call initial_array(vec_u_2, nl, di_2, zero)
  end subroutine init_solver

  subroutine close_solver()
    implicit none
    !call close_array(solver_data)
    call close_array(h)
    call close_array(a)
    call close_array(xi)
    call close_array(r)
    call close_array(q)
    call close_array(v)
    call close_array(c)

    call close_array(vec_u)
    call close_array(vec_f)
    call close_array(vec_d)
    call close_array(vec_e)
    call close_array(vec_u_o)
    call close_array(vec_u_1)
    call close_array(vec_u_2)

  end subroutine close_solver

  subroutine update(vec_u, nodes)
    implicit none
    real(kind=dp), dimension(:, :), intent(in) :: vec_u
    integer(kind=di), dimension(:), OPTIONAL, intent(in) :: nodes
    integer(kind=di) :: i, n_nodes
    if (present(nodes)) then
      n_nodes = size(nodes)
      do i = di_1, n_nodes
        associate (cur_i => nodes(i))
          h(cur_i) = vec_u(cur_i, 1)
          a(cur_i) = h(cur_i) * b
          xi(cur_i) = wet_perimeter(h(cur_i))
          r(cur_i) = a(cur_i) / xi(cur_i)
          q(cur_i) = vec_u(cur_i, 2)
          vel(cur_i) = q(cur_i) / h(cur_i)
          cel(cur_i) = sqrt(g * h(i))

          vec_f(cur_i, di_1) = q(cur_i)
          vec_f(cur_i, di_2) = flux_f(h(cur_i), vel(cur_i))
        end associate
      end do
    else
      do i = di_2, nl - 1
        h(i) = vec_u(i, 1)
        a(i) = h(i) / b
        xi(i) = wet_perimeter(h(i))
        r(i) = a(i) / xi(i)
        q(i) = vel_u(i, 2)
        vel(i) = q(i) / h(i)
        cel(i) = sqrt(g * h(i))

        vec_f(i, di_1) = q(i)
        vec_f(i, di_2) = flux_f(h(i), vel(i))
      end do
    end if
  contains
    function flux_f(cur_h, cur_v) result (f)
      implicit none
      real(kind=dp), intent(in) :: cur_h, cur_v
      real(kind=dp) :: f
      f = cur_h * cur_v**2 + half * g * cur_h**2
    end function flux_f
  end subroutine update


  subroutine initialize()
    implicit none
    integer(kind=di) :: i 

    call set_bc(vec_u(di_1, di_1), vec_u(nl, di_1), vec_u(di_1, di_2), vec_u(nl, di_2))
    call update(vec_u, (/1, nl/))
    do i = di_2, nl - 1
      h(i) = h(1)
      if (x(i) > three * ten) then
        h(i) = h(nl)
      end if
      vel(i) = zero
      vec_u(i, 1) = h(i)
      vec_u(i, 2) = vel(i)
    end do
    call update(vec_u)
    call output(x, h, vel, nl, t_cur, output_funit)
  end subroutine initialize

  subroutine solve()
    implicit none
    integer(kind=di) :: ti
    real(kind=dp) :: lambda


    call init_solver()

    call initialize()

    do ti = 1, nt
      !do
      t_delta = calc_t_delta(vel, h, dl, cfl)
      if (t_delta < zero) then
        write(log_funit, "(A21, F10.3)") "# Float Error at t = ", t_cur
        exit
      end if
      t_cur = t_cur + t_delta
      if (t_cur - t_total > eps) then
        !call output(x, h, v, a, q, nl, t_cur-t_delta, output_funit)
        write(log_funit, "(A)") "# Done"
        exit
      else
        write(log_funit, fmt="(A16, F10.3, ',', 3X, A19, F10.3)") &
          "# Next time: ", t_cur, "current time step: ", t_delta
      end if

      lambda =  t_delta / dl


      call set_bc(vec_u(di_1, di_1), vec_u(nl, di_1), vec_u(di_1, di_2), vec_u(nl, di_2))
      call update(vec_u, (/1, nl /))


      vec_u_o = vec_u

      call solve_method_ptr(lambda)

      call update(vec_u)

      !if (output_cur <= size(arr_output_t)) then
      if (t_cur == output_cur_t) then
        call output(x, h, vel, nl, t_cur, output_funit)
        output_cur = output_cur + 1
      end if
      !end if
    end do
    call close_solver()
  end subroutine

  subroutine solver_maccormack(lambda)
    implicit none
    real(kind=dp), intent(in) :: lambda
    integer(kind=di) :: i, j
    ! predictor step
    write(log_funit, "(A)") "#    Start of Predictor Step"

    do i = di_2, nl
      do j = di_1, di_2
        vec_u_1(i, j) = vec_u_o(i, j) - lambda * (vec_f(i+di_1, j) - vec_f(i, j))
      end do
    end do

    write(log_funit, "(A)") "#    End of Predictor Step"

    ! update vec_f and vec_g
    call update(vec_u_1)

    ! correct step
    write(log_funit, "(A)")"#    Start of Correct Step"

    do i = di_2, nl
      do j = di_1, di_2
        vec_u_2(i, j) = vec_u_o(i, j) - lambda * (vec_f(i, j) - vec_f(i-di_1, j))
      end do
    end do

    write(log_funit, "(A)")"#    End of Correct Step"

    do i = di_2, nl - di_1
      do j = di_1, di_2
        vec_u(i, j) = half * (vec_u_1(i, j) + vec_u_2(i, j))
      end do
    end do
  end subroutine solver_maccormack

  subroutine solver_maccormack_TVD(lambda)
    implicit none
    real(kind=dp), intent(in) :: lambda
    integer(kind=di) :: i, j
    integer(kind=di) :: n_mid

    call solver_maccormack(lambda)

    write(log_funit, "(A)")"#   Start of TVD" 
    
    n_mid = nl - di_1
    ! base
    do i = di_1, n_mid
      v_mid(i) = (vel(i)*sqrt(h(i)) + vel(i+1)*sqrt(h(i+di_1)) ) / (sqrt(h(i)) + sqrt(h(i+di_1)))
      c_mid(i) = sqrt(half * g * (h(i) + h(i+1)))
      speed(i, di_1) = vel(i) + cel(i)
      speed(i, di_2) = vel(i) - cel(i)
      speed_mid(i, di_1) = v_mid(i) + c_mid(i)
      speed_mid(i, di_2) = v_mid(i) - c_mid(i)
    end do 

    ! cal epsi
    do i = di_1, n_mid
      do j = di_1, di_2
        associate ( &
            diff1 => speed_mid(i, j) - speed(i, j), &
            diff2 => speed(i+1, j) - speed_mid(i, j) )
          epsi(i, j) = zero
          diff1 = speed_mid(i, j) - speed(i, j)
          diff2 = speed(i+1, j) - speed_mid(i, j)
          if (diff1 > epsi(i, j)) then
            epsi(i, j) = diff1
          end if 
          if (diff2 > epsi(i, j)) then
            epsi(i, j) = diff2
          end if 
        end associate
      end do
    end do

    ! cal psi
    do i = di_1, n_mid
      do j = di_1, di_2
        if (abs(speed_mid(i, j)) > epsi(i, j)) then
          psi(i, j) = abs(speed_mid(i, j))
        else
          psi(i, j) = epsi(i, j)
        end if
      end do 
    end do

    ! cal alpha 
    do i = di_1, n_mid
      associate ( &
          h_diff => vec_u(i+1, 1) - vec_u(i, 1), &
          q_diff => vec_u(i+1, 2) - vec_u(i, 2), &
          coeff => half / c_mid(i) )
        alfa(i, 1) = coeff * ( -speed_mid(i, 1) * h_diff + q_diff)
        alfa(i, 2) = coeff * ( speed_mid(i, 2) * h_diff - q_diff)
      end associate
    end do

    ! cal phi 
    do i = di_2, n_mid - di_1
      do j = di_1, di_2
        associate ( s => nint( speed_mid(i, j) / abs( spped_mid(i, j) ) ) )
          if (abs(alfa(i, j)) < eps) then
            r = zero
          else
            r = alfa(i-s, j) / alfa(i, j)
          end if
        end associate
        phi(i, j) = r * (one + r) / (one + r**2)
      end do
    end do i

    ! cal viscocity D
    do i = di_2, n_mid
      do j = di_1, di_2
        factor(i, j) = alfa(i, j) * psi(i, j) * (1 - lambda * abs(speed_mid(i, j))) * (1 - phi(i, j))
      end do
      visco_d(i, 1) = factor(i, 1) + factor(i, 2)
      visco_d(i, 2) = factor(i, 1) * speed_mid(i, 1) + factor(i, 2) * speed_mid(i, 2)
    end do

    ! TVD 
    do i = di_2, nl - di_1
      do j = di_1, di_2
        vec_u(i, j) = vec_u(i, j) + lambda * (visco_d(i, j) - visco_d(i-1, j))
      end do
    end do

    write(log_funit, "(A)")"#   End of TVD" 
  end subroutine solver_maccormack_TVD

end module solver
