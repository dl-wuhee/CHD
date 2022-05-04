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
    vec_u_o, vec_u_1, vec_u_2
  real(kind=dp) :: cfl
  ! used in MacCormack Artificial Viscosity
  real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
    art_epsi

  ! used in MacCormack TVD scheme
  real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
    v_mid, c_mid
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
    speed, speed_mid, epsi_mid, psi_mid, alfa_mid, phi_mid, factor_mid, visco_mid
contains
  subroutine init_solver()
    implicit none
    integer(kind=di) :: i_method
    !call initial_array(solver_data)
    cfl = eight / ten 

    call initial_array(h, nl, zero)
    call initial_array(a, nl, zero)
    call initial_array(xi, nl, zero)
    call initial_array(r, nl, zero)
    call initial_array(vel, nl, zero)
    call initial_array(q, nl, zero)
    call initial_array(cel, nl, zero)

    call initial_array(vec_u, nl, di_2, zero)
    call initial_array(vec_f, nl, di_2, zero)
    call initial_array(vec_u_o, nl, di_2, zero)
    call initial_array(vec_u_1, nl, di_2, zero)
    call initial_array(vec_u_2, nl, di_2, zero)

    i_method = con_imethod
    select case (i_method)
    case (di_1)
      solve_method_ptr => solver_maccormack
    case (di_2)
      solve_method_ptr => solver_maccormack_artives
      call initial_array(art_epsi, nl, zero)
    case (di_3)
      solve_method_ptr => solver_maccormack_tvd
      call initial_array(speed, nl, di_2, zero)
      associate (n_mid => nl - di_1)
        call initial_array(v_mid, n_mid, zero)
        call initial_array(c_mid, n_mid, zero)
        call initial_array(speed_mid, n_mid, di_2, zero)
        call initial_array(epsi_mid, n_mid, di_2, zero)
        call initial_array(psi_mid, n_mid, di_2, zero)
        call initial_array(alfa_mid, n_mid, di_2, zero)
        call initial_array(phi_mid, n_mid, di_2, zero)
        call initial_array(factor_mid, n_mid, di_2, zero)
        call initial_array(visco_mid, n_mid, di_2, zero)
      end associate
    case default
      solve_method_ptr => solver_maccormack
    end select


    call open_result()
    call open_log()

  end subroutine init_solver

  subroutine close_solver()
    implicit none
    !call close_array(solver_data)
    call close_array(h)
    call close_array(a)
    call close_array(xi)
    call close_array(r)
    call close_array(q)
    call close_array(vel)
    call close_array(cel)

    call close_array(vec_u)
    call close_array(vec_f)
    call close_array(vec_u_o)
    call close_array(vec_u_1)
    call close_array(vec_u_2)

    call close_array(art_epsi)

    call close_array(v_mid)
    call close_array(c_mid)
    call close_array(speed_mid)
    call close_array(epsi_mid)
    call close_array(psi_mid)
    call close_array(alfa_mid)
    call close_array(phi_mid)
    call close_array(factor_mid)
    call close_array(visco_mid)

    call close_log()
    call close_result()

    call del_mesh()
    call del_unsteady()
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
          cel(cur_i) = sqrt(g * h(cur_i))

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
        q(i) = vec_u(i, 2)
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
    call update(vec_u, (/di_1, nl/))
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
    call output_result(x, h, vel, nl, t_cur)
  end subroutine initialize

  subroutine solve()
    implicit none
    integer(kind=di) :: ti
    real(kind=dp) :: lambda


    call init_solver()

    call initialize()

    call write_log("Solve Start", .True.)

    do ti = 1, nt
      !do
      t_delta = calc_t_delta(vel, h, dl, cfl)
      if (t_delta < zero) then
        log_info = ""
        write(log_info, "(A21, F10.3)") "# Float Error at t = ", t_cur
        call write_log(trim(log_info))
        exit
      end if
      t_cur = t_cur + t_delta
      if (t_cur - t_total > eps) then
        !call output_result(x, h, v, a, q, nl, t_cur-t_delta, output_funit)
        log_info = ""
        write(log_info, "(A)") "# Done"
        call write_log(trim(log_info))
        exit
      else
        log_info = ""
        write(log_info, fmt="(A16, F10.3, ',', 3X, A19, F10.3)") &
          "# Next time: ", t_cur, "current time step: ", t_delta
        call write_log(trim(log_info))
      end if

      lambda =  t_delta / dl


      call set_bc(vec_u(di_1, di_1), vec_u(nl, di_1), vec_u(di_1, di_2), vec_u(nl, di_2))
      call update(vec_u, (/di_1, nl /))


      vec_u_o = vec_u

      call solve_method_ptr(lambda)

      call update(vec_u)

      !if (output_cur <= size(arr_output_t)) then
      if (t_cur == output_cur_t) then
        call output_result(x, h, vel, nl, t_cur)
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
    log_info = ""
    write(log_info, "(A)") "#    Start of Predictor Step"
    call write_log(trim(log_info))

    do i = di_2, nl - di_1
      do j = di_1, di_2
        vec_u_1(i, j) = vec_u_o(i, j) - lambda * (vec_f(i+di_1, j) - vec_f(i, j))
      end do
    end do

    log_info = ""
    write(log_info, "(A)") "#    End of Predictor Step"
    call write_log(trim(log_info))

    ! update vec_f and vec_g
    call update(vec_u_1)

    ! correct step
    log_info = ""
    write(log_info, "(A)")"#    Start of Correct Step"
    call write_log(trim(log_info))

    do i = di_2, nl-di_1
      do j = di_1, di_2
        vec_u_2(i, j) = vec_u_o(i, j) - lambda * (vec_f(i, j) - vec_f(i-di_1, j))
      end do
    end do

    log_info = ""
    write(log_info, "(A)")"#    End of Correct Step"
    call write_log(trim(log_info))

    do i = di_2, nl - di_1
      do j = di_1, di_2
        vec_u(i, j) = half * (vec_u_1(i, j) + vec_u_2(i, j))
      end do
    end do
  end subroutine solver_maccormack

  subroutine solver_maccormack_artives(lambda)
    implicit none
    real(kind=dp), intent(in) :: lambda
    integer(kind=di) :: i, j
    real(kind=dp), parameter :: k = two
    real(kind=dp) :: z1, z2

    call solver_maccormack(lambda)

    log_info = ""
    write(log_info, "(A)")"#   Start of Calibrated Artificial Viscosity" 
    call write_log(trim(log_info))


    art_epsi(1) = abs(h(2) - two * h(1) + h(2)) / &
      (abs(h(2)) + two * abs(h(1)) + abs(h(2)))
    do i = di_2, nl - di_1
      art_epsi(i) = abs(h(i+1) - two * h(i) + h(i-1)) / &
        (abs(h(i+1)) + two * abs(h(i)) + abs(h(i-1)))
    end do
    art_epsi(nl) = abs(h(nl-1) - two * h(nl) + h(nl-1)) / &
      (abs(h(nl-1)) + two * abs(h(nl)) + abs(h(nl-1)))

    do i = di_2, nl - di_1
      if (art_epsi(i) > art_epsi(i+1)) then
        z1 = k * art_epsi(i)
      else
        z1 = k * art_epsi(i+1)
      end if
      if (art_epsi(i) > art_epsi(i-1)) then
        z2 = k * art_epsi(i)
      else
        z2 = k * art_epsi(i-1)
      end if
      do j = di_1, di_2
        vec_u(i, j) = vec_u(i, j) + &
          z1 * (vec_u(i+1, j) - vec_u(i, j)) - &
          z2 * (vec_u(i, j) - vec_u(i-1, j))
      end do
    end do

    log_info = ""
    write(log_info, "(A)")"#   End of Calibrated Artificial Viscosity" 
    call write_log(trim(log_info))
  end subroutine solver_maccormack_artives

  subroutine solver_maccormack_TVD(lambda)
    implicit none
    real(kind=dp), intent(in) :: lambda
    real(kind=dp) :: r_mid
    integer(kind=di) :: i, j
    integer(kind=di) :: n_mid

    call solver_maccormack(lambda)

    log_info = ""
    write(log_info, "(A)")"#   Start of TVD" 
    call write_log(trim(log_info))

    n_mid = nl - di_1

    do i = di_1, nl 
      speed(i, di_1) = vel(i) + cel(i)
      speed(i, di_2) = vel(i) - cel(i)
    end do

    ! base
    do i = di_1, n_mid
      v_mid(i) = &
        ( vel(i)*sqrt(h(i)) + vel(i+1)*sqrt(h(i+di_1)) ) / &
        ( sqrt(h(i)) + sqrt(h(i+di_1)) )
      c_mid(i) = sqrt(half * g * (h(i) + h(i+1)))
      speed_mid(i, di_1) = v_mid(i) + c_mid(i)
      speed_mid(i, di_2) = v_mid(i) - c_mid(i)
    end do 

    ! cal epsi
    do i = di_1, n_mid
      do j = di_1, di_2
        epsi_mid(i, j) = zero
        associate ( &
            diff1 => speed_mid(i, j) - speed(i, j), &
            diff2 => speed(i+1, j) - speed_mid(i, j) )
          if (diff1 > epsi_mid(i, j)) then
            epsi_mid(i, j) = diff1
          end if 
          if (diff2 > epsi_mid(i, j)) then
            epsi_mid(i, j) = diff2
          end if 
        end associate
      end do
    end do

    ! cal psi
    do i = di_1, n_mid
      do j = di_1, di_2
        if (abs(speed_mid(i, j)) > epsi_mid(i, j)) then
          psi_mid(i, j) = abs(speed_mid(i, j))
        else
          psi_mid(i, j) = epsi_mid(i, j)
        end if
      end do 
    end do

    ! cal alpha 
    do i = di_1, n_mid
      associate ( &
          h_diff => vec_u(i+1, 1) - vec_u(i, 1), &
          q_diff => vec_u(i+1, 2) - vec_u(i, 2), &
          coeff => half / c_mid(i) )
        alfa_mid(i, 1) = coeff * &
          (-speed_mid(i, 2) * h_diff + q_diff)
        alfa_mid(i, 2) = coeff * &
          ( speed_mid(i, 1) * h_diff - q_diff)
      end associate
    end do

    ! cal phi 
    ! hard to conside while i = 1, and n_mid
    ! as i-s will equal to 0 when i = 1, and 
    ! i-s will equal to nl when i = n_mid
    ! array over bound
    do i = di_1, n_mid
      do j = di_1, di_2
        associate (&
            s => nint(speed_mid(i, j) / abs(speed_mid(i, j))))
          if (abs(alfa_mid(i, j)) < eps) then
            !r = zero
            alfa_mid(i, j) = eps
          end if
          if (i-s == 0 .or. i-s == nl) then 
            r_mid = one
          else
            r_mid = alfa_mid(i-s, j) / alfa_mid(i, j)
          end if
        end associate
        phi_mid(i, j) = r_mid * (one + r_mid) / (one + r_mid**2)
      end do
    end do

    ! cal viscocity D
    do i = di_1, n_mid
      do j = di_1, di_2
        factor_mid(i, j) = alfa_mid(i, j) * psi_mid(i, j) * &
          (one - lambda * abs(speed_mid(i, j))) * &
          (one - phi_mid(i, j))
      end do
      visco_mid(i, 1) = factor_mid(i, 1) + factor_mid(i, 2)
      visco_mid(i, 2) = factor_mid(i, 1) * speed_mid(i, 1) + &
        factor_mid(i, 2) * speed_mid(i, 2)
    end do

    ! TVD 
    do i = di_2, nl - di_1
      do j = di_1, di_2
        vec_u(i, j) = vec_u(i, j) + & 
          lambda * (visco_mid(i, j) - visco_mid(i-1, j))
      end do
    end do

    log_info = ""
    write(log_info, "(A)")"#   End of TVD" 
    call write_log(trim(log_info))
  end subroutine solver_maccormack_TVD

end module solver
