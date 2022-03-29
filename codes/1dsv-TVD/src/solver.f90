module solver
  use precision
  use const
  use channel
  use mesh
  use array
  use time
  use boundary
  use fio
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
    h, a, xi, r, pf ! pf: hydrostatic pressure force
  real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
    q, v, sf
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
    vec_u, vec_f, vec_g
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
    vec_u_1, vec_u_2, vec_u_o
  real(kind=dp) :: cfl
contains
  subroutine init_solver()
    implicit none
    !call initial_array(solver_data)
    solve_method_ptr => solver_maccormack

    cfl = eight / ten 

    call initial_array(h, nl+di_1, zero)
    call initial_array(a, nl+di_1, zero)
    call initial_array(xi, nl+di_1, zero)
    call initial_array(r, nl+di_1, zero)
    call initial_array(pf, nl+di_1, zero)
    call initial_array(q, nl+di_1, zero)
    call initial_array(v, nl+di_1, zero)
    call initial_array(sf, nl+di_1, zero)

    call initial_array(vec_u, nl+di_1, di_2, zero)
    call initial_array(vec_f, nl+di_1, di_2, zero)
    call initial_array(vec_g, nl+di_1, di_2, zero)

    call initial_array(vec_u_1, nl+di_1, di_2, zero)
    call initial_array(vec_u_2, nl+di_1, di_2, zero)
    call initial_array(vec_u_o, nl+di_1, di_2, zero)

  end subroutine init_solver

  subroutine close_solver()
    implicit none
    !call close_array(solver_data)
    call close_array(h)
    call close_array(a)
    call close_array(xi)
    call close_array(r)
    call close_array(pf)
    call close_array(q)
    call close_array(v)
    call close_array(sf)

    call close_array(vec_u)
    call close_array(vec_f)
    call close_array(vec_g)

    call close_array(vec_u_1)
    call close_array(vec_u_2)
    call close_array(vec_u_o)
  end subroutine close_solver

  subroutine update(vec_u, nodes)
    implicit none
    real(kind=dp), dimension(:, :), intent(in) :: vec_u
    integer(kind=di), dimension(:), OPTIONAL, intent(in) :: nodes
    integer(kind=di) :: i, n_nodes
    if (present(nodes)) then
      n_nodes = size(nodes)
      do i = di_1, n_nodes
        a(nodes(i)) = vec_u(nodes(i), 1)
        q(nodes(i)) = vec_u(nodes(i), 2)
        h(nodes(i)) = a(nodes(i)) / b
        xi(nodes(i)) = wet_perimeter(h(i))
        r(nodes(i)) = a(nodes(i)) / xi(nodes(i))
        pf(nodes(i)) = hydrostatic_pressure_force(a(nodes(i)))
        v(nodes(i)) = q(nodes(i)) / a(nodes(i))
        sf(nodes(i)) = friction_slope(q(nodes(i)), r(nodes(i)), a(nodes(i)))
      end do
      do i = di_1, n_nodes
        vec_f(nodes(i), di_1) = q(nodes(i))
        vec_f(nodes(i), di_2) = flux_f(q(nodes(i)), a(nodes(i)), pf(nodes(i)))
        vec_g(nodes(i), di_1) = zero
        vec_g(nodes(i), di_2) = flux_g(a(nodes(i)), sf(nodes(i)))
      end do
    else
      ! bug here 
      ! caused by the first and last element of vec_u_1 and vec_u_2 
      ! have not been set correctly
      ! to solve bug, just set
      ! do i = di_1, nl + di_1
      ! to
      do i = di_2, nl
        a(i) = vec_u(i, 1)
        q(i) = vec_u(i, 2)
        h(i) = a(i) / b
        xi(i) = wet_perimeter(h(i))
        r(i) = a(i) / xi(i)
        pf(i) = hydrostatic_pressure_force(a(i))
        v(i) = q(i) / a(i)
        sf(i) = friction_slope(q(i), r(i), a(i))
      end do
      do i = di_2, nl
        vec_f(i, di_1) = q(i)
        vec_f(i, di_2) = flux_f(q(i), a(i), pf(i))
        vec_g(i, di_1) = zero
        vec_g(i, di_2) = flux_g(a(i), sf(i))
      end do
    end if
  contains
    function flux_f(l_q, l_a, l_pf) result (f)
      implicit none
      real(kind=dp), intent(in) :: l_q, l_a, l_pf
      real(kind=dp) :: f
      f = l_q ** two / l_a + g * l_pf
    end function flux_f

    function flux_g(l_a, l_sf) result (source_g)
      implicit none
      real(kind=dp), intent(in) :: l_a, l_sf
      real(kind=dp) :: source_g
      source_g = g * l_a * (s0 - l_sf)
    end function flux_g
  end subroutine update


  subroutine initialize()
    implicit none
    integer(kind=di) :: i 

    call set_bc(vec_u(di_1, di_1), vec_u(nl+di_1, di_1), vec_u(di_1, di_2), vec_u(nl+di_1, di_2))
    call update(vec_u, (/1, nl+di_1 /))
    do i = di_2, nl
      h(i) = h(1)
      if (x(i) > three * ten) then
        h(i) = h(nl+di_1)
      end if
      a(i) = h(i) * b
      q(i) = zero
      vec_u(i, 1) = a(i)
      vec_u(i, 2) = q(I)
    end do
    call update(vec_u)
    call output(x, h, v, a, q, nl, t_cur, output_funit)
  end subroutine initialize

  subroutine solve()
    implicit none
    integer(kind=di) :: ti
    real(kind=dp) :: lambda


    call init_solver()

    call initialize()

    do ti = 1, nt
      !do
      t_delta = calc_t_delta(v, h, dl, cfl)
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


      call set_bc(vec_u(di_1, di_1), vec_u(nl+di_1, di_1), vec_u(di_1, di_2), vec_u(nl+di_1, di_2))
      call update(vec_u, (/1, nl+di_1 /))


      vec_u_o = vec_u

      call solve_method_ptr(lambda)

      !call McCromark_TVD()


      call update(vec_u)

      !if (output_cur <= size(arr_output_t)) then
      if (t_cur == output_cur_t) then
        call output(x, h, v, a, q, nl, t_cur, output_funit)
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
        vec_u_1(i, j) = vec_u_o(i, j) - lambda * (vec_f(i+di_1, j) - vec_f(i, j)) + t_delta * vec_g(i, j)
      end do
    end do

    write(log_funit, "(A)") "#    End of Predictor Step"

    ! update vec_f and vec_g
    call update(vec_u_1)

    ! correct step
    write(log_funit, "(A)")"#    Start of Correct Step"

    do i = di_2, nl
      do j = di_1, di_2
        vec_u_2(i, j) = vec_u_o(i, j) - lambda * (vec_f(i, j) - vec_f(i-di_1, j)) + t_delta * vec_g(i, j)
      end do
    end do

    write(log_funit, "(A)")"#    End of Correct Step"

    do i = di_2, nl
      do j = di_1, di_2
        vec_u(i, j) = half * (vec_u_1(i, j) + vec_u_2(i, j))
      end do
    end do
  end subroutine solver_maccormack

  subroutine solver_maccormack_TVD(lambda)
    implicit none
    real(kind=dp), intent(in) :: lambda
    integer(kind=di) :: i, j

    call solver_maccormack(lambda)

    do i = di_2, nl

        u_bar = u_mid(i)
        c_bar = c_mid(i)
        a_bar_1 = u_bar + c_bar
        a_bar_2 = u_bar - c_bar
        abs_a_bar_1 = abs(a_bar_1)
        abs_a_bar_2 = abs(a_bar_2)
        e_bar_1(1) = 1
        e_bar_1(2) = a_bar_1
        e_bar_2(1) = 1
        e_bar_2(2) = a_bar_2
        psi_1 = psi(abs_a_bar_1)
        psi_2 = psi(abs_a_bar_2)
        alpha_0_1 = alpha(u_bar, c_bar, one, i)
        alpha_0_2 = alpha(u_bar, c_bar, minus_one, i)


      vec_u(i, 1) = lambda * ()
      do j = di_1, di_2
        vec_u(i, j) = vec_u(i, j) + 
      end do
    end do

  contains
    function cal_r_mid(cur_alpha, cur_i) result (r_mid)
      implicit none
      integer(kind=di), intent(in) :: cur_i
      real(kind=dp), intent(in) :: cur_alpha
      real(kind=dp) :: r_mid 
    end function cal_r_mid

    function cal_alpha(u_bar, c_bar, signal, cur_i) result (alpha)
      implicit none
      integer(kind=di), intent(in) :: cur_i
      real(kind=dp), intent(in) :: u_bar, c_bar, signal
      real(kind=dp) :: alpha, new_c_bar
      new_c_bar = signal * c_bar
      alpha = half / new_c_bar * & (
        q(cur_i + di_1) - q(cur_i) + &
        (-u_bar + new_c_bar) * (a(cur_i + di_1) - a(cur_i)) )
    end function cal_alpha

    function cal_psi(a_bar) result (psi)
      implicit none
      real(kind=dp), intent(in) :: a_bar
      real(kind=dp) :: delta
      delta = 0.2
      if (a_bar >= delta) then
        psi = a_bar
      else
        psi = delta
      end if
    end function cal_psi

    function cal_u_mid(cur_i) result (u_mid)
      integer(kind=di), intent(in) :: cur_i
      real(kind=dp) :: q_0, q_1, a_0, a_1
      real(kind=dp) :: u_mid
      q_0 = q(cur_i)
      q_1 = q(cur_i + di_1)
      a_0 = sqrt(a(cur_i))
      a_1 = sqrt(a(cur_i + di_1))
      
      u_mid = (q_1 / a_1 + q_0 / a_0) / (a_1 + a_0)
    end function cal_u_mid

    function cal_c_mid(cur_i) result (c_mid)
      integer(kind=di), intent(in) :: cur_i
      real(kind=dp) :: h_0, h_1
      real(kind=dp) :: c_mid
      h_0 = h(cur_i)
      h_1 = h(cur_i + di_1)
      
      c_mid = (sqrt(g * h_0) + sqrt(g * h_1)) ** 2 / four
    end function cal_c_mid


  end subroutine solver_maccormack_TVD

end module solver
