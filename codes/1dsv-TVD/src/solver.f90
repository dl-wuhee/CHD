module solver
  use const
  use channel
  use mesh
  use array
  use time
  use boundary
  use fio
  implicit none

  !type solver
  !procedure(), pointer :: solve_method_ptr => null()
  !integer(kind=fi) :: index_solve_method
  !real(kind=dp) :: h, a, xi, r, pf, &
  !q, v, sf
  !real(kind=dp), dimension(2) :: vec_u, vec_f, vec_g, &
  !vec_u_1, vec_u_2, vec_u_previous, &
  !end type

  !type(solver), allocatable, dimension(:) :: solver_data

  procedure(), pointer :: solve_method_ptr => null()
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
    integer(kind=di) :: i, j
    integer(kind=di) :: ti
    real(kind=dp) :: lambda


    call init_solver()

    call initialize()

    !do ti = 1, nt
    do
      t_delta = calc_t_delta(v, h, dl, cfl)
      if (t_delta < zero) then
        write(log_funit, "(A)") "Float Error"
        exit
      end if
      t_cur = t_cur + t_delta
      if (t_cur - t_total > eps) then
        call output(x, h, v, a, q, nl, t_cur-t_delta, output_funit)
        write(log_funit, "(A)") "Well Done"
        exit
      else
        write(log_funit, fmt="(A16, F10.3, ',', 3X, A19, F10.3)") &
          "# current time: ", t_cur, "current time step: ", t_delta
      end if

      lambda =  t_delta / dl


      call set_bc(vec_u(di_1, di_1), vec_u(nl+di_1, di_1), vec_u(di_1, di_2), vec_u(nl+di_1, di_2))
      call update(vec_u, (/1, nl+di_1 /))


      vec_u_o = vec_u

      !call McCromark_TVD()

      ! predictor step
       write(log_funit, "(A)") "Start of Predictor Step"

      do i = di_2, nl
        do j = di_1, di_2
          vec_u_1(i, j) = vec_u_o(i, j) - lambda * (vec_f(i+di_1, j) - vec_f(i, j)) + t_delta * vec_g(i, j)
        end do
      end do

       write(log_funit, "(A)") "End of Predictor Step"

      ! update vec_f and vec_g
      call update(vec_u_1)

      ! correct step
       write(log_funit, "(A)")"Start of Correct Step"

      do i = di_2, nl
        do j = di_1, di_2
          vec_u_2(i, j) = vec_u_o(i, j) - lambda * (vec_f(i, j) - vec_f(i-di_1, j)) + t_delta * vec_g(i, j)
        end do
      end do

       write(log_funit, "(A)")"End of Correct Step"

      do i = di_2, nl
        do j = di_1, di_2
          vec_u(i, j) = half * (vec_u_1(i, j) + vec_u_2(i, j)) + half * (zero)
        end do
      end do

      call update(vec_u)

      if (output_cur <= size(arr_output_t)) then
        if (t_cur == arr_output_t(output_cur)) then
          call output(x, h, v, a, q, nl, t_cur, output_funit)
          output_cur = output_cur + 1
        end if
      end if
    end do
    call close_solver()
  end subroutine
end module solver
