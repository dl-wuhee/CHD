module solver
    use const
    use channel
    use mesh
    use array
    use time
    use boundary
    implicit none
    real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
        h, a, xi, r, pf ! pf: hydrostatic pressure force
    real(kind=dp), ALLOCATABLE, DIMENSION(:) :: &
        q, v, sf
    real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
        vec_u, vec_f, vec_g
    real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: &
        vec_u_1, vec_u_2, vec_u_o
contains
    subroutine init_solver()
        implicit none
        call initial_array(h, nl, zero)
        call initial_array(a, nl, zero)
        call initial_array(xi, nl, zero)
        call initial_array(r, nl, zero)
        call initial_array(pf, nl, zero)
        call initial_array(q, nl, zero)
        call initial_array(v, nl, zero)
        call initial_array(sf, nl, zero)

        call initial_array(vec_u, nl, di_2, zero)
        call initial_array(vec_f, nl, di_2, zero)
        call initial_array(vec_g, nl, di_2, zero)

        call initial_array(vec_u_1, nl, di_2, zero)
        call initial_array(vec_u_2, nl, di_2, zero)
        call initial_array(vec_u_o, nl, di_2, zero)
        call initial_array(vec_f_1, nl, di_2, zero)
        call initial_array(vec_g_1, nl, di_2, zero)
    end subroutine init_solver

    subroutine close_solver()
        implicit none
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
        call close_array(vec_f_1)
        call close_array(vec_g_1)
    end subroutine close_solver

    subroutine update(vec_u)
        implicit none
        real(kind=dp), dimension(*, *), intent(in) :: vec_u
        integer(kind=di) :: i 
        do i = di_1, nl + di_1
            a[i] = vec_u[i, 1]
            q[i] = vec_u[i, 2]
            h[i] = a[i] / b
            xi[i] = wet_perimeter(h[i])
            r[i] = a[i] / xi[i]
            pf[i] = hydrostatic_pressure_force(a[i])
            v[i] = q[i] / a[i]
            sf[i] = friction_slope(h[i], q[i], r[i], a[i])
        end do
        do i = di_1, nl + di_1
            vec_f[i, di_1] = q[i]
            vec_f[i, di_2] = flux_f(q[i], a[i], pf[i])
            vec_g[i, di_1] = zero
            vec_g[i, di_2] = flux_g(a[i], sf[i])
        end do
    contains
        function flux_f(l_q, l_a, l_pf) result (f)
            implicit none
            real(kind=dp), intent(in) :: l_q, l_a, l_pf
            real(kind=dp) :: f
            f = q ** two / a + g * pf
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
        do i = di_1, nl + di_1
            h[i] = h[1]
            if ((x[i] - 1000_dp) > zero) then
                h[i] = h[nl+di_1]
            end if
            q[i] = zero

            a[i] = area(h[i])
            xi[i] = wet_perimeter(h[i])
            r[i] = a[i] / xi[i]
            pf[i] = hydrostatic_pressure_force(a[i])
            v[i] = q[i] / a[i]
            sf[i] = friction_slope(h[i], q[i], r[i], a[i])

            vec_f[i, di_1] = q[i]
            vec_f[i, di_2] = flux_f(q[i], a[i], pf[i])
            vec_g[i, di_1] = zero
            vec_g[i, di_2] = flux_g(a[i], sf[i])
        end do
    end subroutine initialize

    subroutine solve()
        implicit none
        integer(kind=di) :: i, j
        integer(kind=di) :: ti
        real(kind=dp) :: lambda

        lambda = dl / dx

        call initialize()
        do ti = 1, nt
            t_delta = calc_t_delta(v, dx)
            t_cur = t_cur + t_delta
            call update_bc()

            vec_u_o = vec_u

            ! predictor step
            do i = di_2, nl
                do j = di_1, di_2
                    vec_u_1[i, j] = vec_u_o[i, j] - lambda * (vec_f[i+di_1, j] - vec_f[i, j]) + t_delta * vec_g[i, j]
                end do
            end do

            ! update vec_f and vec_g
            call update(vec_u_1)

            ! correct step
            do i = di_2, nl
                do j = di_1, di_2
                    vec_u_2[i, j] = vec_u_o[i, j] - lambda * (vec_f[i, j] - vec_f_1[i-di_1, j]) + t_delta * vec_g[i, j]
                end do
            end do

            do i = di_2, nl
                do j = di_1, di_2
                    vec_u[i, j] = half * (vec_u_1[i, j] + vec_u_2[i, j]) + half * (zero)
                end do
            end do

            call update(vec_u)
        end do
    end subroutine
end module solver
