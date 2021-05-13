program main
    use array
    use preissmann
    implicit none
    real(kind=8), dimension(:), allocatable :: x, zb, z, h, q, a, b, p, k, dzt, dqt
    real(kind=8) :: l, dx, t_total, dt, ct
    integer(kind=4) :: nt, nx, i, j
    character(len=250) :: forstr, infile, outfile

    dx = 500.0_8
    nx = nint(l / dx) + 1
    dt = 60.0_8
    nt = nint(t_total / dt)

    call initial_array(x, nx, zero)
    call initial_array(zb, nx, zero)
    call initial_array(z, nx, zero)
    call initial_array(h, nx, zero)
    call initial_array(q, nx, zero)
    call initial_array(a, nx, zero)
    call initial_array(b, nx, zero)
    call initial_array(k, nx, zero)
    call initial_array(dzt, nx, zero)
    call initial_array(dqt, nx, zero)

    ! initial
    do i = 1, nx
        x(i) = (i+1) * dx
        zb(i) = zb0 + dx * s
        z(i) = 15.518_8
        h(i) = z(i) - zb(i)
        q(i) = zero
        a(i) = (b + m * h(i)) * h(i)
        b(i) = bb + 2 * m * h(i)
        p(i) = b + two * h(i) * (one + m * m) ** half 
        k(i) = a(i) ** (5.0_8/3.0_8) / (n * p(i)**(2.0_8/3.0_8))
    end do

    call initial_zgarr(nx, dt, dx)
    do j = 1, nt
        ct = j * dt
        call zg(z, q, a, b, k, dkdz, dzt, dqt, nx, ct)
        do i = 1, nx
            h(i) = z(i) - zb(i)
            a(i) = (b + m * h(i)) * h(i)
            b(i) = bb + 2 * m * h(i)
            p(i) = b + two * h(i) * (one + m * m) ** half 
            k(i) = a(i) ** (5.0_8/3.0_8) / (n * p(i)**(2.0_8/3.0_8))
        end do
    end do
    call close_zgarr()


    call close_array(x)
    call close_array(zb)
    call close_array(z)
    call close_array(h)
    call close_array(q)
    call close_array(a)
    call close_array(b)
    call close_array(k)
    call close_array(dzt)
    call close_array(dqt)


end program main
