program main
    use array
    implicit none
    real(kind=8), parameter :: pi = 3.14159265857_8
    real(kind=8), parameter :: lx = 2_8 * pi, dx = 0.05_8 * pi, c = 1.0_8, &
        t_total = 10.0_8, coeff = 2.0_8 / pi
    real(kind=8), dimension(:), allocatable :: u, x
    real(kind=8) :: ct, dt
    integer(kind=4) :: nx, nt
    integer(kind=4) :: i, j
    character(len=255) :: forstr, outfile
    outfile = "result1.txt"

    dt = coeff * dx / c
    nx = nint(lx / dx) + 1
    !print *, lx, dx, nx
    nt = nint(t_total / dt)
    !print *, t_total, dt, nt

    forstr = ""
    write(forstr, "(A1, I5, A9)")"(", nx+1, "(F10.5,))"
    call initial_array(u, nx, 0.0_8)
    call initial_array(x, nx, 0.0_8)
    ! Initial condition
    do i = 1, nx
        x(i) = (i - 1) * dx
        u(i) = sin(x(i))
    end do
    ct = 0.0_8
    open(unit=10, file=outfile, action="write", status="unknown")
    write(*, forstr)ct, (x(i), i=1,nx)
    write(*, forstr)ct, (u(i), i=1,nx)

    ! begin time loop
    do j = 1, nt
        ct = j * dt

        !! begin FTFS
        !do i = 1, nx-1
            !u(i) = u(i) - coeff * (u(i+1) - u(i))
        !end do
        !u(nx) = u(nx) - coeff * (u(2) - u(nx))
        !! end FTFS

        !! begin FTRS
        !do i = 2, nx
            !u(i) = u(i) - coeff * (u(i) - u(i-1))
        !end do
        !u(1) = u(1) - coeff * (u(1) - u(nx-1))
        !! end FTRS

        ! begin FTCS
        do i = 2, nx-1
            u(i) = u(i) - 0.5_8 * coeff * (u(i+1) - u(i-1))
        end do
        u(1) = u(1) - 0.5_8 * coeff * (u(2) - u(nx-1))
        u(nx) = u(nx) - 0.5_8 * coeff * (u(2) - u(nx-1))
        ! end FTCS


        if ( abs(ct - 0.5_8) < 1.0e-6 .or. &
             abs(ct - 1.0_8) < 1.0e-6 .or. &
             abs(ct - 2.0_8) < 1.0e-6 .or. &
             abs(ct - t_total) < 1.0e-6) then
             write(*, forstr)ct, (u(i), i=1,nx)
         end if
    end do
    ! end time loop

    call close_array(x)
    call close_array(u)

end program main
