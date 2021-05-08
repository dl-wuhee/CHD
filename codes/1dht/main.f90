program main
    use array
    use implicit_method
    use explicit_method
    use residual
    implicit none
    real(kind=8), parameter :: alpha = 0.5_8, l = 5.0_8, t_total = 4.0_8
    real(kind=8) :: coeff, dx, dt, ct
    real(kind=8), dimension(:), allocatable :: t, to, x
    real(kind=8), dimension(:), allocatable :: a, b, c, d
    integer(kind=4) :: nx, nt
    integer(kind=4) :: i, j
    character(len=250) :: forstr
    integer(kind=4) :: imethod

    ! imethod: 1, 2, 3, 4 
    ! 1: cranknicolson
    ! 2: implicit compact pade
    ! 3: forward time and central space
    ! 4: third order runge-kutta 
    read(*, *) dx, imethod

    select case (imethod)
    case (1, 2)
        coeff = 2.0_8
    case (3, 4)
        coeff = 0.2_8
    end select
    nx = nint(l / dx) + 1
    if (nx < 3) then
        print *, "Error! dx is too large"
        stop
    end if
    dx = l / (nx-1)
    dt = coeff / alpha * dx * dx
    nt = nint(t_total / dt)
    dt = t_total / nt

    coeff = alpha * dt / (dx * dx)
    !print *, "l", l, "nx=", nx, "nt=", nt, "t_total=", t_total
    print *, "dx=", dx, "dt=", dt, "coeff=", coeff
    select case (imethod)
    case (1)
        print *, "Solver method: Crank Nicolson"
    case (2)
        print *, "Solver method: Implicit Compact Pade"
    case (3)
        print *, "Solver method: Forward time and Central space"
    case (4)
        print *, "Solver method: Third order Runge Kutta"
    case default
        print *, "Solver method: Error"
    end select

    ! initialization
    call initial_array(x ,nx, 0.0_8)
    call initial_array(t ,nx, 0.0_8)
    call initial_array(to ,nx, 0.0_8)

    do i = 1, nx
        x(i) = (i-1)*dx
        t(i) = 0.0_8
    end do
    t(1) = 0.0_8
    t(nx) = 50.0_8
    to = t

    !print *, (x(i), i = 1, nx)
    ! begin time loop
    select case (imethod)
    case (1)
        call initial_array(a ,nx-2, 0.0_8)
        call initial_array(b ,nx-2, 0.0_8)
        call initial_array(c ,nx-2, 0.0_8)
        call initial_array(d ,nx-2, 0.0_8)
        do j = 1, nt
            ct = j * dt
            call cranknicolson(coeff, nx, t, a, b, c, d)
            if (checkresidual(t, to, nx)) then
                exit
            end if
            to = t
        end do
    case (2)
        call initial_array(a ,nx-2, 0.0_8)
        call initial_array(b ,nx-2, 0.0_8)
        call initial_array(c ,nx-2, 0.0_8)
        call initial_array(d ,nx-2, 0.0_8)
        do j = 1, nt
            ct = j * dt
            call implicitcompactpade(coeff, nx, t, a, b, c, d)
            if (checkresidual(t, to, nx)) then
                exit
            end if
            to = t
        end do
    case (3)
        do j = 1, nt
            ct = j * dt
            call ftcs(coeff, nx, t)
            if (checkresidual(t, to, nx)) then
                exit
            end if
            to = t
        end do
    case (4)
        do j = 1, nt
            ct = j * dt
            call rungekutta3(coeff, nx, t)
            if (checkresidual(t, to, nx)) then
                exit
            end if
            to = t
        end do
    case default
        print *, "Error: Solver method is not available!!!"
    end select

    forstr = ""
    !write(forstr, "(A1, I5, A6)")"(", nx+1, "F10.5)"
    !write(*, forstr)ct, (t(i), i = 1, nx)
    write(*, "(2I4, 4F10.5)")nx, nx/2, ct, t(1), t(nx/2), t(nx)

    call close_array(t)
    call close_array(to)
    call close_array(x)
    call close_array(a)
    call close_array(b)
    call close_array(c)
    call close_array(d)
end program main
