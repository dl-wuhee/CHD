module unsteady 
    use precision
    use const
    use array
    use config, only : con_dt
    implicit none
    ! t_total: the flow will be computed to t_total
    ! t_cur: time of current compute
    real(kind=dp) :: t_total, t_cur
    ! nt: number of time step
    integer(kind=di) :: nt, nt_cur

    ! dt_policy: dt calculation policy
    !   1: constant dt 
    !   2: adaptive dt
    integer(kind=si) :: dt_policy
    ! t_delta: time step
    ! t_delta_specified: specified time step at initial.
    real(kind=dp) :: t_delta, t_delta_specified


    ! output_policy: out policy, can equal to 
    !   1: output at set times, need to specify arr_output_t
    !   2: output at specified steps, need to specify arr_output_ns
    !   3: output according to specified time step, need to specify output_ts
    !   4: output according to specified step, need to specify output_ti
    integer(kind=si) :: output_policy
    real(kind=dp) :: output_cur_t
    real(kind=dp), allocatable, dimension(:) :: arr_output_t
    integer(kind=di), allocatable, dimension(:) :: arr_output_ns
    integer(kind=di) :: output_cur
    real(kind=dp) :: output_ts
    integer(kind=di) :: output_ti
contains
    subroutine set_unsteady()
        implicit none
        t_total = ten
        t_cur = zero
        t_delta = con_dt
        t_delta_specified = t_delta

        dt_policy = si_2
        select case (dt_policy)
        case (si_1)
            nt = floor(t_total / t_delta) + di_1
        case (si_2)
            nt = 9999999_di
        case default
            nt = floor(t_total / t_delta) + di_1
        end select

        nt_cur = di_0

        output_policy = si_3
        select case (output_policy)
        case (si_1)
            call initial_array(arr_output_t, di_2, zero)
            arr_output_t = (/one, five, ten/)
        case (si_2)
            call initial_array(arr_output_ns, di_2, di_0)
            arr_output_ns = (/di_5, di_10/)
        case (si_3)
            output_ts = one / ten 
        case (si_4)
            output_ti = di_10
        case default
            output_ti = di_10
        end select

        output_cur = di_1
    end subroutine set_unsteady

    function adpative_dt(u, h, dx, cfl) result (dt)
        implicit none
        real(kind=dp), dimension(:), intent(in) :: u, h
        real(kind=dp), intent(in) :: dx, cfl
        real(kind=dp) :: dt, max_speed, speed
        integer(kind=di) :: i, nl
        max_speed = -999.0_dp
        nl = size(u)
        do i = di_1, nl
          if (h(i) < zero .or. isnan(h(i))) then
            dt = -999.0_dp 
            exit
          end if
          speed = abs(u(i) + sqrt(g * h(i)))
          if (speed > max_speed) then
            max_speed = speed 
          end if
          speed = abs(u(i) - sqrt(g * h(i)))
          if (speed > max_speed) then
            max_speed = speed 
          end if
        end do
        dt = cfl * dx / max_speed
    end function adpative_dt

    function calc_t_delta(u, h, dx, cfl) result (dt)
        implicit none
        real(kind=dp), dimension(:), intent(in) :: u, h
        real(kind=dp), intent(in) :: dx, cfl
        real(kind=dp) :: dt
        select case (dt_policy)
        case (si_1)
            dt = t_delta_specified
        case (si_2)
            dt = adpative_dt(u, h, dx, cfl)
        case default
            print *, "time stepping policy error"
            print *, "time stepping policy is set to 1"
            dt = t_delta_specified
        end select
        select case (output_policy)
        case (si_1)
          if (output_cur > size(arr_output_t)) then
            output_cur_t = zero
          else
            output_cur_t = arr_output_t(output_cur)
          end if
        case (si_2)
            output_cur_t = t_delta_specified * arr_output_ns(output_cur)
        case (si_3)
            output_cur_t = output_cur * output_ts
        case (si_4)
            output_cur_t = output_cur * (t_delta_specified * output_ti)
        case default
            print *, "output policy error"
            print *, "output policy is set to default value"
            output_cur_t = output_cur * (t_delta_specified * output_ti)
        end select
        if ( t_cur < output_cur_t .and. (t_cur + dt) > output_cur_t ) then
            dt = output_cur_t - t_cur
        end if
        if ( t_cur < t_total .and. (t_cur + dt) > t_total ) then
            dt = t_total - t_cur
        end if
    end function calc_t_delta

    subroutine del_unsteady()
        implicit none
        call close_array(arr_output_ns)
        call close_array(arr_output_t)
    end subroutine del_unsteady
end module unsteady
