module time
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
    real(kind=dp), allocatable, dimension(:) :: arr_output_t
    integer(kind=dp), allocatable, dimension(:) :: arr_output_ns
    integer(kind=di) :: output_cur
    real(kind=dp) :: output_ts
    integer(kind=di) :: output_ti
contains
    subroutine set_time()
        implicit none
        t_total = 50.0_dp
        t_cur = zero
        t_delta = one
        t_delta_specified = t_delta
        nt = di_0
        nt_cur = di_0
        output_policy = si_1
        output_cur = di_1
        dt_policy = si_2
    end subroutine

    function calc_t_delta(u, dx) result (dt)
        implicit none
        real(kind=dp), dimension(:), intent(in) :: u
        real(kind=dp) :: dx
        real(kind=dp) :: dt
        real(kind=dp) :: output_t_cur
        select case (dt_policy)
        case (si_1)
            dt = t_delta_specified
        case (si_2)
            dt = adpative_dt(u, dx)
        case default
            print *, "time stepping policy error"
            print *, "time stepping policy is set to 1"
            dt = t_delta_specified
        end select
        select case (output_policy)
        case (si_1)
            output_cur_t = arr_output_t[output_cur]
        case (si_2)
            output_cur_t = t_delta_specified * arr_output_ns[output_cur]
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
            output_cur = output_cur + 1
        end if
        if ( t_cur < t_total .and. (t_cur + dt) > t_total ) then
            dt = t_total - t_cur
        end if
    end function calc_t_delta

end module time
