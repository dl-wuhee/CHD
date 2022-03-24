program main
    use const
    use channel
    use mesh
    use time
    use solver
    implicit none

    call gen_channel()
    call gen_mesh()
    call set_time()

    call init_solver()
    call initialize()
    call solve()

    call close_solver()
    call del_mesh()

end program main
