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

    call solve()

    call del_time()
    call del_mesh()

end program main
