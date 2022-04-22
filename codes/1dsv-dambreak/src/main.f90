program main
    use const
    use channel
    use mesh
    use time
    use solver
    use fio
    implicit none
    call set_files()
    call gen_channel()
    call gen_mesh()
    call set_time()

    call solve()

    call del_time()
    call del_mesh()
    call close_files()

end program main
