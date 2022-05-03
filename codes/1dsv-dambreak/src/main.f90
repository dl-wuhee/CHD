program main
    use const
    use channel
    use mesh
    use unsteady
    use solver
    use fio
    use config, only : read_from_config
    implicit none
    call read_from_config()

    call init_channel()
    call gen_mesh()
    call set_unsteady()

    call solve()

end program main
