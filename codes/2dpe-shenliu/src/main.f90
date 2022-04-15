program main
  use mesh
  use solver
  use config, only : read_from_config
  implicit none

  call read_from_config()

  call gen_mesh()

  call solve()
  call del_mesh()

end program main
