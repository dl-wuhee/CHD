module config
  use precision
  use fio
  implicit none
  real(kind=dp) :: con_h, con_omega, con_eps
  integer(kind=di) :: con_imethod
  character(len=255) :: &
    config_filename, &
    con_result_filename, &
    con_log_filename
  namelist /conf/ con_imethod, con_h, con_omega, con_eps, con_result_filename, con_log_filename
contains
  subroutine read_from_config()
    implicit none
    integer(kind=fi) :: config_funit

    read(*, *)config_filename
    call open_file_to_read(config_filename, config_funit)

    read(config_funit, nml=conf)

    close(config_funit)
  end subroutine
end module config

!program test
  !use config
  !implicit none
  !real(kind=dp) :: h, omega
  !character(len=255) :: &
    !result_filename, &
    !log_filename
  !call read_from_config()

  !h = con_h
  !omega = con_omega
  !result_filename = con_result_filename
  !log_filename = con_log_filename
  
  !print *, h
  !print *, omega
  !print *, trim(result_filename)
  !print *, trim(log_filename)
!end program test
