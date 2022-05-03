module log
  use precision
  use config, only: log_filename => con_log_filename
  use fio, only: open_file_to_write
  use time
  implicit none

  public log_funit

  public open_log, write_log, close_log

  character(len=255) :: log_info
  integer(kind=si) :: log_funit

contains
  subroutine open_log()
    implicit none
    call open_file_to_write(log_filename, log_funit)
  end subroutine open_log

  subroutine write_log(logs, log_timestamp)
    implicit none
    CHARACTER(len=*), intent(in) :: logs
    logical, intent(in), optional :: log_timestamp
    character(len=21) :: cur_timestamp
    if (present(log_timestamp)) then
      cur_timestamp = timestamp()
      write(unit=log_funit, fmt="(A, ':', 1x, A21)")trim(logs), cur_timestamp
    else
      write(unit=log_funit, fmt="(4x, A)")trim(logs)
    end if
    !write(unit=log_funit, fmt="(2(1x, A, F10.6))") "Current t=", t, "dt=", dt
  end subroutine write_log

  subroutine close_log()
    implicit none
    close(log_funit)
  end subroutine close_log
end module log
