module time
  use precision
  implicit none
  public timestamp
contains
  subroutine timestamp(descr_str)
    implicit none
    character(len=*), intent(in), optional :: descr_str
    character(len=8) :: date
    character(len=10) :: time
    character(len=5) :: zone
    integer(kind=di), dimension(1:8) :: date_time
    integer(kind=di) year, month, day, hour, minute, second, millisecond

    call date_and_time(date, time, zone, date_time)
    year = date_time(1)
    month = date_time(2)
    day = date_time(3)
    hour = date_time(5)
    minute = date_time(6)
    second = date_time(7)
    millisecond = date_time(8)

    if (present(descr_str)) then
      write(*, "(A, 1x, i4, '-', i2.2, '-', i2.2, ' ', i2.2, ':', i2.2, ':', i2.2, '.', i3.3)") &
        trim(descr_str), year, month, day, hour, minute, second, millisecond
    else
      write(*, "(1x, i4, '-', i2.2, '-', i2.2, ' ', i2.2, ':', i2.2, ':', i2.2, '.', i3.3)") &
        year, month, day, hour, minute, second, millisecond
    end if

  end subroutine timestamp
end module
