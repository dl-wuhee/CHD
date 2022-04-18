module fio
  use precision
  use const
  implicit none
  private get_unit

  public open_file_to_read, open_file_to_write
contains
  subroutine get_unit ( iunit )
    !*****************************************************************************80
    !
    !! GET_UNIT returns a free FORTRAN unit number.
    !
    !  Discussion:
    !
    !    A "free" FORTRAN unit number is a value between 1 and 99 which
    !    is not currently associated with an I/O device.  A free FORTRAN unit
    !    number is needed in order to open a file with the OPEN command.
    !
    !    If IUNIT = 0, then no free FORTRAN unit could be found, although
    !    all 99 units were checked (except for units 5, 6 and 9, which
    !    are commonly reserved for console I/O).
    !
    !    Otherwise, IUNIT is a value between 1 and 99, representing a
    !    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
    !    are special, and will never return those values.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    28 March   2022
    !    26 October 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, integer IUNIT, the free unit number.
    !
    implicit none
    integer(kind=si), intent(inout) :: iunit
    integer(kind=si) :: i
    integer(kind=si) :: ios
    logical(kind=si) :: lopen

    iunit = si_0
    do i = si_1, si_9 * (si_10 + si_1)
      if ( i /= si_5 .and. i /= si_6 .and. i /= si_9 ) then
        inquire ( unit = i, opened = lopen, iostat = ios )
        if ( ios == si_0 ) then
          if ( .not. lopen ) then
            iunit = i
            return
          end if
        end if
      end if
    end do
    return
    end

    subroutine open_file_to_read(filename, unit_f)
      implicit none
      character(len=*), intent(in) :: filename 
      integer(kind=si), intent(inout) :: unit_f
      integer(kind=si) :: f_status
      call get_unit(unit_f)
      open(unit=unit_f, file=filename, &
        status = "old", action="read", iostat = f_status)
      if (f_status /= 0) then
        write ( *, "(a)" ) " "
        write ( *, "(a)" ) "open_file_to_read - Fatal error!"
        write ( *, "(a,i8)" ) "  Could not open the file '" // &
          trim ( filename ) // "' to read on unit ", unit_f 
        unit_f = -1
        stop
      end if
    end subroutine open_file_to_read

    subroutine open_file_to_write(filename, unit_f)
      implicit none
      character(len=*), intent(in) :: filename 
      integer(kind=si), intent(inout) :: unit_f
      integer(kind=si) :: f_status
      call get_unit(unit_f)
      open(unit=unit_f, file=filename, &
        status = "replace", action="write", iostat = f_status)
      if (f_status /= 0) then
        write ( *, "(a)" ) " "
        write ( *, "(a)" ) "open_file_to_write - Fatal error!"
        write ( *, "(a,i8)" ) "  Could not open the output file '" // &
          trim ( filename ) // "' on unit ", unit_f 
        unit_f = -1
        stop
      end if
    end subroutine open_file_to_write
  end module fio
