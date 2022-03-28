module fio
  use const
  implicit none
  character(len=255) :: output_filename, log_filename
  integer(kind=fi) :: output_funit, log_funit
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
    !    John Burkardt, Dan Li
    !
    !  Parameters:
    !
    !    Output, integer IUNIT, the free unit number.
    !
    implicit none
    integer(kind=fi), intent(inout) :: iunit
    integer(kind=fi) :: i
    integer(kind=fi) :: ios
    logical(kind=fi) :: lopen

    iunit = 0
    do i = 1, 99
      if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then
        inquire ( unit = i, opened = lopen, iostat = ios )
        if ( ios == 0 ) then
          if ( .not. lopen ) then
            iunit = i
            return
          end if
        end if
      end if
    end do
    return
    end

    subroutine open_file_to_write(filename, unit_f)
      implicit none
      character(len=*), intent(in) :: filename 
      integer(kind=fi), intent(inout) :: unit_f
      integer(kind=fi) :: f_status
      open(unit=unit_f, file=filename, &
        status = "replace", iostat = f_status)
      if (f_status /= 0) then
        write ( *, "(a)" ) " "
        write ( *, "(a)" ) "open_file_to_write - Fatal error!"
        write ( *, "(a,i8)" ) "  Could not open the output file '" // &
          trim ( filename ) // "' on unit ", unit_f 
        unit_f = -1
        stop
      end if
    end subroutine open_file_to_write

    subroutine close_files()
      implicit none
      close(unit = output_funit)
      close(unit = log_funit)
    end subroutine close_files

    subroutine set_files()
      implicit none
      output_filename = "result.dat"
      log_filename = "run.log"
      call get_unit(output_funit)
      call open_file_to_write(output_filename, output_funit)
      call get_unit(log_funit)
      call open_file_to_write(log_filename, log_funit)
    end subroutine set_files

    subroutine output(x, h, v, a, q, nl, t_cur, unit_f)
      implicit none
      real(kind=dp), DIMENSION(*), intent(in) :: x, h, v, a, q
      real(kind=dp), intent(in) :: t_cur
      integer(kind=di), intent(in) :: nl
      integer(kind=fi), intent(in) :: unit_f
      integer(kind=di) :: i
      write(unit_f, "(A70)") "#"//repeat("+", 69)
      write(unit_f, "('#', A3, ',', 5(A10, ','), A10)") &
        "i", "t", "x", "h", "v", "area", "discharge" 
      do i = di_1, nl+di_1
        write(unit_f, "(I4, ',', 5(F10.3, ','), F10.3)") &
          i, t_cur, x(i), h(i), v(i), a(i), q(i)
      end do
      write(unit_f, "(A70)") "#"//repeat("-", 69)
      write(unit_f, *)
      write(unit_f, *)
    end subroutine output
  end module fio
