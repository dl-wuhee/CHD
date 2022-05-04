module output
  use precision
  use const
  use config, only: result_filename => con_result_filename
  use fio, only: open_file_to_write
  implicit none

  public result_funit

  public output_result, open_result, close_result

  integer(kind=si) :: result_funit
contains
  subroutine open_result()
    implicit none
    call open_file_to_write(result_filename, result_funit)
  end subroutine open_result 

  subroutine output_result(x, h, v, nl, t_cur)
    implicit none
    real(kind=dp), DIMENSION(*), intent(in) :: x, h, v
    real(kind=dp), intent(in) :: t_cur
    integer(kind=di), intent(in) :: nl
    integer(kind=di) :: i

    write(result_funit, "(A70)") "#"//repeat("=", 69)
    write(result_funit, "('#', A3, ',', 3(A10, ','), A10)") &
      "i", "t", "x", "h", "v"
    do i = di_1, nl
      write(result_funit, "(I6, ',', 3(F10.3, ','), F10.3)") &
        i, t_cur, x(i), h(i), v(i)
    end do
    write(result_funit, "(A70)") "#"//repeat("=", 69)
    write(result_funit, *)
    write(result_funit, *)
  end subroutine output_result

  subroutine close_result()
    implicit none
    close(result_funit)
  end subroutine close_result
end module output 
