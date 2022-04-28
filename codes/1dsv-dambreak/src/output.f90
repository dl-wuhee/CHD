module output
  use precision
  use config, only: result_filename => con_result_filename
  use fio, only: open_file_to_write
  implicit none

  public result_funit

  public write_result

  integer(kind=si) :: result_funit
contains
  subroutine output(x, h, v, nl, t_cur, output_funit)
    implicit none
    real(kind=dp), DIMENSION(*), intent(in) :: x, h, v
    real(kind=dp), intent(in) :: t_cur
    integer(kind=di), intent(in) :: nl
    integer(kind=fi), intent(in) :: output_funit 

    integer(kind=di) :: i

    write(unit_f, "(A70)") "#"//repeat("=", 69)
    write(unit_f, "('#', A3, ',', 3(A10, ','), A10)") &
      "i", "t", "x", "h", "v"
    do i = di_1, nl
      write(unit_f, "(I4, ',', 3(F10.3, ','), F10.3)") &
        i, t_cur, x(i), h(i), v(i)
    end do
    write(unit_f, "(A70)") "#"//repeat("=", 69)
    write(unit_f, *)
    write(unit_f, *)
  end subroutine write_result
end module output 
