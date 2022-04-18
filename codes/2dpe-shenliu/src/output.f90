module output
  use precision
  use config, only: result_filename => con_result_filename
  use fio, only: open_file_to_write
  implicit none

  public result_funit

  public write_result

  integer(kind=si) :: result_funit
contains
  subroutine write_result(nx, ny, x, y, f)
    implicit none
    integer(kind=di), intent(in) :: nx, ny
    real(kind=dp), dimension(:, :), intent(inout) ::x, y, f
    integer(kind=di) :: i, j
    call open_file_to_write(result_filename, result_funit)
    do i = 1, nx
      do j = 1, ny
        write(result_funit, fmt="(3(1x, F10.6))") x(i, j), y(i, j), f(i, j)
      end do
      write(result_funit, *)
    end do
    close(result_funit)
  end subroutine write_result
end module output 
