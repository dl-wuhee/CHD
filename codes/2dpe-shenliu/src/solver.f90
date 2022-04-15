module solver
  use precision
  use const
  use geometry
  use mesh
  use array
  use fio
  use config, only : con_imethod, con_eps, con_omega
  use log
  use output
  use omp_lib
  implicit none

  abstract interface
    subroutine solve_method()
    end subroutine solve_method
  end interface

  procedure(solve_method), pointer :: solve_method_ptr => null()
  real(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: f, fo
  integer(kind=di), ALLOCATABLE, DIMENSION(:, :) :: internal_ij
  integer(kind=di) :: n_internal
contains
  subroutine init_solver()
    implicit none
    select case (con_imethod)
    case (di_1)
      solve_method_ptr => jacob_iter 
    case (di_2)
      solve_method_ptr => guass_iter
    case (di_3)
      solve_method_ptr => ssor_iter
    case (di_4)
      solve_method_ptr => ssor_parallel_iter
      call initial_array(internal_ij, nx * ny , 2, di_0)
      call set_internal()
    case default
      solve_method_ptr => guass_iter
    end select

    call initial_array(f, nx, ny, zero)
    call initial_array(fo, nx, ny, zero)
  end subroutine init_solver

  subroutine close_solver()
    implicit none
    call close_array(f)
    call close_array(fo)
    call close_array(internal_ij)
  end subroutine close_solver

  
  subroutine initialize()
    implicit none
    integer(kind=di) :: i, j

    do i = 1, nx
      do j = 1, ny
        f(i, j) = zero
      end do 
    end do

    call set_bc()

    fo = f
  end subroutine initialize

  subroutine set_internal()
    implicit none
    integer(kind=di) :: i, j
    n_internal = di_0

    do i = 2, nx-1
      do j = 2, ny-1
        if (.not. patch(i, j)) then
          n_internal = n_internal + 1
          internal_ij(n_internal, 1) = i
          internal_ij(n_internal, 2) = j
        end if
      end do
    end do
  end subroutine set_internal

  function patch(i, j) result(p)
    implicit none
    integer(kind=di), intent(in) :: i, j
    logical :: p
    p = .false.
    if (x(i, j) >= 30.0_dp .and. x(i, j) <= 45.0_dp .and. &
      y(i, j) >= 48.0_dp .and. y(i, j) <= 50.0_dp) then
      p = .true.
      return
    end if
    if (x(i, j) >= 30.0_dp .and. x(i, j) <= 33.0_dp .and. &
      y(i, j) >= 46.0_dp .and. y(i, j) <= 48.0_dp) then
      p = .true.
      return
    end if
    if (x(i, j) >= 44.0_dp .and. x(i, j) <= 45.0_dp .and. &
      y(i, j) >= 46_dp .and. y(i, j) <= 48.0_dp) then
      p = .true.
      return
    end if
    if (x(i, j) >= 31.0_dp .and. x(i, j) <= 32.0_dp .and. &
      y(i, j) >= 40.0_dp .and. y(i, j) <= 46.0_dp) then
      p = .true.
      return
    end if
  end function

  subroutine set_bc()
    implicit none
    integer(kind=di) :: li
    integer(kind=di) :: i, j

    do i = 1, nx
      do j = 1, ny
        if (patch(i, j)) then
          f(i, j) = zero
        end if
      end do
    end do

    do j = 1, ny
      f(1, j) = 15.0_dp
      f(nx, j) = 15.0_dp
    end do

    do i = 1, nx
      f(i, 1) = 15.0_dp
    end do
    
    li = nint(30.0_dp / h)
    do i = 2, li - 1
        f(i, ny) = 15.0_dp / li * (li - i)
    end do
    li = nint(45.0_dp / h)
    do i = li + 2, nx - 1
        f(i, ny) = 15.0_dp / (nx - li) * (i - li - 1)
    end do
  end subroutine set_bc

  subroutine update_bc()
    implicit none
    integer(kind=di) :: i
    integer(kind=di) :: li
    li = nint(30.0_dp / h)
    do i = 2, li - 1
        f(i, ny) = f(i, ny-1)
    end do
    li = nint(45.0_dp / h)
    do i = li + 2, nx - 1
      f(i, ny) = f(i, ny-1)
    end do

  end subroutine update_bc

  function cal_diff() result (maxdiff)
    implicit none
    real(kind=dp) :: maxdiff, fdiff
    integer(kind=di) :: i, j

    maxdiff = - 999.0_dp
    do i = 2, nx - 1
      do j = 2, ny - 1
        if (.not. patch(i, j)) then
          fdiff = f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1) - 4.0_8 * f(i, j)
          if (abs(fdiff) > maxdiff) then
            maxdiff = abs(fdiff)
          end if
        end if
      end do
    end do
  end function cal_diff

  subroutine solve()
    implicit none
    integer(kind=di) :: k
    real(kind=dp) :: maxdiff


    call init_solver()

    call initialize()

    call open_log()


    k = di_0 
    do
      maxdiff = cal_diff()
      call write_log(k, maxdiff)
      print *, k, maxdiff
      if (abs(maxdiff) < con_eps) then 
        call write_result(nx, ny, x, y, f)
        exit
      end if
      k = k + 1
      if (k > 500000) then
        print *, "Failed to conveger!"
        exit
      end if


      call solve_method_ptr()

      call update_bc()
    end do

    call close_log()

    call close_solver()
  end subroutine solve

  subroutine jacob_iter()
    implicit none
    integer(kind=di) :: i, j
    do i = 2, nx-1
      do j = 2, ny-1
        if (.not. patch(i, j)) then
          f(i, j) = 0.25_8 * (fo(i+1,j) + fo(i-1,j) + fo(i, j+1) + fo(i, j-1))
        end if
      end do
    end do
    fo = f
  end subroutine jacob_iter

  subroutine guass_iter()
    implicit none
    integer(kind=di) :: i, j
    do i = 2, nx-1
      do j = 2, ny-1
        if (.not. patch(i, j)) then
          f(i, j) = 0.25_8 * (f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1))
        end if
      end do
    end do
  end subroutine guass_iter

  subroutine ssor_iter()
    implicit none
    integer(kind=di) :: i, j
    do i = 2, nx-1
      do j = 2, ny-1
        if (.not. patch(i, j)) then
          f(i, j) = (1.0-con_omega) * f(i, j) + con_omega * 0.25_8 * (f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1))
        end if
      end do
    end do
  end subroutine ssor_iter

  subroutine ssor_parallel_iter()
    implicit none
    integer(kind=di) :: i

    
    do i = 1, n_internal
      !print *, internal_ij(i, 1), internal_ij(i, 2)
        f(internal_ij(i, 1), internal_ij(i, 2)) = (1.0-con_omega) * &
          f(internal_ij(i, 1), internal_ij(i, 2)) + &
          con_omega * 0.25_8 * &
          ( &
          f(internal_ij(i, 1)+1,internal_ij(i, 2)) + &
          f(internal_ij(i, 1)-1,internal_ij(i, 2)) + &
          f(internal_ij(i, 1),internal_ij(i, 2)+1) + &
          f(internal_ij(i, 1),internal_ij(i, 2)-1) &
          )
    end do
    !OMP PARALLEL WORKSHARE
    !forall (i = 1:n_internal, .true.)
        !f(internal_ij(i, 1), internal_ij(i, 2)) = (1.0-con_omega) * &
          !f(internal_ij(i, 1), internal_ij(i, 2)) + &
          !con_omega * 0.25_8 * &
          !( &
          !f(internal_ij(i, 1)+1,internal_ij(i, 2)) + &
          !f(internal_ij(i, 1)-1,internal_ij(i, 2)) + &
          !f(internal_ij(i, 1),internal_ij(i, 2)+1) + &
          !f(internal_ij(i, 1),internal_ij(i, 2)-1) &
          !)
    !end forall
    !OMP END PARALLEL WORKSHARE
  end subroutine ssor_parallel_iter


end module solver
