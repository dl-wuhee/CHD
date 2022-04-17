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
  use time
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
  integer(kind=di), ALLOCATABLE, DIMENSION(:,:) :: red_ij
  integer(kind=di), ALLOCATABLE, DIMENSION(:,:) :: black_ij
  integer(kind=di) :: n_red, n_black

contains
  subroutine init_solver()
    implicit none
    select case (con_imethod)
    case (di_1) ! Jacobean Iteration
      solve_method_ptr => jacob_iter 
      call initial_array(fo, nx, ny, zero)
    case (di_2) ! Guass Seidel Iteration
      solve_method_ptr => guass_iter
    case (di_3) ! Sucssor Overrelation Iteration
      solve_method_ptr => ssor_iter
    case (di_4) ! OpenMP SSOR
      solve_method_ptr => ssor_parallel_iter
      call initial_array(internal_ij, nx * ny , 2, di_0)
      call set_internal()
    case (di_5) ! Red Black GS Iteration
      solve_method_ptr => rbguass_iter
      call initial_array(red_ij, (nx+1)*(ny+1)/di_2, 2, di_0)
      call initial_array(black_ij, (nx+1)*(ny+1)/di_2, 2, di_0)
      call set_redblack()
    case (di_6) ! Red Black GS Iteration -- OpenMP
      solve_method_ptr => rbguass_openmp_iter
      call initial_array(red_ij, (nx+1)*(ny+1)/di_2, 2, di_0)
      call initial_array(black_ij, (nx+1)*(ny+1)/di_2, 2, di_0)
      call set_redblack()
    case default
      solve_method_ptr => guass_iter
    end select

    call initial_array(f, nx, ny, zero)
  end subroutine init_solver

  subroutine close_solver()
    implicit none
    call close_array(f)
    call close_array(fo)
    call close_array(internal_ij)
    call close_array(red_ij)
    call close_array(black_ij)
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

    if (con_imethod == 1) then
      fo(1:nx, 1:ny) = f(1:nx, 1:ny)
    end if
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

  subroutine set_redblack()
    implicit none
    integer(kind=di) :: i, j
    integer(kind=di) :: red_start_j, black_start_j
    n_red = di_0
    n_black = di_0

    do i = 2, nx-1
      if (modulo(i-2, 2) == 0) then
        red_start_j = di_2
        black_start_j = di_3
      else
        red_start_j = di_3
        black_start_j = di_2
      end if
      do j = red_start_j, ny-1, 2
        if (.not. patch(i, j)) then
          n_red = n_red + 1
          red_ij(n_red, 1) = i
          red_ij(n_red, 2) = j
        end if
      end do
      do j = black_start_j, ny-1, 2
        if (.not. patch(i, j)) then
          n_black = n_black + 1
          black_ij(n_black, 1) = i
          black_ij(n_black, 2) = j
        end if
      end do
    end do
  end subroutine set_redblack


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
    integer(kind=di) :: li
    li = nint(30.0_dp / h)
    f(2:li-1, ny) = f(2:li-1, ny-1)

    li = nint(45.0_dp / h)
    f(li+2:nx-1, ny) = f(li+2:nx-1, ny-1)
  end subroutine update_bc

  function cal_diff() result (maxdiff)
    implicit none
    real(kind=dp) :: maxdiff, fdiff
    integer(kind=di) :: i, j

    maxdiff = - 999.0_dp
    !$OMP parallel shared (f) private(i, j, fdiff) 
    !$omp do reduction(max: maxdiff)
    do i = 2, nx - 1
      do j = 2, ny - 1
        if (.not. patch(i, j)) then
          fdiff = f(i+1,j) + f(i-1,j) + f(i, j+1) + f(i, j-1) - 4.0_8 * f(i, j)
          maxdiff = max(maxdiff, fdiff)
        end if
      end do
    end do
    !$omp end do
    !$omp end parallel 
  end function cal_diff

  subroutine solve()
    implicit none
    integer(kind=di) :: k
    real(kind=dp) :: maxdiff
    real(kind=dp) :: wtime


    call init_solver()

    call initialize()

    call open_log()


    k = di_0 

    call timestamp("Start at ")
    wtime = omp_get_wtime()
    do
      maxdiff = cal_diff()
      call write_log(k, maxdiff)
      write(unit=*, fmt="(A, I5, A, E15.7)") "Current iteration step:", k, ", residual:", maxdiff
      if (abs(maxdiff) < con_eps) then 
        call write_result(nx, ny, x, y, f)
        exit
      end if
      k = k + 1
      if (k > 100000) then
        print *, "Failed to conveger!"
        exit
      end if


      call solve_method_ptr()

      call update_bc()
    end do
    wtime = omp_get_wtime() - wtime
    print "('Total time = ', F10.3, ' seconds')", wtime 
    call timestamp("End at ")

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
    integer(kind=di) :: k
    
    do k = 1, n_internal
      associate( i=> internal_ij(k, 1), j =>internal_ij(k, 2) )
      !print *, internal_ij(i, 1), internal_ij(i, 2)
        f(i, j) = (1.0-con_omega) * &
          f(i, j) + &
          con_omega * 0.25_8 * &
          ( &
          f(i+1, j) + &
          f(i-1, j) + &
          f(i,j+1) + &
          f(i,j-1) &
          )
      end associate
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

  subroutine rbguass_iter()
    implicit none
    integer(kind=di) :: k
    do k = 1, n_red
      associate( i=> red_ij(k, 1), j =>red_ij(k, 2) )
      !print *, internal_ij(i, 1), internal_ij(i, 2)
        f(i, j) = (1.0-con_omega) * &
          f(i, j) + &
          con_omega * 0.25_8 * &
          ( &
          f(i+1, j) + &
          f(i-1, j) + &
          f(i,j+1) + &
          f(i,j-1) &
          )
      end associate
    end do
    do k = 1, n_black
      associate( i=> black_ij(k, 1), j =>black_ij(k, 2) )
      !print *, internal_ij(i, 1), internal_ij(i, 2)
        f(i, j) = (1.0-con_omega) * &
          f(i, j) + &
          con_omega * 0.25_8 * &
          ( &
          f(i+1, j) + &
          f(i-1, j) + &
          f(i,j+1) + &
          f(i,j-1) &
          )
      end associate
    end do
  end subroutine rbguass_iter

  subroutine rbguass_openmp_iter()
    implicit none
    integer(kind=di) :: k

    !$OMP PARALLEL shared(f, red_ij, black_ij, n_red, n_black) private(k)
    !$omp do
    do k = 1, n_red
      associate( i=> red_ij(k, 1), j =>red_ij(k, 2) )
        f(i, j) = (1.0-con_omega) * &
          f(i, j) + &
          con_omega * 0.25_8 * &
          ( &
          f(i+1, j) + &
          f(i-1, j) + &
          f(i,j+1) + &
          f(i,j-1) &
          )
      end associate
    end do
    !$omp end do

    !$omp do
    do k = 1, n_black
      associate( i=> black_ij(k, 1), j =>black_ij(k, 2) )
        f(i, j) = (1.0-con_omega) * &
          f(i, j) + &
          con_omega * 0.25_8 * &
          ( &
          f(i+1, j) + &
          f(i-1, j) + &
          f(i,j+1) + &
          f(i,j-1) &
          )
      end associate
    end do
    !$omp end do
    !$OMP END PARALLEL
  end subroutine rbguass_openmp_iter

end module solver
