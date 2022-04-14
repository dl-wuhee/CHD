module array
    implicit none
    interface initial_array
        module procedure initial_array_int4_1d, &
                         initial_array_real8_1d, &
                         initial_array_int4_2d, &
                         initial_array_real8_2d !, &
    end interface initial_array

    interface close_array
        module procedure close_array_int4_1d, &
                         close_array_real8_1d, &
                         close_array_int4_2d, &
                         close_array_real8_2d !, &
    end interface close_array

contains
    subroutine initial_array_int4_1d(x, n, x0)
        implicit none
        integer(kind=4), dimension(:), allocatable, intent(inout) :: x
        integer(kind=4), intent(in) :: x0
        integer(kind=4), intent(in) :: n
        integer :: ierr
        if (.not. allocated(x)) then
            allocate(x(1:n), stat = ierr)
            if (ierr == 0) then
                x = x0
            else
                print *, "Error: Array allocated failed!!!"
            end if
        else
            print *, "Error: Array should not be re-allocated!!!"
        end if
    end subroutine initial_array_int4_1d

    subroutine initial_array_int4_2d(x, n1, n2, x0)
        implicit none
        integer(kind=4), dimension(:,:), allocatable, intent(inout) :: x
        integer(kind=4), intent(in) :: x0
        integer(kind=4), intent(in) :: n1, n2
        integer :: ierr
        if (.not. allocated(x)) then
            allocate(x(1:n1, 1:n2), stat = ierr)
            if (ierr == 0) then
                x = x0
            else
                print *, "Error: Array allocated failed!!!"
            end if
        else
            print *, "Error: Array should not be re-allocated!!!"
        end if
    end subroutine initial_array_int4_2d

    subroutine initial_array_real8_1d(x, n, x0)
        implicit none
        real(kind=8), dimension(:), allocatable, intent(inout) :: x
        real(kind=8), intent(in) :: x0
        integer(kind=4), intent(in) :: n
        integer :: ierr
        if (.not. allocated(x)) then
            allocate(x(1:n), stat = ierr)
            if (ierr == 0) then
                x = x0
            else
                print *, "Error: Array allocated failed!!!"
            end if
        else
            print *, "Error: Array should not be re-allocated!!!"
        end if
    end subroutine initial_array_real8_1d

    subroutine initial_array_real8_2d(x, n1, n2, x0)
        implicit none
        real(kind=8), dimension(:,:), allocatable, intent(inout) :: x
        real(kind=8), intent(in) :: x0
        integer(kind=4), intent(in) :: n1, n2
        integer :: ierr
        if (.not. allocated(x)) then
            allocate(x(1:n1, 1:n2), stat = ierr)
            if (ierr == 0) then
                x = x0
            else
                print *, "Error: Array allocated failed!!!"
            end if
        else
            print *, "Error: Array should not be re-allocated!!!"
        end if
    end subroutine initial_array_real8_2d


    subroutine close_array_int4_1d(x)
        implicit none
        integer(kind=4), dimension(:), allocatable, intent(inout) :: x
        integer :: ierr
        if(allocated(x)) then
            deallocate(x, stat=ierr)
            if (ierr /= 0) then
                print *, "Error: Array deallocated failed"
            end if
        end if
    end subroutine close_array_int4_1d

    subroutine close_array_int4_2d(x)
        implicit none
        integer(kind=4), dimension(:,:), allocatable, intent(inout) :: x
        integer :: ierr
        if(allocated(x)) then
            deallocate(x, stat=ierr)
            if (ierr /= 0) then
                print *, "Error: Array deallocated failed"
            end if
        end if
    end subroutine close_array_int4_2d


    subroutine close_array_real8_1d(x)
        implicit none
        real(kind=8), dimension(:), allocatable, intent(inout) :: x
        integer :: ierr
        if(allocated(x)) then
            deallocate(x, stat=ierr)
            if (ierr /= 0) then
                print *, "Error: Array deallocated failed"
            end if
        end if
    end subroutine close_array_real8_1d

    subroutine close_array_real8_2d(x)
        implicit none
        real(kind=8), dimension(:, :), allocatable, intent(inout) :: x
        integer :: ierr
        if(allocated(x)) then
            deallocate(x, stat=ierr)
            if (ierr /= 0) then
                print *, "Error: Array deallocated failed"
            end if
        end if
    end subroutine close_array_real8_2d
end module


!program main
    !use array
    !implicit none
    !real(kind=8), dimension(:), allocatable :: xr
    !real(kind=8) :: xr0
    !integer(kind=4), dimension(:), allocatable :: xi
    !integer(kind=4) :: xi0
    !integer(kind=4) :: n
    !integer(kind=4) :: i

    !n = 5
    !xr0 = 10.0
    !xi0 = 5
    !call initial_array(xr, n, xr0)
    !call initial_array(xi, n, xi0)

    !print *, (xr(i), i=1,n)
    !print *, (xi(i), i=1,n)

    !call close_array(xr)
    !call close_array(xi)
!end program main
