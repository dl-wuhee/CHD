program main
   use array
   use linear_solver
   implicit none
   real(kind=8), parameter :: coeff = 0.2, alpha = 0.5, l = 7.0, t_total = 120.0
   real(kind=8)::dx, dt, ct
   real(kind=8),dimension(:),allocatable :: a, b, c, d, t, x
   integer(kind=4) :: nx, nt
   integer(kind=4) :: i, j
   
   read(*,*) dx
   
   nx = nint(l / dx) + 1
   if(nx<3) then
       print*,"Error! dx is too large"
       stop
   end if
   dx = l / (nx-1)
   dt = coeff / alpha * dx * dx
   nt = nint(t_total / dt)
   dt = t_total / nt
   !print *, "dx="
   !print *, dx
   !print *, "dt="
   !print *, dt
   
   call initial_array(x ,nx, 0.0_8)
   call initial_array(t ,nx, 0.0_8)
   call initial_array(a ,nx-2, 0.0_8)
   call initial_array(b ,nx-2, 0.0_8)
   call initial_array(c ,nx-2, 0.0_8)
   call initial_array(d ,nx-2, 0.0_8)
   
   do i = 1, nx
       x(i) = (i-1)*dx
       t(i) = 0.0
   end do
   t(1) = 0.0
   t(nx) = 50.0

   !print*,(x(i),i=1,nx)
   do j = 1, nt
       ct = j * dt
       do i=1, nx-2
           a(i) = 0.5*coeff
           b(i) = -1-coeff
           c(i) = a(i)
           d(i)= -t(i+1) - 0.5 * coeff * (t(i+2) - 2 * t(i+1) + t(i))
       end do
       d(1) = d(1) - a(1) * t(1)
       d(nx-2) = d(nx-2) - a(nx-2) * t(nx)
       
       call tdma(a, b, c, d, t(2:nx-1), nx-2)
       write(*,"(9f12.5)")ct, (t(i), i=1, nx)
   end do
   
   call close_array(a)
   call close_array(b)
   call close_array(c)
   call close_array(d)
   call close_array(t)
   call close_array(x)
end program main
