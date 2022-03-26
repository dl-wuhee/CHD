module boundary
    use const
    use time
    use channel
    implicit none
contains
    subroutine set_bc(a_u, a_d, q_u, q_d)
        implicit none
        real(kind=dp), intent(inout) :: a_u, a_d, q_u, q_d
        a_u = ten * b
        a_d = half * b
        q_u = zero
        q_d = zero
    end subroutine set_bc 
end module boundary
