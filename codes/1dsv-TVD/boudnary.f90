module boundary
    use const
    use solver
    use time
    implicit none
    real(kind=dp) :: h_u, h_d
contains
    subroutine update_bc()
        implicit none
        h[1] = ten
        h[nl+one] = half
    end subroutine update_bc
end module boundary
