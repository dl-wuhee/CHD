module preissmann
    use const
    implicit none
    ! coeffs(1,1): A1i coeffs(2,1): A2i
    ! coeffs(1,2): B1i coeffs(2,2): B2i
    ! coeffs(1,3): C1i coeffs(2,3): C2i
    ! coeffs(1,4): D1i coeffs(2,4): D2i
    ! coeffs(1,5): E1i coeffs(2,5): E2i
    real(kind=8), dimension(:), allocatable :: zgf, zgg, zgh, zgi, zgj
    real(kind=8) :: dt_dx, fourdt_dx, thetadt, thetadt_dx, fourthetadt_dx, gthetadt, gdt
contains
    subroutine initial_zgarr(nx, dt, dx)
        implicit none
        real(kind=8), dimension(:), allocatable, intent(inout) :: &
            zgf, zgg, zgh, zgi, zgj
        integer(kind=4), intent(in) :: nx
        call initial_array(zgf, nx, zero)
        call initial_array(zgg, nx, zero)
        call initial_array(zgh, nx, zero)
        call initial_array(zgi, nx, zero)
        call initial_array(zgj, nx, zero)

        dt_dx = dt / dx
        fourdt_dx = four * dt_dx
        thetadt = theta * dt
        thetadt_dx = theta * dt_dx
        fourthetadt_dx = four * thetadt
        gthetadt = g * thetadt
        gdt = g * dt
    end subroutine initial_zgarr

    subroutine close_zgarr()
        implicit none
        real(kind=8), dimension(:), allocatable, intent(inout) :: &
            zgf, zgg, zgh, zgi, zgj
        call close_array(zgf)
        call close_array(zgg)
        call close_array(zgh)
        call close_array(zgi)
        call close_array(zgj)
    end subroutine close_zgarr

    subroutine upbc(ct, dzt1, dqt1, zgf1, zgg1)
        implicit none
        real(kind=8), intent(inout) :: dzt1, dqt1, zgf1, zgg1
        real(kind=8), intent(in) :: ct

        ! upstream bc is z=z(t)
        dzt1 =
        zgf1 = 1.0e6_8
        zgg1 = -zgf1 * dzt1

        ! upstream bc is q=q(t)
        dzq1 = 
        zgf1 = zero
        zgg1 = dqt1

        ! upstream bc is q=q(z)


    end subroutine upbc

    subroutine downbc(ct, dztnx, dqtnx, zgfnx, zggnx)
        implicit none
        real(kind=8), intent(inout) :: dztnx, dqtnx
        real(kind=8), intent(in) :: zgfnx, zggnx
        real(kind=8), intent(in) :: ct

        ! downstream bc is z=z(t)
        dztnx = 
        dqtnx = zgfnx * dztnx + zggnx

        ! downstream bc is q=q(t)
        dqtnx =
        dztnx = (dqtnx - zggnx) / zgfnx
        
        ! downstream bc is q=q(z)
        dqtnx = 

    end subroutine upbc

    subroutine zg(z, q, a, b, k, dkdz, dzt, dqt, nx, ct)
        implicit none
        real(kind=8), dimension(:), intent(inout) :: z, q, a, b, k, dkdz
        real(kind=8), dimension(:), intent(inout) :: dzt, dqt
        real(kind=8), intent(in) :: ct
        integer(kind=4), intent(in) :: nx

        call upbc(ct, dzt(1), dqt(1), zgf(1), zgg(1))

        do i = 1, nx-1
            call zgcoeff(z(i:i+1), q(i:i+1), a(i:i+1), b(i:i+1), k(i:i+1), &
                dkdz(i:i+1), zgh(i), zgi(i), zgj(i), zgf(i+1), zgg(i+1))
        end do

        call downbc(ct, dzt(nx), dqt(nx))
        do i = nx, 2, -1
            dzt(i-1) = zgh(i) * dqt(i) + zgi(i) * dzt(i) + zgj(i)
            dqt(i-1) = zgf(i-1) * dzt(i-1) + zgg(i-1)
            z(i-1) = z(i-1) + dzt(i-1)
            q(i-1) = q(i-1) + dqt(i-1)
        end do
    end subroutine zg

    subroutine zgcoeff(z, q, a, b, k, dkdz, zghi, zgii, zgji, zgfi1, zggi1)
        implicit none
        real(kind=8), dimension(2), intent(in) :: z, q, a, b, k, dkdz
        real(kind=8), intent(inout) :: zghi, zgii, zgji, zgfi1, zggi1
        real(kind=8), dimension(2,5) :: coeffs
        real(kind=8) :: af_plus_b_1, af_plus_b_2
        call equcoeff(z, q, a, b, k, dkdz, coeffs)
        af_plus_b_1 = coeffs(1, 1) * zgf(i) + coeffs(1, 2)
        af_plus_b_2 = coeffs(2, 1) * zgf(i) + coeffs(2, 2)
        zghi = -coeffs(1, 3) / af_plus_b_1
        zgii = -coeffs(1, 4) / af_plus_b_1
        zgji = (coeffs(1, 5) - coeffs(1, 1) * zgg(i)) / af_plus_b_1
        zgfi1 = - (af_plus_b_2 * zgi(i) + coeffs(2, 4)) &
            / (af_plus_b_2 * zgh(i) + coeffs(2, 3))
        zggi1 = (coeffs(2, 5) - af_plus_b_2 * zgi(i) - coeffs(2, 1) * zgg(i)) &
            / (af_plus_b_2 * zgh(i) + coeffs(2, 3))
    end subroutine zgcoeff

    subroutine equcoeff(z, q, a, b, k, dkdz, coeffs)
        implicit none
        real(kind=8), dimension(2,5), intent(inout) :: coeffs
        real(kind=8), dimension(2), intent(in) :: z, q, a, b, k, dkdz
        real(kind=8), intent(in) :: dt, dx
        real(kind=8) :: q_a_1, q2_a_1, q2b_a2_1, qq_k2_1, aq_k2_1, aqq_k2_1, a_k_1, &
            q_a_2, q2_a_2, q2b_a2_2, qq_k2_2, aq_k2_2, aqq_k2_2, a_k_2
        real(kind=8) :: dq, dz, dq2_a, bb_plus, aa_puls, aqq_k2_plus, fourthetadt_dx_bbplus


        q_a_1 = q(1) / a(1) ! Q_i^n/A_i^n
        q2_a_1 = q(1) * q_a_1
        q2b_a2_1 = q_a_1 * q_a_1 * b(1)
        qq_k2_1 = q(1) * abs(q(1)) / (k(1) * k(1))
        aq_k2_1 = a(1) * abs(q(1)) / (k(1) * k(1))
        aqq_k2_1 = a(1) * qq_k2_1
        a_k_1 = a(1) / k(1)

        q_a_2 = q(2) / a(2) ! Q_i^n/A_i^n
        q2_a_2 = q(2) * q_a_2
        q2b_a2_2 = q_a_2 * q_a_2 * b(2)
        qq_k2_2 = q(2) * abs(q(2)) / (k(2) * k(2))
        aq_k2_2 = a(2) * abs(q(2)) / (k(2) * k(2))
        aqq_k2_2 = a(2) * qq_k2_2
        a_k_2 = a(2) / k(2)

        dq = q(2) - q(1)
        dz = z(2) - z(1)
        dq2_a = q2_a_2 - q2_a_1
        bb_plus = b(2) + b(1)
        aa_plus = a(2) + a(1)
        aqq_k2_plus = aqq_k2_1 + aqq_k2_2
        fourthetadt_dx_bbplus = fourthetadt_dx / bbplus

        coeffs(1,1) = -fourthetadt_dx_bbplus
        coeffs(1,2) = one - fourthetadt_dx_bbplus * dq * m
        coeffs(1,3) = fourthetadt_dx_bbplus
        coeffs(1,4) = one - fourthetadt_dx_bbplus * dq * m
        coeffs(1,5) = - fourdt_dx / bb_plus * dq

        coeffs(2,1) = one - fourthetadt_dx * q_a_1 + two * gthetadt * aq_k2_1
        coeffs(2,2) = thetadt_dx * &
            (two * q2_a2_1 * b(1) - g * (aa_plus - dz * b(1)))  &
            + gthetadt * qq_k2_1 * (b(1) - 2*a(1)/k(1)*dkdz(1))
        coeffs(2,3) = one + fourthetadt_dx * q_a_2 + two * gthetadt * aq_k2_2
        coeffs(2,4) = thetadt_dx * &
            (two * q2_a2_2 * b(2) + g * (aa_plus + dz * b(2))) &
            + gthetadt * qq_k2_2 * (b(2) - two*a(2)/k(2)*dkdz(2))
        coeffs(2,5) = -two * dt_dx * dq2_a - gthetadt * aa_puls * dz - gdt * aqq_k2_plus
    end subroutine equcoeff

end module preissmann

