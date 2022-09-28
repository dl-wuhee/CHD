#include "CompHCS.h"
#include <cmath>

namespace odsv{
    namespace cs{
        void CompHCS::cal_hf() {
            if (h < h_m) {
                h_f = zero;
            } else {
                h_f = h - h_m;
            }
        }

        void CompHCS::cal_B() {
            cal_hf();
            if (h > h_m) {
                B = B_m + (m.m_fl + m.m_fr) * h_f;
            } else {
                B =  b.b_m + (m.m_ml + m.m_mr) * h; 
            }
        }

        void CompHCS::cal_X() {
            if (h > h_m) { 
                X = b.b_m + 
                    (pow(one + m.m_ml * m.m_ml, half) + pow(one + m.m_mr * m.m_mr, half))  * h_m +
                    (pow(one + m.m_fl * m.m_fl, half) + pow(one + m.m_fr * m.m_fr, half))  * h_f ;
            }else {
                X = b.b_m + 
                    (pow(one + m.m_ml * m.m_ml, half) + pow(one + m.m_mr * m.m_mr, half))  * h;
            }
        }

        void CompHCS::cal_A() {
            if ( h > h_m ) {
                A = A_m + half * (B_m + b.b_fl + b.b_fr + B) * h_f;
            } else {
                A = half * (b.b_m + B) * h;  
            }
        }

        void CompHCS::cal_n() {
            double X_b, X_l, X_r, alpha;
            double n_b, n_l, n_r;
            if (h > h_m) {
                X_b = b.b_m + (pow(one + m.m_ml, half) + pow(one + m.m_mr, half)) * h_m;;
                X_l = b.b_fl + pow(one + m.m_fl, half) * h_f;
                X_r = b.b_fr + pow(one + m.m_fr, half) * h_f;
                alpha = one;
                n_b = std::pow( (
                            b.b_m * n.n_mb + 
                            pow(one + m.m_ml, half) * h_m * n.n_ml +
                            pow(one + m.m_ml, half) * h_m * n.n_ml 
                            ) / X_b, one / alpha );
                n_l = std::pow( (
                            b.b_fl * n.n_fbl + 
                            pow(one + m.m_fl, half) * h_f * n.n_fl 
                            ) / X_l, one / alpha );
                n_l = std::pow( (
                            b.b_fr * n.n_fbr + 
                            pow(one + m.m_fr, half) * h_f * n.n_fr 
                            ) / X_r, one / alpha );
                n = std::pow( (
                            X_b * std::pow(n_b, alpha) +
                            X_l * std::pow(n_l, alpha) +
                            X_r * std::pow(n_r, alpha) 
                            ) / X, one / alpha);
            } else {
                n = std::pow( (
                            b.b_m * n.n_mb + 
                            pow(one + m.m_ml, half) * h * n.n_ml +
                            pow(one + m.m_ml, half) * h * n.n_ml 
                            ) / X, one / alpha );
            }
        }

        void CompHCS::cal_h_from_A() {
            double m2;
            if (std::abs(A - A_m) < consts::eps) {
                h = h_m;
            } else {
                if (A < A_m) {
                    m2 = m.m_ml + m.m_mr;
                    h = pow(two * A / m2 + pow(b.b_m / m2, two), half) - b.b_m / m2;
                } else {
                    m2 = m.m_fl + m.m_fr;
                    h = h_m + 
                        pow(
                                two * (A - A_m) / m2 + 
                                pow((b.b_fl + B_m + b.b_fr) / m2, two),
                                half) -
                        (b.b_fl + B_m + b.b_fr) / m2;
                }
            }
        }
    }
}
