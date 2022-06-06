
#include "TrapHCS.h"
#include <cmath>

namespace odsv{
    namespace cs{
        void TrapHCS::Area() {
            O = half * (b + b + (m.m_l+m.m_r) * half * (dz_l + dz_r)) * half * (dz_l + dz_r);
        }

        void TrapHCS::cal_B() {
            B = b + (m.m_l + m.m_r) * h;
        }

        void TrapHCS::cal_X() {
            X = b + (pow(one + m.m_l * m.m_l, half) + pow(one + m.m_r * m.m_r, half))  * h;
        }

        void TrapHCS::cal_A() {
            A = half * (b + B) * h;
        }

        void TrapHCS::cal_n() {
            double X_b, X_l, X_r, alpha;
            X_b = b;
            X_l = b + m.m_l * h;
            X_r = b + m.m_r * h;
            alpha = one;
            n = std::pow( (
                    X_b * std::pow(n.n_b, alpha) +
                    X_l * std::pow(n.n_l, alpha) +
                    X_r * std::pow(n.n_r, alpha) 
                    ) / X, one / alpha);
        }

        void TrapHCS::cal_h_from_A() {
            double m2 = m.m_l + m.m_r;
            h = pow(two * A / m2 + pow(b / m2, two), half) - b / m2;
        }
    }
}
