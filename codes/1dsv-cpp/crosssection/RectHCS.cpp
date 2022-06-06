#include "RectHCS.h"

namespace odsv{
    namespace cs{
        void RectHCS::Area() {
            O = half * b * (dz_l + dz_r);
        }

        void RectHCS::cal_B() {
            B = b;
        }

        void RectHCS::cal_X() {
            X = b + consts::two * h;
        }

        void RectHCS::cal_A() {
            A = b * h;
        }

        void RectHCS::cal_n() {
            n = n;
        }

        void RectHCS::cal_h_from_A() {
            h = A / b;
        }
    }
}
