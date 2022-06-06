#include "RectCS.h"

namespace odsv{
    namespace cs{
        void RectCS::Area(){
            A = half * (dz_l + dz_r) * b;
        }
    }
}
