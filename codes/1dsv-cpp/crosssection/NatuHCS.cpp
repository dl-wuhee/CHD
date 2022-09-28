#include "NatuHCS.h"

namespace odsv{
    namespace cs{
        NatuHCS::NatuHCS(
                const bool &o_c, 
                const std::vector<coordinates::Coords3D> & ps) :
            HydroCS(o_c) {
                points = ps;
        }

    }
}
