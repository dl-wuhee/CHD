#ifndef LD_ODSV_RECT_CS_H
#define LD_ODSV_RECT_CS_H

#include "../const.h"
#include "../coordinates.h"
#include "CrossSection.h"
#include <cmath>

namespace odsv{
    namespace cs{
        class RectCS : public CrossSection{
            protected:
                double b; 
                virtual void Area() final;

            public:
                RectCS();
                RectCS( const double &_b, 
                        const bool &o_c,
                        const coordinates::Coords3D &l,
                        const coordinates::Coords3D &d,
                        const coordinates::Coords3D &r):
                    b(_b), CrossSection(o_c, l, d, r){
                        Area();
                    }
        };
    }
}

#endif

