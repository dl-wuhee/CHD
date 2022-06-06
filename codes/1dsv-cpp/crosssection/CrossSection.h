#ifndef LD_ODSV_CROSSSECTION_H
#define LD_ODSV_CROSSSECTION_H

#include "../const.h"
#include "../coordinates.h"
#include <cmath>
#include <string>

namespace odsv{
    namespace cs{
        class CrossSection{
            private:
                void cal_distance();
                void cal_dz();

            protected:
                std::string name;
                unsigned int id;
                coordinates::Coords3D
                    coord_l,
                    coord_d,
                    coord_r;
                double 
                    l_t,
                    l_l,
                    l_r,
                    O,
                    dz_l,
                    dz_d,
                    dz_r;

                bool open_closed;

                virtual void Area()=0;

            public:
                CrossSection();
                CrossSection(const bool &o_c);
                CrossSection(
                        const bool &o_c,
                        const coordinates::Coords3D &l,
                        const coordinates::Coords3D &d,
                        const coordinates::Coords3D &r );
                virtual ~CrossSection(){};
        };
    }
}


#endif
