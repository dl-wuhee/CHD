#include "CrossSection.h"

namespace odsv{
    namespace cs{
        CrossSection::CrossSection(){
        }

        CrossSection::CrossSection(const bool &o_c){
            open_closed = o_c;
        }

        CrossSection::CrossSection(
                const bool &o_c,
                const coordinates::Coords3D &l,
                const coordinates::Coords3D &d,
                const coordinates::Coords3D &r ){
            open_closed = o_c;
            coord_l = l;
            coord_d = d;
            coord_r = r;

            cal_distance();
            cal_dz();
        }

        void CrossSection::cal_distance(){
            double dx, dy, dz;
            // total distance
            dx = coord_l.X() - coord_r.X();
            dy = coord_l.Y() - coord_r.Y();
            l_t = pow(dx*dx + dy*dy, half);

            // left distance
            dx = coord_l.X() - coord_d.X();
            dy = coord_l.Y() - coord_d.Y();
            l_l = pow(dx*dx + dy*dy, half);

            // right distance
            dx = coord_r.X() - coord_d.X();
            dy = coord_r.Y() - coord_d.Y();
            l_r = pow(dx*dx + dy*dy, half);
        }

        void CrossSection::cal_dz(){
            dz_l = coord_l.Z() - coord_d.Z();
            dz_r = coord_r.Z() - coord_d.Z();
            dz_d = dz_l + l_l * (dz_r - dz_l) / l_t;
        }

    }
}
