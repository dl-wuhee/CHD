#include "HydroCS.h"
#include <iomanip>

namespace odsv{
    namespace cs{
        HydroCS::HydroCS()
            : CrossSection(), HydroParas() {

            }

        HydroCS::HydroCS(const bool &o_c)
            : CrossSection(o_c), HydroParas() {

            }

        HydroCS::HydroCS(
                const double &_n,
                const bool &o_c,
                const coordinates::Coords3D &l,
                const coordinates::Coords3D &d,
                const coordinates::Coords3D &r )
            : CrossSection(o_c, l, d, r), HydroParas(_n, d.Z()) {

        } 

        std::ostream &operator<<(std::ostream &out, const HydroCS &a) {
            int pres = 6;
            int total_label = 10;
            int total_width_of_scientific = 14;
            out << std::setprecision(6);
            //out << std::setiosflags(std::ios::fixed);
            out << std::setiosflags(std::ios::scientific);

            out << std::setw(total_label) <<  "h(m): " 
                << std::setw(total_width_of_scientific) << a.h
                << std::endl;
            out << std::setw(total_label) <<  "B(m): " 
                << std::setw(total_width_of_scientific)  << a.B
                << std::endl;
            out << std::setw(total_label) <<  "X(m): "
                << std::setw(total_width_of_scientific)  << a.X
                << std::endl;
            out << std::setw(total_label) <<  "A(m2): "
                << std::setw(total_width_of_scientific)  << a.A
                << std::endl;
            out << std::setw(total_label) <<  "R(m): "
                << std::setw(total_width_of_scientific)  << a.R
                << std::endl;
            out << std::setw(total_label) <<  "n: " 
                << std::setw(total_width_of_scientific)  << a.n
                << std::endl;
            out << std::setw(total_label) <<  "C: " 
                << std::setw(total_width_of_scientific)  << a.C
                << std::endl;
            out << std::setw(total_label) <<  "K(m3/s): " 
                << std::setw(total_width_of_scientific)  << a.K
                << std::endl;
            out << std::setw(total_label) <<  "Q(m3/s): " 
                << std::setw(total_width_of_scientific)  << a.Q
                << std::endl;
            out << std::setw(total_label) <<  "v(m/s): " 
                << std::setw(total_width_of_scientific)  << a.v
                << std::endl;
            out << std::setw(total_label) <<  "c(m/s): " 
                << std::setw(total_width_of_scientific)  << a.c
                << std::endl;
            out << std::setw(total_label) <<  "Fr: " 
                << std::setw(total_width_of_scientific)  << a.Fr
                << std::endl;
            return out;
        }
    }
}
