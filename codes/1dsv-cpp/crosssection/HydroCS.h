#ifndef LD_ODSV_HYDRO_CS_H
#define LD_ODSV_HYDRO_CS_H

#include "HydroParas.h"
#include "CrossSection.h"
#include <iostream>
#include <vector>

namespace odsv{
    namespace cs{
        class HydroCS: public CrossSection, public HydroParas {
            protected:
                virtual void Area() = 0;
                virtual void cal_B() = 0;
                virtual void cal_A() = 0;
                virtual void cal_X() = 0;
                virtual void cal_n() = 0;
                virtual void cal_h_from_A() = 0;

            public:
                HydroCS();
                HydroCS(
                        const double &_n,
                        const bool &o_c,
                        const coordinates::Coords3D &l,
                        const coordinates::Coords3D &d,
                        const coordinates::Coords3D &r );
                HydroCS(const bool &o_c);
                virtual ~HydroCS(){};
                friend std::ostream &operator<<(std::ostream &out, const HydroCS &a);

                double getB() const {
                    return B;
                }

                double getX() const {
                    return X;
                }

                double getA() const {
                    return A;
                }

                double getR() const {
                    return R;
                }

                double getn() const {
                    return n;
                }

                double getC() const {
                    return C;
                }

                double getK() const {
                    return K;
                }

                double getQ() const {
                    return Q;
                }

                double getv() const {
                    return v;
                }

                double getc() const {
                    return c;
                }

                double getFr() const {
                    return Fr;
                }
        };
    }
}

#endif

