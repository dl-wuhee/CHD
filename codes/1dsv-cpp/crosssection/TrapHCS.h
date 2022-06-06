#ifndef LD_ODSV_TRAP_HYDRO_CS_H
#define LD_ODSV_TRAP_HYDRO_CS_H

#include "HydroCS.h"

namespace odsv{
    namespace cs{
        struct TrapSlope{
            double m_l, m_r;
            TrapSlope(const double &m) {
                m_l = m;
                m_r = m;
            }

            TrapSlope(const double &m1, const double &m2){
                m_l = m1;
                m_r =m2;
            }
        };

        struct TrapRough{
            double n_b, n_l, n_r;
            TrapRough(const double &n) {
                n_b = n;
                n_l = n;
                n_r = n;
            }

            TrapRough(const double &n1, const double &n2, const double &n3) {
                n_b = n1;
                n_l = n2;
                n_r = n3;
            }
        };

        class TrapHCS : public HydroCS {
            private:
                double b;
                TrapSlope m;
                TrapRough n;

                virtual void Area() final;
                virtual void cal_B() final;
                virtual void cal_X() final;
                virtual void cal_A() final;
                virtual void cal_n() final;
                virtual void cal_h_from_A() final;

            public:
                TrapHCS(const double &_b, 
                        const TrapSlope &_m,
                        const TrapRough &_n,
                        const bool &o_c,
                        const coordinates::Coords3D &l,
                        const coordinates::Coords3D &d,
                        const coordinates::Coords3D &r)
                    : 
                        b(_b), m(_m), n(_n),
                        HydroCS(_n.n_b, o_c, l, d, r){
                        Area();
                    }

                virtual ~TrapHCS(){};
        };
    }
}

#endif


