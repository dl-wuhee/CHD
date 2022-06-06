#ifndef LD_ODSV_RECT_HYDRO_CS_H
#define LD_ODSV_RECT_HYDRO_CS_H

#include "HydroCS.h"

namespace odsv{
    namespace cs{
        class RectHCS : public HydroCS {
            private:
                double b;
                virtual void Area() final;
                virtual void cal_B() final;
                virtual void cal_X() final;
                virtual void cal_A() final;
                virtual void cal_n() final;
                virtual void cal_h_from_A() final;

            public:
                RectHCS(const double &_b, 
                        const double &n,
                        const bool &o_c,
                        const coordinates::Coords3D &l,
                        const coordinates::Coords3D &d,
                        const coordinates::Coords3D &r)
                    : b(_b), HydroCS(n, o_c, l, d, r){
                        Area();
                    }
                virtual ~RectHCS(){};
        };
    }
}

#endif

