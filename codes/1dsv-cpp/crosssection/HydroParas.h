#ifndef LD_ODSV_HYDRO_PARAS_H
#define LD_ODSV_HYDRO_PARAS_H

#include "../const.h"

namespace odsv{
    namespace cs{
        class HydroParas {
            private:
                void cal_zs_from_h();
                void cal_h();
                void cal_R();
                void cal_C();
                void cal_K();
                void cal_v();
                void cal_c();
                void cal_Q();
                void cal_Fr();
            protected:
                double 
                    zs,
                    zb,
                    h;
                double
                    A,
                    B,
                    X,
                    R;
                double
                    n,
                    C,
                    K;
                double
                    Q,
                    q,
                    v,
                    c,
                    Fr;

                virtual void cal_B() = 0;
                virtual void cal_A() = 0;
                virtual void cal_X() = 0;
                virtual void cal_n() = 0;
                virtual void cal_h_from_A() = 0;
            public:
                HydroParas();
                HydroParas(const double &n, const double &_zb);
                virtual ~HydroParas(){};
                void setzQ(const double &_z, const double &_Q);
                void setzv(const double &_z, const double &_v);
                void sethQ(const double &_h, const double &_Q);
                void sethv(const double &_h, const double &_v);
                void setAQ(const double &_A, const double &_Q);

                void setz(const double &_z);
                void seth(const double &_h);
                void setA(const double &_A);
                void setQ(const double &_Q);
                void setv(const double &_v);

        };
    }
}

#endif
