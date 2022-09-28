#ifndef LD_ODSV_COMP_HYDRO_CS_H
#define LD_ODSV_COMP_HYDRO_CS_H

#include "HydroCS.h"
#include <vector>

namespace odsv{
    namespace cs{
        struct CompBed{
            double b_m, b_fl, b_fr;
            CompBed(const double &b1, const double &b2){
                b_m = b1;
                b_fl = b2;
                b_fr = b2;
            }

            CompBed(const double &b1, const double &b2, const double &b3){
                b_m = b1;
                b_fl = b2;
                b_fr = b3;
            }
        };

        struct CompSlope{
            double m_ml, m_mr;
            double m_fl, m_fr;
            CompSlope(const double &m) {
                m_ml = m;
                m_mr = m;
                m_fl = m;
                m_fr = m;
            }

            CompSlope(const double &m1, const double &m2){
                m_ml = m1;
                m_mr = m1;
                m_fl = m2;
                m_fr = m2;
            }

            CompSlope(const double &m1, const double &m2, const double &m3, const double &m4){
                m_ml = m1;
                m_mr = m2;
                m_fl = m3;
                m_fr = m4;
            }
        };

        struct CompRough{
            double n_mb, n_ml, n_mr;
            double n_fbl, n_fbr, n_fl, n_fr;
            CompRough(const double &n) {
                n_mb = n;
                n_ml = n;
                n_mr = n;
                n_fbl = n;
                n_fbr = n;
                n_fl = n;
                n_fr = n;
            }
            CompRough(const double &n1, const double &n2) {
                n_mb = n1;
                n_ml = n1;
                n_mr = n1;
                n_fbl = n2;
                n_fbr = n2;
                n_fl = n2;
                n_fr = n2;
            }

            CompRough(const double &n1, const double &n2, const double &n3) {
                n_mb = n1;
                n_ml = n1;
                n_mr = n1;
                n_fbl = n2;
                n_fbr = n2;
                n_fl = n3;
                n_fr = n3;
            }

            CompRough(const std::vector<double> &n) {
                n_mb = n[0];
                n_ml = n[1];
                n_mr = n[2];
                n_fbl = n[3];
                n_fbr = n[4];
                n_fl = n[5];
                n_fr = n[6];
            }
        };

        class CompHCS : public HydroCS {
            private:
                double B_m, A_m, h_m, h_f;
                CompBed b;
                CompSlope m;
                CompRough n;

                void cal_hf();
                void cal_Am();
                virtual void cal_B() final;
                virtual void cal_X() final;
                virtual void cal_A() final;
                virtual void cal_n() final;
                virtual void cal_h_from_A() final;

            public:
                CompHCS(
                        const double &_h_m,
                        const CompBed &_b, 
                        const CompSlope &_m,
                        const CompRough &_n,
                        const bool &o_c,
                        const coordinates::Coords3D &l,
                        const coordinates::Coords3D &d,
                        const coordinates::Coords3D &r)
                    : 
                        h_m(_h_m), b(_b), m(_m), n(_n),
                        HydroCS(_n.n_mb, o_c, l, d, r){
                            B_m = b.b_m + (m.m_ml + m.m_mr) * h_m; 
                            A_m = half * (b.b_m + B_m) * h_m; 
                        }

                virtual ~CompHCS(){};
        };
    }
}

#endif


