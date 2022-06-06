#include "HydroParas.h"
#include <cmath>

namespace odsv{
    namespace cs{
        HydroParas::HydroParas() {
            zs = consts::zero;
            zb = consts::zero;
            h = consts::zero;
            A = consts::zero;
            B = consts::zero;
            X = consts::zero;
            R = consts::zero;
            n = consts::zero;
            C = consts::zero;
            K = consts::zero;
            Q = consts::zero;
            q = consts::zero;
            v = consts::zero;
            c = consts::zero;
        }

        HydroParas::HydroParas(const double &_n, const double &_zb) {
            zs = consts::zero;
            zb = _zb;
            h = consts::zero;
            A = consts::zero;
            B = consts::zero;
            X = consts::zero;
            R = consts::zero;
            n = _n;
            C = consts::zero;
            K = consts::zero;
            Q = consts::zero;
            q = consts::zero;
            v = consts::zero;
            c = consts::zero;
        }

        void HydroParas::cal_zs_from_h() {
            zs = h + zb;
        }

        void HydroParas::cal_h() {
            h = zs - zb;
        }

        void HydroParas::cal_R() {
            R = A / X;
        }

        void HydroParas::cal_C() {
            // manning formula
            C = consts::one / n * std::pow(R, consts::one_sixth);
        }

        void HydroParas::cal_K() {
            K = A * C * std::pow(R, consts::half);
        }

        void HydroParas::cal_v() {
            v = Q / A;
        }

        void HydroParas::cal_c() {
            c = std::pow(consts::g * h, consts::half);
        }

        void HydroParas::cal_Q() {
            Q = v * A;
        }

        void HydroParas::cal_Fr() {
            Fr = v / c;
        }


        void HydroParas::setAQ(const double &_A, const double &_Q){
            setA(_A);
            setQ(_Q);
        }

        void HydroParas::setzQ(const double &_z, const double &_Q){
            setz(_z);
            setQ(_Q);
        }

        void HydroParas::setzv(const double &_z, const double &_v){
            setz(_z);
            setv(_v);
        }

        void HydroParas::sethQ(const double &_h, const double &_Q){
            seth(_h);
            setQ(_Q);
        }

        void HydroParas::sethv(const double &_h, const double &_v){
            seth(_h);
            setv(_v);
        }

        void HydroParas::setz(const double &_z) {
            zs = _z;
            cal_h();
            cal_B();
            cal_A();
            cal_X();
            cal_R();
            cal_C();
            cal_K();
            cal_v();
            cal_c();
            cal_Fr();
        }

        void HydroParas::seth(const double &_h) {
            h = _h;
            cal_zs_from_h();
            cal_B();
            cal_A();
            cal_X();
            cal_R();
            cal_C();
            cal_K();
            cal_v();
            cal_c();
            cal_Fr();
        }

        void HydroParas::setA(const double &_A) {
            A = _A;
            cal_h_from_A();
            cal_zs_from_h();
            cal_B();
            cal_X();
            cal_R();
            cal_C();
            cal_K();
            cal_v();
            cal_c();
            cal_Fr();
        }

        void HydroParas::setQ(const double &_Q) {
            Q = _Q;
            cal_v();
            cal_c();
            cal_Fr();
        }

        void HydroParas::setv(const double &_v) {
            v = _v;
            cal_Q();
            cal_c();
            cal_Fr();
        }

    }
}
