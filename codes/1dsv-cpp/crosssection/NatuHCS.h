#ifndef LD_ODSV_NATU_HYDRO_CS_H
#define LD_ODSV_NATU_HYDRO_CS_H

#include "HydroCS.h"
#include <vector>

namespace odsv{
    namespace cs{
        class NatuHCS : public HydroCS {
            private:
                std::vector<coordinates::Coords3D> points; 
                std::vector<coordinates::Coords3D> roughs; 


                virtual void Area() final;
                virtual void cal_B() final;
                virtual void cal_X() final;
                virtual void cal_A() final;
                virtual void cal_n() final;
                virtual void cal_h_from_A() final;

            public:
                NatuHCS(const bool &o_c,
                        const std::vector<coordinates::Coords3D> & ps
                       )
                    : 
                        HydroCS(o_c){
                            Area();
                        }

                virtual ~NatuHCS(){};
        };
    }
}

#endif


