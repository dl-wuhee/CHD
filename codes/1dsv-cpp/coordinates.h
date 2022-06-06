#ifndef LD_1DSV_COORDINATES_H
#define LD_1DSV_COORDINATES_H

#include "const.h"

using namespace odsv::consts;

namespace odsv{
    namespace coordinates{
        class Coords2D{
            private:
                double x, y;
            public:
                Coords2D(): 
                    x(zero), 
                    y(zero) {
                    };
                Coords2D(const double _x, 
                        const double _y):
                    x(_x),
                    y(_y){
                    };
                ~Coords2D(){};
        };
        class Coords3D{
            private:
                double x, y, z;
            public:
                Coords3D():
                    x(zero), 
                    y(zero), 
                    z(zero){
                    };
                Coords3D(const double _x,
                        const double _y,
                        const double _z):
                    x(_x),
                    y(_y),
                    z(_z){
                    };
                ~Coords3D(){};
                double X() const;
                double Y() const;
                double Z() const;
        };
    }
}
#endif
