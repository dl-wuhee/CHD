#include "./crosssection/HydroCS.h"
#include "./crosssection/RectHCS.h"
#include "./crosssection/TrapHCS.h"
#include "./crosssection/CompHCS.h"

#include "coordinates.h"

#include <iostream>

using namespace std;
using namespace odsv::consts;
using namespace odsv::coordinates;
using namespace odsv::cs;

int main(int argc, char **argv){
    Coords3D l(zero, zero, five);
    Coords3D r(zero, one, five);
    Coords3D d(zero, half, five);
    bool oc = false;
    HydroCS *rcs = new RectHCS(two, 0.01, false, l, d, r);
    rcs->sethQ(one, ten);
    cout << *rcs << endl;
    delete rcs;
    rcs = NULL;

    HydroCS *rcs1 = new TrapHCS(
            two, 
            TrapSlope(one), 
            TrapRough(0.01), 
            false, l, d, r);
    rcs1->setAQ(three, ten);
    cout << *rcs1 << endl;
    delete rcs1;
    rcs1 = NULL;

    HydroCS *rcs2 = new TrapHCS(
            two, 
            TrapSlope(one, two), 
            TrapRough(0.01, 0.015, 0.018), 
            false, l, d, r);
    rcs2->sethQ(one, ten);
    cout << *rcs2 << endl;
    delete rcs2;
    rcs2 = NULL;

    HydroCS *rcs3 = new CompHCS(
            two,
            CompBed(two, one), 
            CompSlope(one, two),
            CompRough(0.01, 0.015, 0.018),
            false, l, d, r);
    rcs3->sethQ(one, ten);
    cout << *rcs3 << endl;
    delete rcs3;
    rcs3 = NULL;

    HydroCS *rcs4 = new CompHCS(
            two,
            CompBed(two, one), 
            CompSlope(one, two),
            CompRough(0.01, 0.015, 0.018),
            false, l, d, r);
    rcs4->setAQ(three, ten);
    cout << *rcs4 << endl;
    rcs4->setAQ(eight, ten);
    cout << *rcs4 << endl;
    rcs4->setAQ(ten, two*ten);
    cout << *rcs4 << endl;
    rcs4->setA(500.0);
    cout << *rcs4 << endl;
    rcs4->setQ(1000.0);
    cout << *rcs4 << endl;
    delete rcs4;
    rcs3 = NULL;
}
