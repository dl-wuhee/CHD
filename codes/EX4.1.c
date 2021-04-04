#include "stdio.h"

int main(int argc, char** argv){
    double c;
    c = 0.2;

    double T[6];
    double To[6];

    // Initialization
    for(int i=0; i<6; i++){
        T[i] = 0.0;
    }
    T[0] = 50.0;
    T[5] = 0.0;

    for(int i=0; i<6; i++){
        To[i] = T[i];
    }

    double dt, t;
    dt = 0.4;
    t = 0.0;

    for(int n=0; ;n++){
        t += dt;
        if (t > 50.0){
            break;
        } else{
            for(int i=1; i<5; i++){
                T[i] = To[i] + c * (To[i+1] - 2*To[i] + To[i-1]);
            }
            printf("%11.6f", t);
            for(int i=0; i<6; i++){
                printf("%11.6f", T[i]);
                To[i] = T[i];
            }
            printf("\n");
        }
    }
}