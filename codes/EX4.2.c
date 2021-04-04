#include "stdio.h"

int main(int argc, char** argv){
    double a[4];
    double b[4];
    double c[4];
    double d[4];
    double y[4];
    double l[4];
    double u[4];
    double x[4];

    double A, B;
    A = 0.25;
    B = 1.5;
    double K[4];

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
    dt = 1.0;
    t = 0.0;

    for(int n=0; ;n++){
        t += dt;
        if (t > 50.0){
            break;
        } else{
            for(int j=0; j<4; j++){
                a[j] = A;
                b[j] = -1 * B;
                c[j] = A;
                d[j] = -1 * To[j+1] - A * (To[j+2] - 2 * To[j+1] + To[j]);
            }
            d[0] -= A * T[0];
            d[3] -= A * T[5];

/*             for(int j=0; j<4; j++){
                a[j] = 1.0;
                b[j] = 2.0;
                c[j] = 1.0;
                d[j] = 1.0;
            }
            d[0] = 3;
            d[1] = 4;
            d[2] = 4;
            d[3] = 3; */
            u[0] = b[0];
            l[0] = 0.0;
            for(int j=1; j<4; j++){
                l[j] = a[j] / u[j-1];
                u[j] = b[j] - l[j] * c[j-1];
            }

            // LY = D
            y[0] = d[0];
            for(int j=1; j<4; j++){
                y[j] = d[j] - l[j] * y[j-1];
            }
            // UX = Y
            x[3] = y[3] / u[3];
            for(int j=2; j>=0; j--){
            // for(int j=2; j>0; j--){
                x[j] = (y[j] - c[j] * x[j+1]) / u[j];
            }

            for(int i=1; i<5; i++){
                T[i] = x[i-1];
                To[i] = T[i];
            }
            // for(int i=1; i<5; i++){
            //     T[i] = To[i] + c * (To[i+1] - 2*To[i] + To[i-1]);
            // }
            printf("%11.6f", t);
            for(int i=0; i<6; i++){
                printf("%11.6f", T[i]);
                To[i] = T[i];
            }
            printf("\n");
            // for(int i=0; i<4; i++){
            //     printf("%11.6f", l[i]);
            //     // To[i] = T[i];
            // }
            // printf("\n");
            // for(int i=0; i<4; i++){
            //     printf("%11.6f", u[i]);
            //     // To[i] = T[i];
            // }
            // printf("\n");
            // break;
        }
    }
}