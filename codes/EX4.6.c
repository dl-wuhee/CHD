#include "stdio.h"
#include "stdlib.h"

#define N 64

int main(int argc, char** argv){
    double f[N][N];
    double f0[N][N];
    double x[N][N];
    double y[N][N];

    // Generation of grid
    double lx = 40.0;
    double ly = 40.0;
    double dx, dy;
    double ox, oy;
    dx = lx / N;
    dy = ly / N;
    ox = 0.0;
    oy = 0.0;
    
    for(int i=0; i<N; i++){
        for(int j=0; j<N; j++){
            x[i][j] = ox + j * dx;
            y[i][j] = oy + i * dy;
        }
    }

    // initialization
    for(int i=0; i<N; i++){
        for(int j=0; j<N; j++){
            f[i][j] = 0.0;
            f0[i][j] = 0.0;
        }
    }

    // Boundary condtion
    for(int i=0; i<N; i++){
        f[i][0] = 0.0;
        f[i][N-1] = 0.0;
        f[0][i] = 0.0;
        f[N-1][i] = 0.0;
        if (y[i][0] > 10.0 & y[i][0] < 30.0){
            f[i][0] = 1.0;
        }
    }

    // Iteration
    // Jacobian Iteration
    double maxdiff;
    double rel_error;
    for(;;){
        for(int i=1; i<N-1; i++){
            for(int j=1; j<N-1; j++){
                f[i][j] = 0.25 * (fo[i+1][j] + fo[i-1][j] + fo[i][j-1] + fo[i][j+1]);
            }
        }
        maxdiff = -99999.0;
        for(int i=1; i<N-1; i++){
            for(int j=1; j<N-1; j++){
                rel_error = abs((f[i][j] - fo[i][j])/f[i][j]);
                if(rel_error > maxdiff){
                    maxdiff = rel_error;
                }
            }
        }
        if(maxdiff < 1.0e-6){
            break;
        }

        for(int i=1; i<N-1; i++){
            for(int j=1; j<N-1; j++){
                fo[i][j] = f[i][j];
            }
        }
    }


    // Gauss Iteration
    for(int i=1; i<N-1; i++){
        for(int j=1; j<N-1; j++){
            f[i][j] = 0.25 * (f[i+1][j] + f[i-1][j] + f[i][j-1] + f[i][j+1]);
        }
    }

    // SOR
    double beta = 1.5;
    for(int i=1; i<N-1; i++){
        for(int j=1; j<N-1; j++){
            f[i][j] = beta * 0.25 * (f[i+1][j] + f[i-1][j] + f[i][j-1] + f[i][j+1])
                + (1.0 - beta) * fo[i][j];
        }
    }





}