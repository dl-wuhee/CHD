import numpy as np

def GenCoeffs(l, t, dt, dx, alpha):
    nx = int(l / dx) + 1
    nt = int(t / dt) + 1
    A = 0.5 * alpha * dt / (dx * dx)
    B = 1 + 2.0 * A
    CoeffList = []
    CoeffArr = np.zeros((nx-2, nx-2))
    for i in range(nx-2):
        for j in range(nx-2):
            if (i == j-1 or i == j+1):
                CoeffArr[i, j] = A
            elif (i==j):
                CoeffArr[i, j] = -B
    return CoeffArr, A, nx, nt

def GenBC(T, nx, nt):
    T[:,0] = 0.0
    T[:,nx-1] = 50.0

def CalK(Told, T1, Tnx, nx, A):
    K = np.zeros(nx-2)
    for i in range(nx-2):
        # Notice that the index of K array should be one less than that of Told array
        # Told: 0 1 2 3 4 5
        # K:      0 1 2 3
        K[i] = -1 * Told[i+1] - A * (Told[i+2] - 2*Told[i+1] + Told[i])
    K[0] = K[0] - A * T1
    K[nx-3] = K[nx-3] - A * Tnx
    return K

if __name__ == "__main__":
    l = 5.0
    t = 10.0
    dx = 1.0
    dt = 2.5
    alpha = 0.5
    CoeffArr, A, nx, nt= GenCoeffs(l, t, dt, dx, alpha)
    T = np.zeros((nt,nx))
    GenBC(T, nx, nt)
    for i in range(1, nt):
        K = CalK(T[i-1,:], T[i,0], T[i,nx-1], nx, A)
        T[i,1:nx-1] = np.linalg.inv(CoeffArr).dot(K)
        # for pt in [1.0 * k for k in range(1,11)]:
        for pt in [2.5 * k for k in range(1,5)]:
            if abs(i * dt - pt) < 1.0e-6:
                print(T[i,:])


