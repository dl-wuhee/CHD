import numpy as np

def GenCoeffs(l, t, dt, dx, alpha):
    nx = int(l / dx) + 1
    nt = int(t / dt) + 1
    A = 0.5 * alpha * dt / (dx * dx)
    B = 1 + 2.0 * A
    CoeffList = []
    for i in range(nx):
        curlist = []
        for j in range(nx):
            if (i < j-1):
                curlist.append(0)
            elif (i == j-1):
                curlist.append(A)
            elif (i==j):
                curlist.append(B)
            elif (i==j+1):
                curlist.append(A)
            elif (i>j+1):
                curlist.append(0)
        CoeffList.append(curlist)
    CoeffArr = np.array(CoeffList)
    return CoeffArr, A, nx, nt

def GenBC(T, nx):
    T[0] = 0.0
    T[nx-1] = 50.0

def CalK(Told, nx, A):
    K = np.zeros(nx)
    for i in range(nx):
        K[i] = -1 * Told[i] - A * (Told[i+1] - 2*Told[i] + Told[i-1])
    K[0] = K[0] - A * T[0]
    K[nx-1] = K[nx-1] - A * T[nx-1]
    return K



def GenK(Told, nx)
if __name__ == "__main__":
    l = 5.0
    t = 10.0
    dx = 1.0
    dt = 1.0
    alpha = 0.5
    CoeffArr, A, nx, nt= GenCoeffs(l, t, dt, dx, alpha)
    Told = np.zeros(nx)
    GenBC(Told, nx)
    T = Told
    for i in range(nt):
        K = CalK(Told, nx, A)
        T = np.linalg.inv(CoeffArr).dot(K)


