import numpy as np
from numpy import linalg as la
from matrix import *

N = 30

b = 1./4.

def or_fun(i, j):
    if i == j + 1:
        return -b
    elif i == j - 1:
        return b
    return 0

def m_fun(i, j):
    if (i == N - 1 and j == N) or (j == N - 1 and i == N):
        return 0

    sign = 1 if i < N else -1

    if i == j + 1:
        if i % 2 == 0:
            return sign * b
        else:
            return sign * -b
    elif i == j - 1:
        if j % 2 == 0:
            return sign * b
        else:
            return sign * -b
    return 0

def t_fun(i, j):
    m = [[2, 0, 0], [0, 3, 4], [0, 4, 9]]
    return m[i][j]

m = FunMatrix(N * 2, N * 2, m_fun)
#m = FunMatrix(N, or_fun)
#m = FunMatrix(3, t_fun)

npm = m.forNP()

#print(npm)
#print()

norm = np.linalg.norm(npm, ord=2)

def minus_o(a, b):
    assert a - b != 0
    return a - b

def sturm(lambd):
    def p(j):
        if j == 0:
            return abs(m.get(0, 1)) / minus_o(0, lambd)
        elif j == m.j_size - 1:
            return 1. / (minus_o(minus_o(0, lambd), (m.get(m.j_size - 2, m.j_size - 1) * p(j - 1))))
        else:
            return abs(m.get(j, j+1)) / (minus_o(minus_o(0, lambd), (m.get(j-1, j) * p(j - 1))))

    c = 0
    for i in range(m.i_size):
        if p(i) < 0:
            c += 1
    return c

eigvals, eigvects = la.eig(npm)

eigvects = np.transpose(eigvects)

max_evs = -1
for evec1 in eigvects:
    for evec2 in eigvects:
        if all(evec1 != evec2):
            #print('#', evec1, evec2, evec1.dot(evec2))
            max_evs = max(max_evs, abs(evec1.dot(evec2)))

print('max scalar evec', max_evs)

print(max([max(npm.dot(evec) - (evec * eval)) for eval, evec in zip(eigvals, eigvects)]))

V = np.transpose(eigvects)
M = V @ npm @ la.inv(V)
M = la.inv(V) @ npm @ V
max_nd = None
for i in range(M.shape[0]):
    for j in range(M.shape[1]):
        if i != j:
            if max_nd is None:
                max_nd = M[i][j]
            else:
                max_nd = max(max_nd, M[i][j])
print('max_nd', max_nd)