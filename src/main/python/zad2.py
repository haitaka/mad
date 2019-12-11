import numpy as np
from numpy import linalg as la
from math import *
from matrix import *
import random

N1 = 20
N2 = 20
M = 6
delta = 10**(-2)


def x(i):
    # val right = (1 to n1) map (_.toDouble / n1)
    # val left = (1 to n2) map (-_.toDouble / n2)
    # (left ++ Seq(0D) ++ right).sorted
    right = [float(i) / N1 for i in range(1, N1 + 1)]
    left = [-float(i) / N2 for i in range(1, N2 + 1)]
    return sorted(left + [0.0] + right)[i]


def f(x):
    return cos(pi * x)


def mat_fun(i, j):
    return x(i) ** j


A = FunMatrix(N2 + N1 + 1, M + 1, mat_fun)

b = [f(x(i)) + random.uniform(-1, 1) * delta for i in range(A.i_size)]
npb = np.array(b)

npA = A.forNP()

p0, r_norm, rank, sing = la.lstsq(npA, npb, None)

r = npb - npA.dot(p0)

print("x:", p0)
print()
print("r:", r)
print(max([abs(v) for v in r]))
print("rank:", rank, npA.shape)


def p(i):
    return sum([p0[k] * x(i)**k for k in range(0, M + 1)])


diff = max([abs(f(x(i)) - p(i)) for i in range(A.j_size)])

print("diff:", diff)

#print("r_norm:", sqrt(r_norm))
#print("r_norm:", (r_norm))

print("A.cond:", la.cond(npA))

