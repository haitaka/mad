import numpy as np
from numpy import linalg as la
from math import log10, log
from matrix import FunMatrix


def conj(A):
    return np.transpose(np.conj(A))


def norm(A):
    return la.norm(A, 2)


def mat_dich(A, B):
    answer = {}
    answer["P"] = None

    # Программа дихотомии единичной окружностью матричного пучка [A,B]
    #[A,B] = matrix(N) заданный матричный пучок
    # answer["trace"] - след проектора P на приводящее подпространство, соответствующее 
    # собственным значениям, лежащим внутри единичной окружности
    # answer["log10norm"]= log10(||H||)
    # answer["error"] = ||P ** 2 - P||/||P|| - погрешность вычисления проектора
    # answer["iters_count"] - число понадобившихся итераций
    
    # ********** КОНСТАНТЫ ************************
    n, nn = A.shape
    om = 40  # максимально допустимый порядок ||H||
    e = 10 ** (-9)  # максимально допустимая погрешность вычисления проектора
    s_ = 10 ** 20  # максимально допустимое число обусловленности обращаемых матриц
    
    # ***********ПРОВЕРКА ВЫПОЛНИМОСТИ АЛГОРИТМА****
    c = la.cond(A + B)
    c1 = la.cond(A - B)
    if max(c, c1) > s_:  # проверка обратимости
        omega = om
        tr = -1
        print('Дихотомия невозможна1')
        answer["trace"] = float(tr)
        answer["log10norm"] = float(omega)
        answer["error"] = 1
        answer["iters_count"] = 0
        return answer

    A1 = la.inv(A + B)
    A2 = la.inv(A - B)
    # ********** ПОДГОТОВИТЕЛЬНЫЙ ЭТАП ************
    I_ = np.eye(n)
    Z = np.zeros((n, n))
    C = I_
    #************ОРТОГОНАЛЬНЫЕ ИСКЛЮЧЕНИЯ*******
    k_ = 0
    HH = I_
    S = (A1 - A2) / 2
    T = (A1 + A2) / 2
    H1 = (S @ C @ conj(S)) + (T @ C @ conj(T))
    H = (A @ H1 @ conj(A)) + (B  @ H1 @ conj(B))

    epsilon = norm(H - HH)
    omega = log10(norm(H))
    # pause
    P = A
    while (epsilon > e * norm(H)) or (norm((P @ P) - P) > e * norm(P)):  # сходимость H  и P
        # ЗАМЕЧАНИЕ. Сходимость проекторов можно исключить из условия    
        k_ += 1  # номер итерации
        if omega >= om:  # проверка того, что норма H не превысила максимально допустимого значения
            omega = om
            print('Дихотомия невозможна2')
            tr = -1
            answer["trace"] = float(tr)
            answer["log10norm"] = float(omega)
            answer["error"] = 1
            answer["iters_count"] = k_
            return answer
        if k_ > 1000:  # ограничение на число итераций
            omega = om
            print('Дихотомия невозможна2')
            tr = -1
            answer["trace"] = float(tr)
            answer["log10norm"] = float(omega)
            answer["error"] = 1
            answer["iters_count"] = k_
            return answer

        F = np.block([[-B, A, Z], [A, Z, -B]])
        Q, R = la.qr(F)
        
        # НОВЫЕ МАТРИЦЫ
        B = -R[n: 2*n, 2*n: 3*n]
        A = R[n: 2*n, n: 2*n]
        #   B=-R(n+1:2*n, 2*n+1:3*n);
        #   A=R(n+1:2*n, n+1:2*n);
        
        # ВЫЧИСЛЕНИЕ МАТРИЦЫ H
        HH = H
        C = la.inv(A + B)
        A1 = C @ A
        B1 = C @ B
        H = (A1 @ H @ conj(A1)) + (B1 @ H @ conj(B1))
        epsilon = norm(H - HH)
        omega = log10(norm(H))
        P = np.matmul((-la.inv(A - B)), B)
    # ответ
    tr = np.trace(P)
    answer["trace"] = float(tr)
    answer["P"] = P
    answer["H.norm"] = norm(H)
    answer["log10norm"] = float(omega)
    answer["error"] = norm(P ** 2 - P)
    answer["iters_count"] = k_
    return answer


# q = 5
q = 1
Aq_diag = [6, 5, 4, 3, 2, 1./2., 1./3., 1./4., 1./5.]
Aq_shape = (len(Aq_diag), len(Aq_diag))


def Aq_fun(i, j):
    assert i in range(Aq_shape[0])
    assert j in range(Aq_shape[1])
    if i == j:
        return Aq_diag[i]
    elif j == i + 1:
        if i == 5:
            return 0
        else:
            return q
    else:
        return 0


Aq = FunMatrix(Aq_shape[0], Aq_shape[1], Aq_fun).forNP()


def sign(x):
    if x < 0:
        return -1
    elif x == 0:
        return 0
    elif a > 0:
        return 1


def AB(a, b):
    s = Aq.shape[0]
    A = np.block([[(a - b) / 2. * np.eye(s), np.zeros((s, s))], [sign(a - b) * Aq, (a - b) / 2. * np.eye(s)]])
    B = np.block([[(a + b) / 2. * np.eye(s), Aq], [np.zeros((s, s)), (a + b) / 2. * np.eye(s)]])
    return A, B


def Pquo(a, b):
    A, B = AB(a, b)
    result = mat_dich(A, B)

    P = result['P']

    K = (conj(Aq) @ Aq) + np.eye(Aq.shape[0])
    Kwave = (conj(A) @ A) + (conj(B) @ B)

    w_min = result['H.norm'] / norm(la.inv(K)) / norm(Kwave)
    w_max = result['H.norm'] * norm(K) * norm(la.inv(Kwave))

    Hwave_norm = (w_max + w_min) / 2.
    print('Hwave_norm', b, Hwave_norm)
    print('P error', b, result['error'])

    return P[0:9, 0:9]


a = norm(Aq)

P1 = Pquo(a, 0)
P2 = Pquo(a, 0.5)
Pdiff = P2 - P1

u, s, vh = la.svd(Pdiff)
r = la.inv(u) @ Aq @ u

print(la.eigvals(r))
print()

for i in range(r.shape[0]):
    print([r[i][j] for j in range(r.shape[1])])



def sk_fun(i, j):
    if i == j + 1 or j == i + 1:
        return 0.5
    else:
        return 0

k = 10
Sk = FunMatrix(k, k, sk_fun).forNP()


def f(l):
    M = Aq
    u, sigmas, vh = la.svd(M - (l * np.eye(M.shape[0])))
    return 1./min(sigmas)

step = 0.01
re_range = (-1, 7)
im_range = (-1, 1)

with open("/tmp/mgla3.dat", "w") as out:
    re = re_range[0]
    while re_range[0] <= re <= re_range[1]:
        im = im_range[0]
        while im_range[0] <= im <= im_range[1]:
            s = "{} {} {}".format(re, im, log10(f(re + 1j * im)))
            out.write(s)
            out.write("\n")
            im += step
        out.write("\n")
        re += step


print()


def Hwave(a, b):
    A, B = AB(a, b)
    result = mat_dich(A, B)

    K = (conj(Aq) @ Aq) + np.eye(Aq.shape[0])
    Kwave = (conj(A) @ A) + (conj(B) @ B)

    w_min = result['H.norm'] / norm(la.inv(K)) / norm(Kwave)
    w_max = result['H.norm'] * norm(K) * norm(la.inv(Kwave))

    Hwave_norm = (w_max + w_min) / 2.

    return Hwave_norm

#Спектральный портрет
#with open('/tmp/mgla3.hwave.dat', 'w') as out:
#    b = 0
 #   while b <= 2:
  #      s = '{} {}'.format(b, log10(Hwave(a, b)))
   #     out.write(s + '\n')
    #    print(s)
     #   b += 0.005
