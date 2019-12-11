
N1 = 10
N2 = 20
M = 24
delta = 10**(-3)


def x(i):
    t = i - N1
    if t in range(-N1, 0):
        return - float(t) / float(N1)
    elif t in range(0, N2):
        return float(t) / float(N2)


if __name__ == '__main__':
    print([x(i) for i in range(0, N2 + N1)])
