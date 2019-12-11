
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
    return 1./min(sigmas) + 10

step = 0.01
re_range = (-1, 7)
im_range = (-1, 1)

with open("/tmp/mgla3.dat", "w") as out:
    re = re_range[0]
    while re_range[0] <= re <= re_range[1]:
        im = im_range[0]
        while im_range[0] <= im <= im_range[1]:
            s = "{} {} {}".format(re, im, log(f(re + 1j * im)))
            out.write(s)
            out.write("\n")
            im += step
        out.write("\n")
        re += step
