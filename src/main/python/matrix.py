import numpy as np


class Matrix:
    def __init__(self, i_size, j_size):
        self.i_size = i_size
        self.j_size = j_size

    def get(self, i, j):
        pass

    def print(self):
        for i in range(self.i_size):
            print(i, [self.get(i, j) for j in range(self.j_size)])

    def forNP(self):
        return np.array([[self.get(i, j) for j in range(self.j_size)] for i in range(self.i_size)])


class FunMatrix(Matrix):
    def __init__(self, i_size, j_size, fun):
        super(FunMatrix, self).__init__(i_size, j_size)
        self.fun = fun

    def get(self, i, j):
        return self.fun(i, j)
