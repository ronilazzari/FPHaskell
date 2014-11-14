import Prelude hiding ((^))

m ^ 0 = 1
m ^ n = (m * m) ^ (n - 1)
