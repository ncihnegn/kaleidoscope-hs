def binary : 1 (x y) 0;  # Low-precedence operator that ignores operands.

def fibi(x)
  var a = 1, b = 1, c = 0 in
  (for i = 3, i < x, 1.0 in
    c = (a + b) :
    a = b :
    b = c
  ): b;

fibi(10);
