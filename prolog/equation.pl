
eval(N, N).
eval(A+B, N) :- eval(A, X), eval(B, Y), N is X + Y.
eval(A-B, N) :- eval(A, X), eval(B, Y), N is X - Y.
eval(A*B, N) :- eval(A, X), eval(B, Y), N is X * Y.
eval(A/B, N) :- eval(A, X), eval(B, Y), N is X / Y.

valid_eq(A, [A|Rs], Rs).
valid_eq(A+B, Ls, Rs) :-
  append(Us, Rs, Ls),
  append(As, Bs, Us),
  As = [_|_],
  Bs = [_|_],
  valid_eq(A, Ls, Ars),
  valid_eq(B, Ars, Rs).
valid_eq(A-B, Ls, Rs) :- valid_eq(A+B, Ls, Rs).
valid_eq(A*B, Ls, Rs) :- valid_eq(A+B, Ls, Rs).
valid_eq(A/B, Ls, Rs) :- valid_eq(A+B, Ls, Rs).
valid_eq(A=B, Ls, Rs) :- valid_eq(A+B, Ls, Rs).

lol(A, A).
