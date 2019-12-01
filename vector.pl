/**/
:- [fields].
:- [listHelper].

/*
 * Defining Vectors and their Properties.
 * */

% vector(Entries, Field, Dimension) - succeeds if the vector with given Entries
%                                     is of the correct Dimension and Field.
vector(Entries, Field, Dimension) :-
  len(Entries, Dimension),
  in_field(Entries, Field).

% vdot(Vector, Vdot) - succeeds if Vdot is the dot product of the vector Vector,
%                      and if Vector is in fact a valid Vector.
vdot(vector(Entries, Field, Dimension), Vdot) :-
  vector(Entries, Field, Dimension),
  sum_entries(Entries, Vdot),
  in_field(Entries, Field).

% vsum(V1, V2, Res) - succeeds if V1, V2, Res are valid vectors and V1+V2=Res.
vsum(vector(E1, F0, D),
     vector(E2, F1, D),
     vector(R, F, D)) :-
  vector(E1, F0, D),
  vector(E2, F1, D),
  add_scalars(E1, E2, R),
  extension_field(F0, F1, F).

% scalar_product(C, V0, V) - succeeds if V0, V1 are valid vectors and C*V0=V1.
scalar_product(Scalar,
               vector(E0, F0, D),
               vector(E1, F1, D)) :-
  vector(E0, F0, D),
  mult_scalars(Scalar, E0, E1),
  field(Scalar, Fs),
  extension_field(Fs, F0, F1).

% norm2(Vector, N) - succeeds if N is the square of the norm of a valid Vector.
norm2(vector([H|T], F, D), Norm) :-
  pow2(H, H2),
  D1 is D-1,
  norm2(vector(T, F, D1), Norm1),
  pow2(Norm1, N1Sqr),
  add_scalar(H2, N1Sqr, Norm).
norm2(vector([H], _, 1), Norm) :- pow2(H, Norm).
  
% vector_proj(V, W, Res) - succeeds if Res is the projection of V
% W is a basis of dimension D. W is a matrix.
vector_proj(V, [W|T], Res) :-  
  dot_product(V,W,VdotW), 
  scalar_product(W, VdotW, W1dotV),
  vector_proj(V, T, Nextres), 
  vsum(W1dotV, Nextres, Res).
vector_proj(vector(E, F, D), [], vector(Zeros, F, D) :- zerovector(D, Zeros).

zerovector(2, vector([0,0], F, 2)).
zerovector(3, vector([0,0,0], F, 3)).


zerovector(0, vector([],F,D)).
zerovector(D, vector([0|Z], F, N)) :- zerovector(Dn, vector(Z, F, N)), Dn is D-1.

dot_product(vector([H1|T1], F, D), vector([H2|T2], F, D), Res) :- 
  dot_product(vector(T1, F, Dn), vector(T2, F, Dn), Nextres),
  Res is H1 * H2 + Nextres.

dot_product(vector([], F, D), vector([], F, D), 0).

