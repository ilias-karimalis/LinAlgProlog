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

% norm1(Vector, Norm) - succeeds if Norm is the 1-Norm of the vector Vector,
%                       and if Vector is in fact a valid Vector.
norm1(vector(Entries, Field, Dimension), Norm) :-
  vector(Entries, Field, Dimension),
  sum_entries(Entries, Norm),
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

% computes the dot product of two vectors.

dot_product(vector([H1|T1], F, D), vector([H2|T2], F, D), Res) :- 
  dot_product(vector(T1, F, Dn), vector(T2, F, Dn), Nextres),
  mult_scalar(H1, H2, Prod).
  add_scalar(Nextres, Prod, Res).

dot_product(vector([], F, D), vector([], F, D), 0).


