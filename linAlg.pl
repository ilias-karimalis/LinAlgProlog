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
  
% lin_independent(VectorList) - succeeds if every Vector in the VectorList is
%                               a valid vector of the same dimension and field
%                               and they are linearly independent.
% NOTE: This relies on the implementation of Det.
% TODO: VectorList must be 1x1 or 2x2 due to Det implementation.
lin_independent(VectorList) :-
  det(VectorList, Det),
  nonZero(Det).

% det(Matrix, Det) - succeeds if Det is the determinant of the Matrix.
% NOTE: Currently has only been implemented for the 2X2 Case.
% TODO: Implement for arbitrary case.
det([vector([A], F, 1)], A) :-
  vector([A], F, 1).
det([vector([A,C], F, 2),vector([B,D], F, 2)], Det) :-
  vector([A,C], F, 2),
  vector([B,D], F, 2),
  mult_scalar(A,D,X),
  mult_scalar(B,C,Y0),
  negative_identity(F, NOE),
  mult_scalar(Y0, NOE, Y),
  add_scalar(X, Y, Det).

% transpose(M1, M2) - succeeds if M2 is the transpose of M1.
% NOTE: This currently only works for nxn matrices with n>=2.
% TODO: Implement for nxn, arbitrary n values.
transpose([vector([A,C], F, 2), vector([B,D], F, 2)],
          [vector([A,B], F, 2), vector([C,D], F, 2)]) :-
  vector([A,C], F, 2),
  vector([B,D], F, 2).
transpose([vector([A], F, 1)], [vector([A], F, 1)]) :-
  vector([A], F, 1).

% adjugate(M1, M2) - succeeds if M2 is the adjugate of M1.
% NOTE: This currently only work for nxn matrices with n>=2.
% TODO: Implement for nxn, arbitrary n values.
adjugate([vector([A,C], F, 2), vector([B,D], F, 2)],
         [vector([D,C1], F, 2), vector([B1,A], F, 2)]) :-
  negative_identity(F, NI),
  mult_scalar(B, NI, B1),
  mult_scalar(C, NI, C1).


% vector([real(1), real(0)], F, 2).
% vector([real(0), real(1)], F, 2).
% vector([complex(0,1), complex(0,-1)], F, 2).
