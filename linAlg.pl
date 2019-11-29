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
  mult_scalar(Y0, real(-1), Y),
  add_scalar(X, Y, Det).

% vector([real(1), real(0)], F, 2).
% vector([real(0), real(1)], F, 2).
% vector([complex(0,1), complex(0,-1)], F, 2).
