/**/
:- [vector].


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

/* trace(M, T) - succeeds if M is a square matrix and T is its trace.
trace([vector([X0|XS0], F, D0)|VS], T) :-
  len([vector([X0|XS0], F, D0)|VS], D0),
  trace(VS)
  add_scalar(X0, TR, T).
  */

% inverse(M, I) - succeeds if I is the inverse of M.
% NOTE: This currently only works for nxn matrices with n>=2.
% TODO: Implement for nxn, arbitrary n values.
inverse([vector([V], F, D)], [vector([M], F, D)]) :-
  mult_inverse(V, M).
inverse(Matrix, Inverse) :-
  adjugate(Matrix, Adjugate),
  det(Matrix, Det),
  mult_matrix_scalar(Det, Adjugate, Inverse).

% vector([real(1), real(0)], F, 2).
% vector([real(0), real(1)], F, 2).
% vector([complex(0,1), complex(0,-1)], F, 2).

% multiply all elements of the matrix by a scalar
mult_matrix_scalar(Scalar, [vector(E, F, D)|T], [vector(X, F, D)|XS]) :-
  mult_scalars(Scalar, E, X),
  mult_matrix_scalar(Scalar, T, XS).
mult_matrix_scalar(_, [], []).
