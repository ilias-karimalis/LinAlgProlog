/**/

/* 
 * Defining Scalar Fields and their Properties.
 * */

% field(Obj, Field) - succeeds if the Obj is a member of the Field.
field(real(X), real) :-
  number(X). 
field(complex(A,B), complex) :-
  number(A), number(B).
%field(real(A), complex) :-
%  number(A).

% subfield(F0, F1) - succeeds if F0 is a subfield of F1.
subfield(X, X).
subfield(real, complex).

% extension_field(F0, F1, F) - succeeds if F is an extension field of F0 and F1.
extension_field(F0, F1, F1) :-
  subfield(F0, F1).
extension_field(F0, F1, F0) :-
  subfield(F1, F0).
extension_field(F, F, F).

% in_field(List, Field) - succeeds if every element of List is an element of 
%                         the Field.
in_field([H|T], Y) :-
  field(H, S),
  subfield(S, Y),
  in_field(T, Y).
in_field([],_).

% nonZero(X) :- X is not the zero element of its field.
nonZero(real(X)) :- X =\= 0.
nonZero(complex(X,_)) :- X =\=0.
nonZero(complex(_,X)) :- X =\=0.

% negative_identity(F, NOE) :- NOE is the additive inverse of the identity
%                              element of F.
negative_identity(real, real(-1)).
negative_identity(complex, complex(-1,0)).

% mult_identity(F, I) :- I is the multiplicative identity of F
mult_identity(real, real(1)).
mult_identity(complex, complex(1,0)).

% mult_inverse(M, I) :- succeeds if I is the multiplicative inverse of M.
mult_inverse(real(X), Y) :- Y is 1/X.

% add_scalar(A, B, C) - succeeds if A + B = C, under the constraints of their
%                       specific field.
add_scalar(real(A), real(B), real(C)) :-
  C is A + B. 
add_scalar(complex(A0,B0), complex(A1, B1), complex(A2,B2)) :-
  A2 is A0 + A1,
  B2 is B0 + B1.
add_scalar(complex(A0,B0), real(A1), complex(A, B0)) :-
  A is A0 + A1.
add_scalar(real(A0), complex(A1, B1), complex(A, B1)) :-
  A is A0 + A1.

% add_scalars(List1, List2, Res) - succeeds if the Res list is elementwise the sum
%                                 of List1 and List2.
% NOTE: here we define sum as it has been defined for the specific field to 
%       which the elements of the Lists belong.
add_scalars(H1, H2, H3) :-
  maplist(add_scalar, H1, H2, H3).

% mult_scalar(X, Y, Z) - succeeds X * Y = Z, under the constraints of theis
%                        specific field.
mult_scalar(real(A), real(B), real(C)) :-
  C is A * B.
mult_scalar(complex(A0, B0), complex(A1, B1), complex(X, Y)) :-
  X is A0*A1 - B0*B1,
  Y is A0*B1 + A1*B0.
mult_scalar(real(A), complex(C, D), complex(X, Y)) :-
  X is A*C,
  Y is A*D.
mult_scalar(complex(C, D), real(A), complex(X, Y)) :-
  X is A*C,
  Y is A*D.

% pow2(Scalar, Res) - succeeds if Res = Scalar^2.
% NOTE: here we define the exponent as repeated multiplication which is in 
%       itself defined by the Field to which the scalar belongs.
pow2(X,Y) :-
  mult_scalar(X, X, Y).
root2(real(X),Y) :- Y is sqrt(X).

% mult_scalars(Scalar, List, Res) - succeeds if the Res list is the elementwise
%                                   product of Scalar and List.
% NOTE : here we define product as it has been defined for the specific field 
%        to which the elements of the List belong.
mult_scalars(Scalar, [H|T], [H1,T1]) :-
  mult_scalar(Scalar, H, H1),
  mult_scalars(Scalar, T, T1).
mult_scalars(_, [], []).

% sum_entries(List, Sum) - succeeds if the sum of the entries of List is Sum.
% NOTE: here we define sum as it has been defined for the specific field to 
%       which the elements of the List belong.
sum_entries([real(E)|T],Sum) :-
  sum_entries([real(E)|T], real(0), Sum).
sum_entries([complex(A,B)|T],Sum) :-
  sum_entries([complex(A,B)|T], complex(0, 0), Sum).

sum_entries([], Sum, Sum).
sum_entries([H|T], Sum0, Sum) :-
  add_scalar(H, Sum0, Sum1),
  sum_entries(T, Sum1, Sum).
