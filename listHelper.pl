/**/

/*
 * Useful List Predicates.
 */

% len(List, N) - succeeds if the length of List is N.
len([],0).
len([_|T],N) :- 
  len(T,X),  N  is  X+1. 
