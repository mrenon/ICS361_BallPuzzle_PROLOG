% -*- mode: Prolog;   -*-
% Mauricio Renon, November 20, 2014
% ICS 361, Assignment # 5
% File: balls.pro
%
% adapted and changed from
%
% http://stackoverflow.com/questions/23189952/prolog-permutation-colors-right-amount-of-combination-error
%

% a program that find solutions for each of the following colored ball
% problems with different sets of constraints.

% to run, type either
% sit1, sit2 or sit3.

% select an element for use in permutation test
%
% If the element is the head of the list, then it is in the list, and the tail is left
selectE(Element, [Element|Tail], Tail).
% If the two lists have the same head, check for more elements in the rest of the lists
selectE(Element, [Head|Tail1], [Head|Tail2]) :-
        selectE(Element, Tail1, Tail2).

% generate permutations
%
% The empty list is a permutation of itself
permutationQ([],[]).
% List1 is a permutation of List2 if each element occurs in both lists
%    the same number of times
permutationQ(List, [Head|Tail]) :- selectE(Head, List, Rest),
                                  permutationQ(Rest, Tail).
%

% There are 6 colors - 2 blues, 2 greens, 2 yellow
%
sit1 :- permutationQ([green,green,blue,blue,yellow,yellow],[A,B,C,D,E,F]),
    \+ A=B, \+ B=C, \+ C=D, \+ D=E, \+ E=F,
    printout([A,B,C,D,E,F]).   % print any solution you find

% print solutions of sit1
printout([A,B,C,D,E,F]) :-
   nl,
   write(A), write(', '),
   write(B), write(', '),
   write(C), write(', '),
   write(D), write(', '),
   write(E), write(', '),
   write(F).


% There are 6 colors - 1 red, 1 blue, 4 blacks,
%
sit2 :- permutationQ([black,black,black,black,red,blue],[A,B,C,D,E,F]),
    ((A==red -> D==blue);
         (A==blue -> D==red);
         (B==red -> E==blue);
         (B==blue -> E==red);
         (C==red -> F==blue);
         (C==blue -> F==red);
         (D==red -> C==blue);
         (D==blue -> C==red)),
    printout2([A,B,C,D,E,F]).   % print any solution you find

% print solutions of sit2
printout2([A,B,C,D,E,F]) :-
    nl,
    write(A), write(', '),
    write(B), write(', '),
    write(C), write(', '),
    write(D), write(', '),
    write(E), write(', '),
    write(F).

% There are 8 colors - 3 greens, 2 whites, 2 reds, 1 black
sit3 :- permutationQ([black,white,white,red,red,green,green,green],[A,B,C,D,E,F,G,H]),
    % The colors in B and C are not green.
    \+ B=green,
    \+ C=green,
    % The colors in E and F are not green because the colors in F and G are not red.
    \+ E=green,
    \+ F=green,
    % Since red can't be in H, green can't be in G.
    \+ G=green,
    % The colors in D and H are the same color.
    D=H,
    % The colors in A and G are of different colors.
    \+ A=G,
    % The color in F and G are not red.
    \+ F=red,
    \+ G=red,
    % Red can't be in A because there isn't any other position on the left for the green.
    \+ A=red,
    % The colors in C and D are not red because the colors in B and C are not green.
    \+ C=red,
    \+ D=red,
    % Whites are neither A nor H.
    \+ A=white,
    \+ H=white,
        % White is not on D because white can't be on H.
    \+ D=white,
    printout3([A,B,C,D,E,F,G,H]).   % print any solution you find

% print solutions of sit3
printout3([A,B,C,D,E,F,G,H]) :-
    nl,
    write(A), write(', '),
    write(B), write(', '),
    write(C), write(', '),
    write(D), write(', '),
    write(E), write(', '),
    write(F), write(', '),
    write(G), write(', '),
    write(H).
