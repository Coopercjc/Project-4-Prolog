:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).
/*
    ALL OF THE FOLLOWING TESTS ARE TO CHECK THE STRUCTURE OF BASIC ARITHMETIC OPERATIONS
*/
%   ADDITION
% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% this test should fail
test(typeExp_fplus_F, [fail]) :-
    typeExp(fplus(float, float), int).

test(typeExp_fplus_T, [true(T == float)]) :-
    typeExp(fplus(float, float), T).

%   SUBTRACTION
% this test should fail
test(typeExp_iSUBT_F, [fail]) :-
    typeExp(isubt(int, int), float).

test(typeExp_iSUBT_T, [true(T == int)]) :-
    typeExp(isubt(int, int), T).

% this test should fail
test(typeExp_fSUBT_F, [fail]) :-
    typeExp(fsubt(float, float), int).

test(typeExp_fSUBT_T, [true(T == float)]) :-
    typeExp(fsubt(float, float), T).

%   MULTIPLICATION
% this test should fail
test(typeExp_iMULT_F, [fail]) :-
    typeExp(imult(int, int), float).

test(typeExp_iMULT_T, [true(T == int)]) :-
    typeExp(imult(int, int), T).

% this test should fail
test(typeExp_fMULT_F, [fail]) :-
    typeExp(fmult(float, float), int).

test(typeExp_fMULT_T, [true(T == float)]) :-
    typeExp(fmult(float, float), T).

%   DIVISION
% this test should fail
test(typeExp_iDIV_F, [fail]) :-
    typeExp(idiv(int, int), float).

test(typeExp_iDIV_T, [true(T == int)]) :-
    typeExp(idiv(int, int), T).

% this test should fail
test(typeExp_fDIV_F, [fail]) :-
    typeExp(fdiv(float, float), int).

test(typeExp_fDIV_T, [true(T == float)]) :-
    typeExp(fdiv(float, float), T).


% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

%Test for If
test(simple_if, [nondet]) :-
    typeStatement( if(true, [3], [4]), T),
    assertion(T==int).

%Tesdt for For
test(simple_for, [nondet]) :-
    typeStatement(for(v, T, 13, [(if(3 =< 4, [3.0], [4.0])), (if(3 =< 4, [3], [4]))])), 
    assertion(T==int).

%bool Expressions
test(bool_expressions, [nondet] ) :-
    typeStatement( if(1.0 < 1.0, [1], [2]), T),
    typeStatement( if(3 =< 4, [3], [4]), T),
    typeStatement( if(5 > 6, [5], [6]), T),
    typeStatement( if(7 >= 8, [7], [8]), T),
    typeStatement( if(==(9, 10), [9], [10]), T),
    assertion(T==int).

%code blocks
test(code_block, [nondet]) :-
    typeCode([if(1 =< 2, [1.0], [2.0]), if(3 =< 4, [3], [4]), if(==(9, 13), [26>13], [39>26])], T),
    assertion(T==bool).


%expressions as statements
test(exprStat, [nondet]) :-
    infer([
        int,
        float,
        bool
        ], Ret),
        assertion(Ret==bool).

%test if statements
test(ifStat, [nondet]) :-
    infer([
        if(>(float,float), [iplus(int,int)], [isubt(int,int)])
        ], Ret),
        assertion(Ret==int).

%test nested let in statements
test(letIn, [nondet]) :-
    deleteGVars(),
    infer([
        lvLet(x, int, iplus(int,int), [
            lvLet(y, float, fplus(float,float), [
                lvLet(z, bool, <(float,float), [
                    getVar(x,X),
                    getVar(y,Y),
                    getVar(z,Z)
                ])
            ])
        ])
        
        ], unit),

        assertion(X==int),
        assertion(Y==float),
        assertion(Z==bool).

%global variables from local scope
test(letInGlobal, [nondet]) :-
    deleteGVars(),
    infer([
        gvLet(v, float, fsubt(float,float)),
        lvLet(x, float, fsubt(float,float), [
            lvLet(y, int, imult(int,int), [
                lvLet(z, bool, ==(float,float), [
                    getVar(x,X),
                    getVar(y,Y),
                    getVar(z,Z),
                    getVar(v,V)
                ])
            ])
        ])
        
        ], unit),

        assertion(X==float),
        assertion(Y==int),
        assertion(Z==bool),
        assertion(V==float).

:-end_tests(typeInf).
