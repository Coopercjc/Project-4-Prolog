%====================================================
%=                                                  =
%=            TYPE INFERENCE IN PROLOG              =
%=                                                  =
%====================================================

:- begin_tests(typeInf).
:- include(typeInf). 

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
        if(>(float,float), [iplus(int,int)], [iminus(int,int)])
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
            gvLet(v, float, fminus(float,float)),
            lvLet(x, float, fminus(float,float), [
                lvLet(y, int, itimes(int,int), [
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

%test definining and calling a function
test(functionDefCall, [nondet]) :-
    infer([
        funcLet(runTest, [string, int, bool], [<(float, float)]),
        funcCall(runTest(X,Y))
        ], Ret),
        assertion(X==string),
        assertion(Y==int),
        assertion(Ret==bool).
  
:-end_tests(typeInf).

