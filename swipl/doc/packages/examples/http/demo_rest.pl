:- module(demo_http_rest,
          [ server/1                            % +Port
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

server(Port) :-
    http_server(http_dispatch,
                [ port(Port)
                ]).

/** <module> Demonstrate dispatching using wildcarts

This example demonstrates dispatching events   using wildcarts, normally
used for REST APIs. To run this  test,   start  the server and use e.g.,
`curl` as:

```
curl http://localhost:5000/user/jan
curl http://localhost:5000/user/default
curl http://localhost:5000/user/jan/parents

```
*/

:- http_handler(root(user/User),         user(M, User), [method(M)]).
:- http_handler(root(user/default),      default_user,  []).
:- http_handler(root(user/User/parents), parents(User), []).

user(Method, User, _Request) :-
    format('Content-type: text/plain~n~n'),
    format('Run method ~p on user ~p~n', [Method, User]).

default_user(_Request) :-
    format('Content-type: text/plain~n~n'),
    format('Send a default user~n', []).

parents(User, _Request) :-
    format('Content-type: text/plain~n~n'),
    format('Asked for parents for ~p~n', [User]).

