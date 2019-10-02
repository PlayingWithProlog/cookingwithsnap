:- module(cooking, [go/0]).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(chr)).

go :- server(8888).

%!  server(+Port)
%
%   Start the server at http://localhost:Port

server(Port) :-
    http_set_session_options(
        [ create(noauto)
        ]),
    http_server(http_dispatch,
                [ port(Port)
                ]).

:- chr_constraint  chr_reset/0, touch/2, costume/2.

:- http_handler('/touch', touched , []).

touched(Request) :-
    http_parameters(Request,
                [
                    me(Me, []),
                    dest(Dest, [])
                ]),
    touch(Me, Dest),
    find_chr_constraint(costume(Me, Costume)),
    format('Content-type: text/text~n~n~w', [Costume]).

:- http_handler('/costume', do_costume , []).

do_costume(Request) :-
    http_parameters(Request,
                [
                    me(Me, [])
                ]),
    debug_constraints(do_costume),
    find_chr_constraint(costume(Me, Costume)),
    format('Content-type: text/text~n~n~w', [Costume]).

:- http_handler('/reset', do_reset , []).

do_reset(_Request) :-
    chr_reset,
    costume(egg, 1),
    debug_constraints(do_reset),
    format('Content-type: text/text~n~nOK').


chr_reset \ costume(_, _) <=> true.
chr_reset \ touch(_, _) <=> true.
chr_reset <=> true.

touch(egg, pan) \ costume(egg, 1) <=> costume(egg, 2).

debug_constraints(Where) :-
    find_chr_constraint(X),
    debug(constraint(Where), '~w', [X]),
    fail.
debug_constraints(_).






