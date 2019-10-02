:- module(cooking, [go/0]).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(chr)).


go :- server(8888).

%!  server(+Port)
%
%   Start the server at http://localhost:Port

server(Port) :-
    create_chr_thread,
    http_server(http_dispatch,
                [ port(Port)
                ]).

:- chr_constraint
    chr_reset/0,
    touch/2,
    costume/2,
    get_costume/2.

:- http_handler('/touch', touched , []).

touched(Request) :-
    http_parameters(Request,
                [
                    me(Me, []),
                    dest(Dest, [])
                ]),
    do_in_chr_thread(touch(Me, Dest), get_costume(Me, Costume)),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~n~w~n', [Costume]).


:- http_handler('/costume', respond_current_costume , []).

respond_current_costume(Request) :-
    http_parameters(Request,
                [
                    me(Me, [])
                ]),
    do_in_chr_thread(true, get_costume(Me, Costume)),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~n~w', [Costume]).


:- http_handler('/reset', do_reset , []).

do_reset(_Request) :-
    do_in_chr_thread(chr_reset, get_dummy(_)),
    do_in_chr_thread(costume(egg, shell), get_dummy(_)),
    format('Access-Control-Allow-Origin: *~n'),
    format('Content-type: text/plain~n~nOK').

		 /*******************************
		 *          Game Logic          *
		 *******************************/

get_dummy(ok).

chr_reset \ costume(_, _) <=> true.
chr_reset \ touch(_, _) <=> true.
chr_reset \ get_costume(_, _) <=> true.
chr_reset <=> true.

% when I touch the pan I go from shell to fried egg
touch(egg, pan) \ costume(egg, shell) <=> costume(egg, friedegg).

% add get_costume with Y unbound to retrieve X's costume
costume(X, Current) \ get_costume(X, Is) <=> Is = Current.
get_costume(_, _) <=> fail.

		 /*******************************
		 * Debug help                   *
		 *******************************/


debug_constraints(Where) :-
    find_chr_constraint(X),
    debug(constraint(Where), '~w', [X]),
    fail.
debug_constraints(_).


		 /*******************************
		 *  Thread Component            *
		 *******************************/

create_chr_thread :-
   message_queue_create(_, [ alias(sub) ]),
   message_queue_create(_, [ alias(par) ]),
   thread_create(polling_sub, _, [ alias(chr) ]).

polling_sub :-
   % listen for new message on `sub` queue
   thread_get_message(sub, sync(ActionCHR, ResultCHR)),
   % do the actual constraint call
   (   call(ActionCHR)
   ;
       debug(constraint(polling_sub),
             'action constraint ~w failed unexpectedly~n',
             [ActionCHR])
   ),

   debug_constraints(polling_sub),
   % get the result using the get_foo pattern
   ResultCHR =.. List,
   append(StubList, [_], List),
   append(StubList, [Result], CallMeList),
   CallMe =.. CallMeList,
   (   call(CallMe)
   ;
       debug(constraint(polling_sub),
             'result constraint ~w failed unexpectedly~n',
             [ResultCHR])
   ),
   !, % nondet calls not allowed
   % send it back to the `par` message queue
   thread_send_message(par, Result),
   % repeat
   polling_sub.

%!  do_in_chr_thread(+ActionCHR:chr_constraint,
%!         +ResultCHR:chr_constraint) is det
%
%   queries ActionCHR in the chr thread, which must be
%   grounded chr_constraint or prolog predicate,
%   then calls ResultCHR, whose last argument must be unbound.
%   the last argument will be bound as if a direct chr call
%   was made.
%
% eg to touch the egg to the pan and then get the egg's costume do
% do_in_chr_thread(touch(egg, pan), get_costume(egg, Costume))
%
% Note that these are effectively called in once/1
%
do_in_chr_thread(ActionCHR, ResultCHR) :-
   ResultCHR =.. List,
   append(_, [Result], List),
   thread_send_message(sub, sync(ActionCHR, ResultCHR)),
   thread_get_message(par, Result).

:- debug(constraint(_)).
