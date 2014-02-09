-module(hhfuns).
-compile(export_all).

% Proof that everything is a function...
one() -> 1.
two() -> 2.

% Call with
% > hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
add(X,Y) -> X() + Y().

% That was confusing. We'll try to exemplify.
% These two functions basically do the same thing:
increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

% Abstract out the similarities into a new function
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.
% Boom. call with
% > hhfuns:map(fun hhfuns:incr/1, L).
% and you'll never have to manually loop through a list again.

% Use anonymous functions to clean up the module
% Fn = fun() -> a end.
% > hhfuns:map(fun(X) -> X + 1 end, L).

% > PrepareAlarm = fun(Room) ->
% >                     io:format("Alarm set in ~s.~n",[Room]),
% >                     fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
% >                   end.
% #Fun<erl_eval.20.67289768>
% > AlarmReady = PrepareAlarm("bathroom").
% Alarm set in bathroom.
% #Fun<erl_eval.6.13229925>
% > AlarmReady().
% Alarm tripped in bathroom! Call Batman!
% ok

% only keep even numbers
even(L) -> lists:reverse(even(L,[])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
    even(T, [H|Acc]);
even([_|T], Acc) ->
    even(T, Acc).

% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L,[])).
 
old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
    old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
    old_men(People, Acc).

% abstracted filtering method
filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H|Acc]);
        false -> filter(Pred, T, Acc)
    end.
% Call with
% > hhfuns:filter(fun(X) -> X rem 2 == 0 end, Numbers).
% and
% > hhfuns:filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People).

% find the maximum of a list
max([H|T]) -> max2(T, H).
 
max2([], Max) -> Max;
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_|T], Max) -> max2(T, Max).
 
% find the minimum of a list
min([H|T]) -> min2(T,H).
 
min2([], Min) -> Min;
min2([H|T], Min) when H < Min -> min2(T,H);
min2([_|T], Min) -> min2(T, Min).
 
% sum of all the elements of a list
sum(L) -> sum(L,0).
 
sum([], Sum) -> Sum;
sum([H|T], Sum) -> sum(T, H+Sum).

% Throw away the guards, to be user-provided, and require an initial value
% to accumulate for the return, yielding abstraction:
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).
% Use this for starters
% > [H|T] = [1,7,3,5,9,0,2,3].
% To simulate max/1
% > hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).
% To simulate min/1
% > hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
% To simulate sum/1
% > hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).

% Functions to look into
% all/2
% any/2
% dropwhile/2
% takewhile/2
% partition/2
