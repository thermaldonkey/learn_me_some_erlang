-module(tree).
-export([empty/0, insert/3, lookup/2]).

% create a new tree
empty() -> {node, 'nil'}.

% If tree is empty, set its new value and make 2 empty children
insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
% If non-empty tree, and new key smaller than node key, new node is 'Smaller' child
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
% If non-empty tree, and new key larger than node key, new node is 'Larger' child
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
% If non-empty tree, and keys are similar, reassign current value
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.

% If searching empty tree for value, return undefined atom
lookup(_, {node, 'nil'}) ->
    undefined;
% If searching non-empty tree with matching key, return value in tuple
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
% If searching non-empty tree with key smaller than node key, lookup in 'Smaller' child
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    lookup(Key, Smaller);
% If searching non-empty tree with key larger than node key, lookup in 'Larger' child
lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).
