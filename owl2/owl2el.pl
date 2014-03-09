
:- dynamic(counter/1).
:- assert(counter(1)).

subclassof(Depth,Out) :-
	expression(class, 1, A),
	classExpression(Exp),
	expression(Exp, Depth, B),
	atomic_list_concat(['SubClassOf(',A,' ',B,')'],'',Out).

subclassof(Depth,Out) :-
	expression(class, 1, A),
	classExpression(Exp),
	expression(Exp, Depth, B),
	atomic_list_concat(['SubClassOf(',B,' ',A,')'],'',Out).

classExpression(class).
classExpression(objectIntersectionOf).
classExpression(objectHasValue).
classExpression(objectOneOf).
classExpression(objectSomeValuesFrom).
classExpression(objectHasSelf).
classExpression(dataSomeValuesFrom).
classExpression(dataHasValue).

dataRange(datatype).
dataRange(dataIntersectionOf).

literal(quotedString).

expression(class, _, Out) :-
	name_with_random_number(':c', Out).

expression(datatype, _, Out) :-
	name_with_random_number(':datatype', Out).

expression(quotedString, _, '"something"').

expression(dataIntersectionOf, Depth, Out) :-
	Depth >= 1,
	NewDepth is Depth - 1,
	dataRange(Exp1),
	dataRange(Exp2),
	expression(Exp1, NewDepth, Out1),
	expression(Exp2, NewDepth, Out2),
	atomic_list_concat(['DataIntersectionOf(', Out1,' ', Out2,')'],'',Out).

expression(objectSomeValuesFrom, Depth, Out) :-
	Depth > 1,
	NewDepth is Depth - 1,
	name_with_random_number(':property', Name),
	classExpression(Exp),
	expression(Exp, NewDepth, Out1),
	atomic_list_concat(['ObjectSomeValuesFrom(',Name,' ',Out1,')'],'',Out).

expression(dataSomeValuesFrom, Depth, Out) :-
	Depth > 1,
	NewDepth is Depth - 1,
	name_with_random_number(':dataproperty', Name),
	dataRange(Exp),
	expression(Exp, NewDepth, Out1),
	atomic_list_concat(['DataSomeValuesFrom(',Name,' ',Out1,')'],'',Out).

expression(objectIntersectionOf, Depth, Out) :-
	Depth > 1,
	Depth1 is Depth - 1,
	binary('ObjectIntersectionOf', Depth1, Depth1, Out).

expression(objectIntersectionOf, Depth, Out) :-
	Depth > 2,
	Depth1 is Depth - 1,
	Depth2 is Depth - 2,
	binary('ObjectIntersectionOf', Depth1, Depth2, Out).

expression(objectHasValue, Depth, Out) :-
	Depth == 1,
	name_with_random_number(':property', PName),
	name_with_random_number(':individual', IName),
	atomic_list_concat(['ObjectHasValue(',PName,' ',IName,')'],'',Out).

expression(dataHasValue, Depth, Out) :-
	Depth == 1,
	name_with_random_number(':dataproperty', PName),
	literal(Exp),
	expression(Exp, Depth, Out1),
	atomic_list_concat(['DataHasValue(',PName,' ',Out1,')'],'',Out).

expression(objectHasSelf, Depth, Out) :-
	Depth == 1,
	name_with_random_number(':property', Name),
	atomic_list_concat(['ObjectHasSelf(',Name,')'],'',Out).

expression(objectOneOf, Depth, Out) :-
	Depth == 1,
	name_with_random_number(':individual', Ind1),
	name_with_random_number(':individual', Ind2),
	atomic_list_concat(['ObjectOneOf(',Ind1,' ',Ind2,')'], '', Out).

binary(Name, Depth1, Depth2, Out) :-
	classExpression(Exp1),
	classExpression(Exp2),
	expression(Exp1, Depth1, Out1),
	expression(Exp2, Depth2, Out2),
	atomic_list_concat([Name,'(',Out1,' ',Out2,')'],'',Out).

name_with_random_number(Prefix, Name) :-
	counter(Counter),
	retractall(counter(_)),
	NewCounter is Counter + 1,
	assert(counter(NewCounter)),
	atomic_list_concat([Prefix, NewCounter], '', Name).

