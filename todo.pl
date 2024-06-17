:- use_module(library(pce)).


save(Term) :- 
    Filename = "todos.database",
    open(Filename, write, Stream),
    write_canonical(Stream, Term),
    write(Stream, '.'),
    close(Stream).

load(Term) :-
    Filename = "todos.database",
    exists_file(Filename),
    open(Filename, read, Stream),
    read(Stream, Term),
    close(Stream), !.
load(Term) :-
    Term = [],
    save(Term), !.


dodaj(X) :- load(A), save([X|A]).

maxi([],0).
maxi([[Id,_,_]|B],Id) :- maxi(B,L), Id > L.
maxi([[Id,_,_]|B],L) :- maxi(B,L), Id =< L.

ostatnieid(X) :- load(T), maxi(T,X).

wypiszzadanie([]).
wypiszzadanie([A|B]) :- write(" - "), write(A), wypiszzadanie(B).

napisz([]).
napisz([A|B]) :- napisz(B), wypiszzadanie(A), nl.
wypisanie :- load(X), write(" - id - temat - tresc"), nl, napisz(X).

dodawanie :- write("tytul (enter zatwierda):"),read_line_to_string(user_input, Title), nl,
            write("tresc (enter zatwierda):"), read_line_to_string(user_input, Content),
            ostatnieid(Id), I is Id + 1,
            dodaj([I,Title,Content]).


usun(Id) :- load(T), delete(T,[Id,_,_],W),save(W).

levenshtein_distance(S1, S2, Distance) :-
    string_chars(S1, L1),
    string_chars(S2, L2),
    levenshtein_distance_list(L1, L2, Distance).

levenshtein_distance_list([], L, Distance) :- length(L, Distance).
levenshtein_distance_list(L, [], Distance) :- length(L, Distance).
levenshtein_distance_list([H1|T1], [H2|T2], Distance) :-
    ( H1 = H2 ->
        levenshtein_distance_list(T1, T2, Distance)
    ;
        levenshtein_distance_list([H1|T1], T2, D1),
        levenshtein_distance_list(T1, [H2|T2], D2),
        levenshtein_distance_list(T1, T2, D3),
        min_list([D1, D2, D3], D),
        Distance is D + 1, !
    ).
check_most_similar(_,[],'none').
check_most_similar(_, [[_,Title,_]|[]], Title).
check_most_similar(Text, [[_,Title,_]|Rest], S) :- check_most_similar(Text, Rest, S2), 
                                                levenshtein_distance(Text, Title, D1),
                                                levenshtein_distance(Text, S2, D2),
                                                (D1 < D2 ->
                                                    S is Title;
                                                    S is S2
                                                ).

dialogBox(Header, Text, Input) :-
        new(D, dialog(Header)),
        send(D, append(new(NameItem, text_item(Text)))),
        send(D, append(button(ok, message(D, return, NameItem?selection)))),
        send(D, append(button(cancel, message(D, return, @nil)))),
        send(D, default_button(ok)),
        get(D, confirm, Rval),
        Rval \== @nil,
        Input = Rval,
        free(D).


% Define the main predicate to create a window
create_window(Window) :-

    send(Window, clear),

    % create button
    new(Button, button('Add task', message(@prolog, add_button_click, Window))),
    % add button to the window
    send(Window, display, Button, point(0, 15)),

    % create text object
    %new(Text, text('Hello, Prolog!')),
    % set font size for the text
    %send(Text, font, font(times, bold, 20)),
    % add text to the window
    %send(Window, display, Text, point(100, 50)),

    load(Database),

    display_list(Window, Database,100,50),
    
    % display window
    send(Window, open).


add_button_click(Window) :-
    dialogBox('Add task', 'Tytul',Title),
    dialogBox('Add task', 'Zawartosc',Content),
    load(Data),
    check_most_similar(Title, Data, Similar),
    send(@display, inform, Similar),
    ostatnieid(Id), I is Id + 1,
    dodaj([I,Title,Content]),
    create_window(Window).

remove_button_click(Window, Id) :-
    usun(Id),
    create_window(Window).

display_list(_, [], _, _).
display_list(Window, [[Id,Title,Content]|Rest], X, Y) :-

    new(Button, button('remove task', message(@prolog, remove_button_click, Window, Id))),
    send(Window, display, Button, point(X-100, Y)),

    new(Text, text(Title)),
    send(Text, font, font(times, normal, 14)),
    TitlePos is Y,
    send(Window, display, Text, point(X, TitlePos)),

    new(ContentText, text(Content)),
    send(ContentText, font, font(times, normal, 14)),
    ContentPos is Y + 14,
    send(Window, display, ContentText, point(X, ContentPos)),

    NextY is Y + 40,
    display_list(Window, Rest, X, NextY).

% Start the program
start :- 
    % Create a new window object
    new(Window, picture('!!! To Do !!!')),
    
    % Set the size of the window
    send(Window, size, size(400, 600)),

    create_window(Window).

:- start.