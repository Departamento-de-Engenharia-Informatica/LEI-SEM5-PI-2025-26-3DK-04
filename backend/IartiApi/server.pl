% Bibliotecas 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- http_handler('/lapr5', responde_ola, []).
:- http_handler('/register_user', register_user, []).
:- http_handler('/send_file_post', send_file_post, []).

server(Port) :-						
        http_server(http_dispatch, [port(Port)]).

% Tratamento de 'http://localhost:5000/lapr5'
responde_ola(_Request) :-					
        format('Content-type: text/plain~n~n'),
        format('Olá LAPR5!~n').

% M�TODO POST
% http_client:http_post('http://localhost:5000/register_user', form_data([name='Jos�', sex=male, birth_year=1975]), Reply, []).

register_user(Request) :-
    http_parameters(Request,
                    [ name(Name, []),
                      sex(Sex, [oneof([male,female])]),
                      birth_year(BY, [between(1850,10000)])
                    ]),
    format('Content-type: text/plain~n~n'),
    format('User registered!~n'),
	format('Name: ~w~nSex: ~w~nBirth Year: ~w~n',[Name,Sex,BY]).

% M�TODO POST enviando um ficheiro de texto
% http_client:http_post('http://localhost:5000/send_file_post', form_data([file=file('./teste.txt')]), Reply, []).

send_file_post(Request) :-
	http_parameters(Request,[ file(X,[])]),
    format('Content-type: text/plain~n~n'),
	format('Received: ~w~n',[X]).