
%predicado que cumple constructora
%date(DD,MM,YYYY,Fecha)
%Permite Construir una fecha
%dom:int x int x int x Outpout
%rec: tda Fecha

fecha(DD,MM,YYYY,Fecha):-number(DD),DD=<31,MM=<12,YYYY > 0,DD > 0, YYYY >0 ,Fecha = [DD,MM,YYYY].



%predicado que Permite calcular si un número es par o no
%DOM: int
%REC: boolean
%Tipo de meta: Secundaria
esPar(0):-!, true.
esPar(1):-!, false.
esPar(Numero):- Aux1 is Numero-2, esPar(Aux1).





%predicado que Permite saber si un elemento cualquiera es del tipo fecha
%dom: elemento cualquiera
%rec: boolean
%Tipo de meta: primaria

esFecha([DD,MM,YYYY]):- number(DD), number(MM),number(YYYY),checkLargoFecha([DD,MM,YYYY])%verifica que sean numeros
                       ,getDiasMes([_,MM,YYYY],MaximoDias), MaximoDias >= DD.


%predicado que verifica que el largo de la lista ser igual a 3
%dom: lista
%rec: boolean
%Tipo de meta: Secundaria
%
checkLargoFecha(Fecha):- length(Fecha,Aux), Aux==3,!.

% predicado que Obtiene el numero de dias maximo que puede tener un mes
% considerando si es año bisiseto o no
%DOM: tda fecha, argumento de salid
% REC: int
%Tipo de meta: Secundaria
getDiasMes([_,MM,YYYY],MaximoDias):-
number(MM),number(YYYY),MM>0,MM<13, asignaDias([_,MM,YYYY],MaximoDias).


asignaDias([_,MM,_],Dias):- MM == 1, Dias = 31,!.
asignaDias([_,MM,_],Dias):- MM == 3, Dias = 31,!.
asignaDias([_,MM,_],Dias):- MM == 5, Dias = 31,!.
asignaDias([_,MM,_],Dias):- MM == 7, Dias = 31,!.
asignaDias([_,MM,_],Dias):- MM == 8, Dias = 31,!.
asignaDias([_,MM,_],Dias):- MM == 10, Dias = 31,!.
asignaDias([_,MM,_],Dias):- MM == 12, Dias = 31,!.
asignaDias([_,MM,_],Dias):- MM == 4, Dias = 30,!.
asignaDias([_,MM,_],Dias):- MM == 6, Dias = 30,!.
asignaDias([_,MM,_],Dias):- MM == 9, Dias = 30,!.
asignaDias([_,MM,_],Dias):- MM == 11, Dias = 30,!.
asignaDias([_,MM,YYYY],Dias):- MM == 2, esBisiesto([_,_,YYYY]),Dias = 29,!;
                                       Dias = 28.


%predicado que Obtiene el maximo de día si es que el mes es par o impar
%DOM: tda fecha, outpout
%REC: int
%Tipo de meta: Secundaria
mesesPares([_,MM,_],MaximoDias):- MM\=2, esPar(MM),MaximoDias=30,!.
mesesPares([_,MM,_],MaximoDias):- MM\=2,not(esPar(MM)), MaximoDias=31,!.
mesesPares([_,MM,YYYY],MaximoDias):- MM == 2, esBisiesto([_,_,YYYY]),MaximoDias = 29,!;
                                       MaximoDias = 28.

%predicado que Permite saber si un año es bisiesto o no
%Dom: tda Fecha
%Rec: boolean
%Tipo de meta: Secundaria
esBisiesto([_,_,YYYY]):- Aux1 is mod(YYYY,400), Aux1 == 0,!;
    Aux2 is mod(YYYY,4), Aux2 == 0,true, Aux3 is mod(YYYY,100), not(Aux3 == 0).

%Selectores
%predicado que Obtiene el día perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int
%Tipo de meta: Primaria
getDD([DD,MM,YYYY],Dia):- esFecha([DD,MM,YYYY]),DD = Dia.

%predicado que Obtiene el mes perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int
%Tipo de meta: Primaria
getMM([DD,MM,YYYY],Mes):- esFecha([DD,MM,YYYY]),MM = Mes.

%predicado que Obtiene el año perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int
%Tipo de meta: Primaria
getYYYY([DD,MM,YYYY],Annio):- esFecha([DD,MM,YYYY]),YYYY = Annio.

/*
length_list([],0).
length_list([_|Xs],C):-length_list(Xs,C_new),C is C_new+1.*/

/* TDA USER */
%predicado que Constructor del tda user
%DOM: string x string x tda date x Output
%REC: tda user
user(Name,Password,Fecha,Usuario):- string(Name),string(Password),%getDD(Fecha,DD),getMM(Fecha,MM),getYYYY(Fecha,YYYY),
   esFecha(Fecha), Usuario = [Name,Password,[],[],Fecha,"offline"].
%    getDiasMes(Fecha,MaximoDias),getDD(Fecha,Dia), MaximoDias>= Dia.
%
%predicado que Verifica si un usuario es valido
%Dom: tda User
%Rec: boolean.
validaUser([Name,Password,Amigos,Perfil,Fecha,Estado]):-length([Name,Password,Amigos,Perfil,Fecha,Estado],L), L == 6,
   string_not_empty(Name),string_not_empty(Password),is_list(Amigos),is_list(Perfil),esFecha(Fecha), Estado == "offline",!;
   Estado == "online".


%predicado que Comprueba que un string no sea vacio
%DOM:string
%REC:boolea
%Tipo de meta: Primaria
string_not_empty(String):- string_length(String,Cont) , Cont\=0.
/*#########################SELECTORES#######################*/

%predicado que Obtiene el Nombre del tda Usuario
%DOM: tda user
%REC: string
%Tipo de meta: Primaria
getNameUser(User,Name):-validaUser(User), nth0(0,User,Name).

%Obtiene la password del tda Usuario
%DOM: tda User
%REC: string
%Tipo de meta: Primaria
getPasswordUser(User,Password):-validaUser(User), nth0(1,User,Password).


%predicado que Obtiene la lista de amigos del tda Usuario
%DOM: tda User
%REC: list tda user
%Tipo de meta: Primaria
getAmigosUser(User,Amigos):-validaUser(User), nth0(2,User,Amigos).
%[Name,Password,Amigos,Perfil,Fecha,Estado]
%

%predicado que Obtiene el perfil del tda Usuario
%DOM: tda User
%REC: lista tda post
%Tipo de meta: Primaria
getPerfilUser(User,Perfil):-validaUser(User), nth0(3,User,Perfil).


%predicado que Obtiene la fecha de registro del tda Usuario
%DOM: tda User
%REC: tda fecha
%Tipo de meta: Primaria
getFechaUser(User,Fecha):-validaUser(User), nth0(4,User,Fecha).

% predicado que Obtiene l estado del tda Usuario,el cual apunta si es
% que está offline o no
%DOM: tda User
%REC: string
%Tipo de meta: Primaria
getEstadoUser(User,Estado):-validaUser(User), nth0(5,User,Estado).

/*########################### Funciones Modificadoras#######*/

%predicado que Se encarga de cambiar el estado de un tda usuario
%DOM: tda user, string
%REC: tda user
%Tipo de meta: Primaria
setEstadoUser(User,NewEstado,NewUser):- validaUser(User),string(NewEstado),setElemLista(5,User,NewEstado,NewUser).

% Predicado que se encarga de cambiar el perfil de un usaurio por uno
% nuevo
% DOM: tda User x list tda post x Output
% REC: tda User
% Tipo de meta: Primaria
setPerfilUser(User,NewPerfil,NewUser):-validaUser(User),setElemLista(3,User,NewPerfil,NewUser).


% predicado que Se encarga de cambiar la lista de amigos de un tda
% usuario
%DOM: tda user, lista de amigos REC: tda user se agregó el "!"
% para que el validaListaUser no retornara un F
% REC: lista tda User
% Tipo de meta: Primaria
setListaAmigosUser(User,NewLista,NewUser):- validaUser(User),validaListaUser(NewLista,AUX),!,setElemLista(2,User,AUX,NewUser).




%predicado que Cambia un elemento de una lista dado un indicador asi entregando una
% nueva lista
%DOM: int x list x cualquier elemento x Output
%REC: List
%Tipo de meta: Secundaria
setElemLista(Index,Lista,NewElem,ListaOut):-Index >= 0,length(Lista,L),L>0,Index =< L, nth0(Index,Lista,_,AUX),nth0(Index, ListaOut,NewElem,AUX).











/*TDA SOCIALNETWORK*/
%Representacion
%[NombreRedSocial,Fecha,ListaUser,ListaPost]



% predicado que Verifica si un string corresponde a los nombre de una red
% social
%dom: string
%rec: boolean
%Tipo de meta: SEcundaria
checkNameSN(Name):- Name == "facebook",!;
                    Name == "fb",!;
                    Name == "instagram",!;
                    Name == "ig",!;
                    Name == "twitter",!;
                    Name == "tw".



%predicado que Permite construir una tda socialnetwork
%DOM: string x tda fecha x Output
%Rec: tda socialNetwork
%Tipo de meta: Primaria
socialNetwork(Name,Date,SOut):- string(Name), esFecha(Date), checkNameSN(Name), SOut = [Name,Date,[],[]].

%Pertenencia
%predicado que Permite valida que un elemento sea un socialnetwork
%dom: elemento
%rec: boolean
%Tipo de meta: Primaria
validaSocialNetwork([Name,Date,ListaUser,ListaPost]):-length([Name,Date,ListaUser,ListaPost],L), L==4,checkNameSN(Name),
                                                       esFecha(Date),is_list(ListaUser),is_list(ListaPost).

/*############################Selectores Tda SocialNewtwork####################*/
%predicado que Entrega el nombre un tda socialnetwork
%DOM: tda SN
%REC: string
%Tipo de meta: Primaria
getNameSN(SN,Name):-validaSocialNetwork(SN), nth0(0,SN,Name).

%predicado que Entrega el nombre un tda socialnetwork
%DOM: tda SN
%REC: tda Fecha
%Tipo de meta: Primaria
getDateSN(SN,Fecha):-validaSocialNetwork(SN), nth0(1,SN,Fecha1), esFecha(Fecha1),Fecha1 = Fecha.

%predicado que Entrega lista User un tda socialnetwork
%DOM: tda SN
% REC: list
% Tipo de meta: Primaria
getUsersSN(SN,ListaUser):-validaSocialNetwork(SN), nth0(2,SN,ListaUser).

%Predicado que entrega lista de post de un tda socialnetwork
%DOM: tda sn
%REC: list
%Tipo de meta: Primaria
getPerfilSN(SN,Perfil):-validaSocialNetwork(SN),nth0(3,SN,Perfil).

%predicado que Cambia la lista de Usuarios por otra nueva
%DOM: tda socialnetork x lista de elementos x output
%REC: tda socialnetwork
%Tipo de meta: Primaria
setUsersSN(SN,NewUsers,NewSN):-validaSocialNetwork(SN),setElemLista(2,SN,NewUsers,NewSN).

%Predicado que cambia la lista de post por otra nueva
%DOM: tda sn x lista elementos x output
%REC: tda sn
%Tipo de meta: Primaria
setPerfilSN(SN,NewPerfil,NewSN):-validaSocialNetwork(SN),setElemLista(3,SN,NewPerfil,NewSN).

%Predicado que dado un id obtiene el post de un SN
%DOM: tda sn x int x output
%REC: tda post
%Tipo de meta: Primaria
getPostSN(SN,ID,Post):-validaSocialNetwork(SN),getPerfilSN(SN,Perfil),nth0(ID,Perfil,Post).

/*TDA POST*/
%Representacion
% [nombreUsuario,Fecha,ContenidoPublicacion,ListaUsuariosDestinatarios,Id]
%
%

% predicado que Contruye el tda Post
%DOM: tda user x tda date x string x lista tda user x int x Output
%REC: tda post
%Tipo de meta: Primaria
post(Usuario,Date,Contenido,ListaUsers,PostID,Post):-validaUser(Usuario),esFecha(Date),
                                                     validaContenido(Contenido),is_list(ListaUsers),/*verificarListaUser*/number(PostID),
                                                     Post = [Usuario,Date,Contenido,ListaUsers,PostID].


% predicado que Comprueba si un elemento cualquiera es tda post es valido
% o no
%dom: cualquier elemento
%rec: boolean
%Tipo de meta: Primaria
validaPost([Usuario,Date,Contenido,ListaUsers,PostID]):- validaUser(Usuario),esFecha(Date),
                                                     validaContenido(Contenido),is_list(ListaUsers),validaListaUser(ListaUsers,Aux_Users),!,is_list(Aux_Users),number(PostID).

/*##########################SELECTORES TDA POST##################################*/
%predicado que Selecciona el usuario creador del tda post
%DOM: tda Post x output
%REC: tda user
%Tipo de meta: Primaria
getUserPost(Post,User):-validaPost(Post),nth0(0,Post,User).

%predicado que Selecciona la fecha de creacion del post
%DOM: tda post x output
%REC: tda fecha
%Tipo de meta: Primaria
getDatePost(Post,Date):-validaPost(Post),nth0(1,Post,Date).

%Selecciona el contenido del post
%DOM: tda post x output
%REC: string
%Tipo de meta: Primaria
getContenidoPost(Post,Contenido):-validaPost(Post),nth0(2,Post,Contenido).

%predicado que Selecciona a lista de destinarios del post
%DOM: tda post x output
%REC: lista tda user
%Tipo de meta: Primaria
getUsersPost(Post,Users):-validaPost(Post),nth0(3,Post,Users).

%predicado que Selecciona el identificador del post
%DOM: tda post x output
%REC: int
%Tipo de meta: Primaria
getIDPost(Post,IDPOST):-validaPost(Post),nth0(4,Post,IDPOST).


%predicado que Valida el contenido de una publicacion
%dom: contenido
%rec: boolean
%Tipo de meta: Secundaria
validaContenido(Contenido):-string(Contenido),string_not_empty(Contenido).

%predicado que Valida que una lista contenga solo elementos Tda Usuarios validos y
% devuelve la lista solo con usuarios correctos
%dom: lista usuarios,output
%rec: lista usuarios
%Tipo de meta: Secundaria
validaListaUser([],[]):-!.
%validaListaUser(L,L).
validaListaUser([User|ColaUser],[User|Result]):-validaUser(User),validaListaUser(ColaUser,Result).
% validaListaUser([User|ColaUser],[User1|ColaUser]):-validaUser(User),validaListaUser(ColaUser,[User1,User|ColaUser]).
validaListaUser([User|ColaUser],Out):-not(validaUser(User)),validaListaUser(ColaUser,Out).



sn(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []]).

snConBobby(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []]).

%predicado que Se encarga de registrar un usuaio en tda SocialNetwork
%DOM: tda SocialNetwork x Tda Fecha x string x string x Output
%REC: tda SocialNetwork
%Tipo de meta: Primaria
socialNetworkRegister(Sn1,_,_,_,Sn2):-Sn1 == Sn2,!.
socialNetworkRegister(Sn1,Fecha,Username,Password,Sn2):-validaSocialNetwork(Sn1),esFecha(Fecha),verificaUserName(Username),
                                                         verificaPassword(Password),not(checkRegister(Username,Sn1)),
                                                         user(Username,Password,Fecha,User),getUsersSN(Sn1,Users),
                                                         append_final(Users,User,NewUsers),setUsersSN(Sn1,NewUsers,Sn2).%,Sn2 = OUT.


/*
 * Prueba que retorna true
socialNetworkRegister(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []],[04,06,1992],"BobbyParra","bobby123",["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []]).

* Prueba que retorna el Sn2
socialNetworkRegister(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []],[04,06,1992],"BobbyParra","bobby123",SN).*/



% predicado que Verifica que un nombre de Usuario sea no vacio y mayor a
% 6
%DOM: string
%REC: boolean
%Tipo de meta: Secundaria
verificaUserName(Username):-string_not_empty(Username),string_length(Username,Cont),Cont >=6.

% Verifica que una password sea no vacia, mayor a 8 y que contenga al
% menos 2 numeros
%DOM: string
%REC: boolean
%Tipo de meta: Secundaria
verificaPassword(Password):-string_not_empty(Password),string_length(Password,Cont),Cont >=8,
                           lista_numeros(Lista_Numeros),tiene_numeros(Password,Lista_Numeros).

%predicado que Verifica si un Username esta en la lista de usuarios de socialnetwork,
%DOM: string x tda socialnetwork
%REC: boolean
%Tipo de meta: Secundaria
checkRegister(Username,SN):-getUsersSN(SN,Users),getListaNames(Users,ListaString),member(Username,ListaString),!.

%predicado que Entrega los nombres (strings) de una lista de usuarios
%DOM: lista tda user x output
%REC: lista string
%Tipo de meta: Secundaria
getListaNames([],[]):-true,!.
getListaNames([X|Xs],[Name|Ys]):- getNameUser(X,Name),getListaNames(Xs,Ys).


%es una lista de numeros en Ascii
lista_numeros([49, 50, 51, 52, 53, 54, 55, 56, 57]).

%predicado que Comprueba que un string tenga al menos 2 numeros
%dom: string x lista numeros en ascii
%rec: boolean
%Tipo de meta: Secundaria
tiene_numeros(String,Lista_Numeros):- string_to_list(String,List),comparten_elementos(List,Lista_Numeros,0,Cont),Cont >= 2.

% predicado que Entrega cuantas veces un elemento está contenido en una
% lista
%DOM: Elemento x Lista x 0 x Output
%REc: int
%Tipo de meta: Secundaria
existe_en_lista(_,[],Out,Out):-true,!.
existe_en_lista(Cabeza1,[Cabeza2|Cola2],CONT,AUX):- Cabeza1 == Cabeza2,Cont1 is CONT+1,existe_en_lista(Cabeza1,Cola2,Cont1,AUX).
existe_en_lista(Cabeza1,[Cabeza2|Cola2],CONT,AUX):- Cabeza1\==Cabeza2,existe_en_lista(Cabeza1,Cola2,CONT,AUX).


% predicado que Entrega cuantos elementos en comun tiene una lista con
% otra
%DOM: Lista x Lista x 0 x Output REC: int
%Tipo de meta: Secundaria
comparten_elementos([],Lista,AUX,Out):-length(Lista,L), L\=0, Out is 0+AUX,!.
comparten_elementos([Cabeza1|Cola1],[Cabeza2|Cola2],CONT,AUX):-existe_en_lista(Cabeza1,[Cabeza2|Cola2],0,CONT1),!,CONTAUX is CONT1+CONT, comparten_elementos(Cola1,[Cabeza2|Cola2],CONTAUX,AUX).




%predicado que Agrega un elemento al final de una lista
%DOM: lista x elemento x output
%REC: lista
%Tipo de meta: Secundaria
append_final([],Elem,[Elem]).
append_final([Cabeza1|Cola1],Elem,[Cabeza1|Cola2]):-append_final(Cola1,Elem,Cola2).



%Predicado que valida el inicio de sesión de un usuario
%DOM: tda socialnetwork, string,string, output
%REC: tda socialnetwork
%Tipo de meta: Primaria
socialNetworkLogin(Sn1,Username,Password,Sn2):- validaSocialNetwork(Sn1),verificaUserName(Username),verificaPassword(Password),
                                                %turnOffSN(Sn1,SNaux),
                                                getUsersSN(Sn1,ListaUser),verificaLogin(Username,Password,ListaUser),
                                                getUserConNombre(Username,ListaUser,User),setEstadoUser(User,"online",User2),
                                                getIndex(User,ListaUser,0,Index),setElemLista(Index,ListaUser,User2,NewListaUser),
                                                setUsersSN(Sn1,NewListaUser,Sn2).

/*
socialNetworkLogin(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []],"BenjaminParra","benja123",SN)socialNetworkLogin(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []],"ChiloParra","chilo123",SN)
 socialNetworkLogin(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []],"BobbyParra","bobby123",SN)*/
/*
setUserListaUser(User,[User|Xs],Aux,Cont):-Aux == Cont,setUserListaUser(User,Xs,Aux,Cont).
setUserListaUser(User,[X|Xs],Aux,Cont):-Aux \= Cont, Aux1 is
Aux+1,setUserListaUser(User,[X|Xs],Aux1,Cont).*/
%setEstadoUser(User,"online",NewUser)

% predicado que verifica si el Username y password coinciden con los de
% un usuario registrado en la lista de Usuarios
%DOM: string x string x lista tda user
%REC: boolean
%Tipo de meta: Secundaria
verificaLogin(_,_,[]):-false,!.
verificaLogin(Username,Password,[User|ColaListaUser]):- getNameUser(User,Name),Name == Username,getPasswordUser(User,Pass),
                                                        Pass == Password,!;
                                                        verificaLogin(Username,Password,ColaListaUser).

% predicado que entrega un tda user que pertenece a una lista tda user
% dado un nombre
%Dom: string, lista tda user,
%Rec: tda user
%Tipo de meta: Secundaria
getUserConNombre(_,[],_):-false,!.
getUserConNombre(UserName,[User|_],Usuario):- validaUser(User),getNameUser(User,Name),Name == UserName,Usuario = User,!.
getUserConNombre(UserName,[User|ColaListaUser],Usuario):- validaUser(User),getNameUser(User,Name),Name \= UserName
                                                         ,getUserConNombre(UserName,ColaListaUser,Usuario).


% predicado que entrega en que posicion se encuentra un elem (siempre
% arroja el primer encuentro).
% DOM: elememto,listaElem,0,Output
% REC: int
% Tipo de meta: Secundaria
getIndex(_,[],_,_):-false,!.
getIndex(Elem,[X|_],Aux,Cont):-Elem == X, Cont = Aux,!.
getIndex(Elem,[X|Xs],Aux,Cont):-Elem \= X, Aux1 is Aux + 1, getIndex(Elem,Xs,Aux1,Cont).


%Predicado que permite al usuario con sesion activa seguir a un usuario
%DOM: tda socialNetwork,string,OutPut
%REC: tda socialNetwork
%Tipo de meta: Primaria
socialNetworkFollow(Sn1,Username,Sn2):- getUsersSN(Sn1,ListaUser),getUserOnline(ListaUser,UserOn),
                                        getNameUser(UserOn,NameON),Username \= NameON,
                                        getUsersSN(Sn1,ListaUser),getUserOnline(ListaUser,UserOn),
                                        validaSocialNetwork(Sn1),verificaUserName(Username),registradoEnSn(Username,Sn1),
                                        getUserConNombre(Username,ListaUser,Usuario),getUsersSN(Sn1,ListaUser),
                                        getAmigosUser(UserOn,AmigosUserOn),
                                        not(estaEnLista(Usuario,AmigosUserOn)),append_final(AmigosUserOn,Usuario,NewAmigosUserON),
                                        setListaAmigosUser(UserOn,NewAmigosUserON,NewUserON),
                                        setEstadoUser(NewUserON,"offline",NewUserOFF),getIndex(UserOn,ListaUser,0,CONT),
                                        setElemLista(CONT,ListaUser,NewUserOFF,NewListaUser),setUsersSN(Sn1,NewListaUser,Sn2),!.

/*
socialNetworkFollow(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], [], [8, 7, 1997], "online"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []],"BobbyParra",SN),getUsersSN(SN,L),nth0(0,L,EM),getAmigosUser(EM,AMIGOS).
                                        */
/*
socialNetworkFollow(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "online"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []],"BobbyParra",SN),getUsersSN(SN,USER).*/


% Predicado que veririfica que un string este registrado en la
% socialnetwork
%DOM: string x tda socialnetwork
%REC: boolean
%Tipo de meta: Secundaria
registradoEnSn(Username,Sn1):-getUsersSN(Sn1,ListaUser),getUserConNombre(Username,ListaUser,Usuario),
                               estaEnLista(Usuario,ListaUser).



%Predicado que dado una lista de User entrega el que esta online
%DOM: Lista tda User x Output
%REC: tda User
%Tipo de meta: Secundaria
getUserOnline([],_):-false,!.
getUserOnline([X|_],User):-getEstadoUser(X,Estado),Estado == "online", User = X,!.
getUserOnline([X|Xs],User):-getEstadoUser(X,Estado),Estado == "offline", getUserOnline(Xs,User).

%Predicado que verifica si un usuario está online o no
%DOM: TDA user
%REC: Boolean
%Tipo de meta: Primaria
estaOnline(User):-getEstadoUser(User,Estado),Estado=="online".


%Predicado que verifica si existe un usuario online en una lista de user
%DOM: Lista tda user
%REC: boolean
%Tipo de meta: Secundaria
existeUserOnlineLista([]):-false,!.
existeUserOnlineLista([X|_]):-estaOnline(X),!.
existeUserOnlineLista([X|Xs]):-not(estaOnline(X)),existeUserOnlineLista(Xs).

% Predicado que verifica si existe un usuario online en una tda
% socialnetwork
%DOM: tda socialnetwork
%REC: boolean
%Tipo de meta: Primaria
existeUserOnline(SN1):-getUsersSN(SN1,Users),existeUserOnlineLista(Users).

% Predicado que verifica si un elemento esta en Lista, al momento de
% encontrarlo retorna un true y si no existe en Lista false
%DOM: elemento x listaElementos
%REC: boolean
%Tipo de meta: Secundaria
estaEnLista(Elem,[Elem|_]):-!.
estaEnLista(Elem,[_|Cola]):-estaEnLista(Elem,Cola).

% Predicado que entrega la lista de tda user en base a una lista de
% usuarios registrados en el socialnetwork
% DOM: tda sn x lista string x output
% REC: lista tda user
% Tipo de meta: Secundaria
convierteListaUser(_,[],[]):-true,!.
convierteListaUser(SN,[UserString|ColaString],[UserTda|ColaTda]):-getUsersSN(SN,ListaUser),
                                                                  getUserConNombre(UserString,ListaUser,UserTda),
                                                                  convierteListaUser(SN,ColaString,ColaTda).

% Predicado que verifica que un user(Amigo) este en la lista del User
% DOM: tda user x tda user
% REC: boolean
% Tipo de meta: Secundaria
esAmigo(User,Amigo):-validaUser(User),validaUser(Amigo),getAmigosUser(User,Amigos),estaEnLista(Amigo,Amigos).

% Predicado que en base a una lista de TDA User obtiene una lista de los
% nombres en string
% DOM: Lista TDA User
% REC: Lista string
% Tipo de meta: Secundaria
listaTdaUserToString([],[]):-true,!.
listaTdaUserToString([User|ColaUsers],[Name|Xs]):-getNameUser(User,Name),listaTdaUserToString(ColaUsers,Xs).


%Predicado que compara dos listas y solo deja sus elementos en comun
%DOM: lista x lista x output
%REC: lista
%Tipo de meta: Secundaria
dejaIguales([],_,[]).
dejaIguales([X|Xs],Lista,[X|OutCola]):-estaEnLista(X,Lista),
                                       dejaIguales(Xs,Lista,OutCola).
dejaIguales([X|Xs],Lista,OutCola):- not(estaEnLista(X,Lista)),
                                    dejaIguales(Xs,Lista,OutCola).


%Predicado que elimina el usuario online de una lista usuarios
%DOM: Lista tda User x output
%REC: lista tda user
%Tipo de meta: Secundaria
eliminaOnline([X|Xs],[X|Ys]):-getEstadoUser(X,E),E=="offline",eliminaOnline(Xs,Ys).

eliminaOnline([X|Xs],Y):-getEstadoUser(X,E),E=="online",eliminaOnline(Xs,Y).

eliminaOnline([],[]):-!.



% Predicado que realiza la publicacion de un usuario Online a una lista
% usuarios que son sus amigos o en su propio perfil
% DOM: tda socialnetwork x tda fecha x string x lista string x Output
% REC: tda socialnetwork
% Tipo de meta: Primaria
socialNetworkPost(Sn1,Fecha,Texto,ListaUserNameDest,Sn2):- length(ListaUserNameDest,L),L==0,
                                                           publicaEnUno(Sn1,Fecha,Texto,Sn2),!.
socialNetworkPost(Sn1,Fecha,Texto,ListaUserNameDest,Sn2):- length(ListaUserNameDest,L),L\=0,
                                                           aplicaPublicaEnMuroUser(Sn1,Fecha,Texto,ListaUserNameDest,SnOut),
                                                           turnOffSN(SnOut,Sn2).



%Predicado que realiza la publicación en el propio perfil
%DOM: tda sn x tda fecha x string x output
%REC: tda sn
%Tipo de meta: Secundaria
publicaEnUno(Sn1,Fecha,Texto,SnOut):-validaSocialNetwork(Sn1),esFecha(Fecha),string_not_empty(Texto),getUsersSN(Sn1,ListaUser),
                                     getUserOnline(ListaUser,UserOnline),getPerfilUser(UserOnline,Perfil),length(Perfil,Largo),
                                     PostID is Largo + 0,post(UserOnline,Fecha,Texto,[],PostID,Post),
                                     getPerfilSN(Sn1,PerfilSN),length(PerfilSN,Largo2),PostIdSn is Largo2+0,
                                     post(UserOnline,Fecha,Texto,[],PostIdSn,PostSN),
                                     agregaPostUser(UserOnline,Post,UserPost),agregaPostSN(Sn1,PostSN,SNAUX),
                                     getIndex(UserOnline,ListaUser,0,Index),setEstadoUser(UserPost,"offline",UserOff),
                                     setElemLista(Index,ListaUser,UserOff,NewListaUser),setUsersSN(SNAUX,NewListaUser,SnOut),!.
                                     %getIndex(User,ListaUser,0,Index),setElemLista(Index,ListaUser,User2,NewListaUser)


/*
 ig(Sn1),getUsersSN(Sn1,ListaUser),getUserOnline(ListaUser,UserOnline),getPerfilUser(UserOnline,Perfil),length(Perfil,Largo),PostID is Largo+1,post(UserOnline,[2,4,2021],"Hola",[],PostID,Post),getPerfilSN(Sn1,PerfilSN),length(PerfilSN,Largo2),PostIdSn is Largo2+1,post(UserOnline,[2,4,2021],"Hola",[],PostIdSn,PostSn),agregaPostUser(UserOnline,Post,UserPost),agregaPostSN(Sn1,PostSn,SNAUX),getIndex(UserOnline,ListaUser,0,Index),setEstadoUser(UserPost,"Offline",UserOff),setElemLista(Index,ListaUser,UserOff,NewListaUser),setUsersSN(SNAUX,NewListaUser,Snout).

                                     */

%Predicado que realiza la publicacion en el perfil de un amigo
%DOM: tda sn x tda fecha x string x string x output
%REC: tda sn
%Tipo de meta: Secundaria
/*
publicaEnMuroUser(Sn1,Fecha,Texto,UserReceptor,SnOut):-validaSocialNetwork(Sn1),esFecha(Fecha),string_not_empty(Texto),
                                               getUsersSN(Sn1,ListaUser),getUserOnline(ListaUser,UserOnline),
                                               getUserConNombre(UserReceptor,ListaUser,ReceptorTda),esAmigo(UserOnline,ReceptorTda),
                                               getPerfilUser(UserOnline,Perfil),length(Perfil,Largo),PostID is Largo + 0,
                                               post(UserOnline,Fecha,Texto,[ReceptorTda],PostID,Post),
                                               getPerfilSN(Sn1,PerfilSN),length(PerfilSN,Largo2),PostIdSn is Largo2+0,
                                               post(UserOnline,Fecha,Texto,[ReceptorTda],PostIdSn,PostSN),
                                               agregaPostUser(UserOnline,Post,UserPost),agregaPostSN(Sn1,PostSN,SNAUX),
                                               getIndex(UserOnline,ListaUser,0,Index),%setEstadoUser(UserPost,"offline",UserOff),
                                               setElemLista(Index,ListaUser,UserPost,NewListaUser),
                                               setUsersSN(SNAUX,NewListaUser,SnOut).*/

publicaEnMuroUser(Sn1,Fecha,Texto,UserReceptor,SnOut):-validaSocialNetwork(Sn1),esFecha(Fecha),string_not_empty(Texto),
                                               getUsersSN(Sn1,ListaUser),getUserOnline(ListaUser,UserOnline),
                                               getUserConNombre(UserReceptor,ListaUser,ReceptorTda),
                                               getAmigosUser(UserOnline,Amigos),getListaNames(Amigos,AmigosStr),member(UserReceptor,AmigosStr),!,
                                               getPerfilUser(UserOnline,Perfil),length(Perfil,Largo),PostID is Largo + 0,
                                               post(UserOnline,Fecha,Texto,[ReceptorTda],PostID,Post),
                                               getPerfilSN(Sn1,PerfilSN),length(PerfilSN,Largo2),PostIdSn is Largo2+0,
                                               post(UserOnline,Fecha,Texto,[ReceptorTda],PostIdSn,PostSN),
                                               agregaPostUser(UserOnline,Post,UserPost),agregaPostSN(Sn1,PostSN,SNAUX),
                                               getIndex(UserOnline,ListaUser,0,Index),%setEstadoUser(UserPost,"offline",UserOff),
                                               setElemLista(Index,ListaUser,UserPost,NewListaUser),
                                               setUsersSN(SNAUX,NewListaUser,SnOut).

/*ig(Sn1),publicaEnMuroUser(Sn1,[08,06,2021],"Hola compa","BobbyParra",SnOut),getPerfilSN(SnOut,Perfil),nth0(0,Perfil,Post),traducePost(Post,PostStr),write(PostStr).*/


% Predicado que se encarga de aplicar el publicaEnMuroUser a una lista
% de string
%DOM: tda sn x tda fecha x string x lista string x output
%REC: tda sn
%Tipo de meta: Secundaria
aplicaPublicaEnMuroUser(SnOut,_,_,[],SnOut):-!.
aplicaPublicaEnMuroUser(Sn1,Fecha,Texto,[X|Xs],SnOut):-validaSocialNetwork(Sn1),esFecha(Fecha),string_not_empty(Texto),
                                                      publicaEnMuroUser(Sn1,Fecha,Texto,X,SNAUX),
                                                      aplicaPublicaEnMuroUser(SNAUX,Fecha,Texto,Xs,SnOut).


/* ig(Sn1),aplicaPublicaEnMuroUser(Sn1,[8,6,2021],"Hola Amigos",["ChiloParra","BobbyParra"],Snout),!,getPerfilSN(Snout,Perfil),nth0(0,Perfil,User1),nth0(1,Perfil,User2),traducePost(User1,STR1),traducePost(User2,STR2),write(STR1),write(STR2).*/


/* ig(Sn1),aplicaPublicaEnMuroUser(Sn1,[8,6,2021],"Hola Amigos",["ChiloParra","BobbyParra"],Snout),getUsersSN(Snout,Users),nth0(0,Users,User1),getPerfilUser(User1,PerfilUser),nth0(0,PerfilUser,Post),traducePost(Post,STR),write(STR),nth0(1,PerfilUser,Post1),traducePost(Post1,STR1),write(STR1).
*/

%Predicado que agrega un post al perfil del usuario
%DOM: tda user x tda post x output
%REC: tda user
%Tipo de meta: Secundaria
agregaPostUser(User,Post,UserPost):-validaUser(User), getPerfilUser(User,Perfil),append_final(Perfil,Post,NewPerfil),
                                    setPerfilUser(User,NewPerfil,UserPost).

%Predicado que agrega un post a la lista de post del tda socialnetwork
%DOM: tda sn x tda post x output
%REC: tda sn
%Tipo de meta: Secundaria
agregaPostSN(Sn,Post,SnOut):-validaSocialNetwork(Sn),getPerfilSN(Sn,ListaPost),append_final(ListaPost,Post,NewListaPost),setPerfilSN(Sn,NewListaPost,SnOut).

% Predicado auxiliar de predicado post, entrega la lista validada de a
% que usuario se le puede publicar
% DOM: tda SoccialNetwork x Lista string x output
% REC: Lista tda user
% Tipo de meta: Secundaria
dejaListaDest(Sn1,ListaUserNameDest,ListaOut):-getUsersSN(Sn1,ListaUser),listaTdaUserToString(ListaUser,ListaUserString),
                                                          dejaIguales(ListaUserNameDest,ListaUserString,DestRegistrados),!,
                                                          convierteListaUser(Sn1,DestRegistrados,DestRegistradosTda),
                                                          eliminaOnline(DestRegistradosTda,ListaOut),%!,
                                                        % getUserOnline(ListaUser,UserOnline),getAmigosUser(UserOnline,Amigos),
                                                         %dejaIguales(DestTdaSinOnline,Amigos,Lista),!,sort(Lista,ListaOut),
                                                         !.



ig(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], [], [8, 7, 1997], "online"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []]).

destino(["BobbyParra","ChiloParr","BobbyParra","caca","BenjaminParra","ChiloParra","ChiloParra"]).


%Predicado que transforma el tda fecha a una representacion en un string
%DOM: tda fecha x Output
%REC: string
%Tipo de meta: Secundaria
traduceFecha(Fecha,FechaString):-getDD(Fecha,DD),getMM(Fecha,MM),getYYYY(Fecha,YY),
                                atomics_to_string([DD, MM, YY], '/', FechaString) .


% Predicado que transforma el tda post o tda postshare a una
% representacion en un string
%DOM: tda postShare o tda post
%REC: String
%Tipo de meta: Secundaria
traducePost(Post,PostStr):-getUserPost(Post,NamePost),getNameUser(NamePost,Name),getDatePost(Post,FechaPost),
                           getContenidoPost(Post,Contenido),getUsersPost(Post,ListaUserPost),length(ListaUserPost,L),
                           L==0,
                           getIDPost(Post,PostID),
                           traduceFecha(FechaPost,FechaStr),
                           atomics_to_string(["ID:",PostID,"\n",
                                              "el día",FechaStr,Name,"publicó:","\n",
                                              '"',Contenido,'"',"\n",
                                              "Destinatarios: Su Perfil","\n"],' ',PostStr),!.


traducePost(Post,PostStr):-getUserPost(Post,NamePost),getNameUser(NamePost,Name),getDatePost(Post,FechaPost),
                           getContenidoPost(Post,Contenido),getUsersPost(Post,ListaUserPost),length(ListaUserPost,L),
                           L\=0,
                           getIDPost(Post,PostID),
                           traduceFecha(FechaPost,FechaStr),
                           listaTdaUserToString(ListaUserPost,ListaUserStr),
                           nth0(0,ListaUserStr,UserDest),
                           atomics_to_string(["ID:",PostID,"\n",
                                              "el día",FechaStr,Name,"publicó:","\n",
                                              '"',Contenido,'"',"\n",
                                              "Destinatarios:",UserDest,"\n"],' ',PostStr),!.


traducePost(Post,PostStr):-length(Post,L),L == 4,getUserPostShare(Post,NamePostShare),getNameUser(NamePostShare,Name),
                          getPostPostShare(Post,PostPostShare),postEnPalabra(PostPostShare,PostShareStr),
                          getFechaPostShare(Post,FechaPostShare),traduceFecha(FechaPostShare,FechaStr),
                          getUsersPostShare(Post,UsersPostShare),listaTdaUserToString(UsersPostShare,UserStr),
                          nth0(0,UserStr,UserDest),
                          atomics_to_string(["El día",FechaStr,Name,"compartió con",UserDest,"la publicación con"
                                         , PostShareStr,"\n"],' ', PostStr),!.


traducePost(Post,PostStr):-length(Post,L),L == 4,getUserPostShare(Post,NamePostShare),getNameUser(NamePostShare,Name),
                          getPostPostShare(Post,PostPostShare),postEnPalabra(PostPostShare,PostShareStr),
                          getFechaPostShare(Post,FechaPostShare),traduceFecha(FechaPostShare,FechaStr),
                          getUsersPostShare(Post,UsersPostShare),length(UsersPostShare,L2),L2==0,

                          atomics_to_string(["El día",FechaStr,Name,"compartió en su perfil","la publicación con",
                                            PostShareStr,"\n"],' ', PostStr),!.


% Predicado que dado un tda usuario obtiene la lista de string de los
% nombres de sus amigos
% DOM: tda user x output
% REC: lista string
% Tipo de meta: Secundaria
traduceAmigos(User,ListaAmigos):-validaUser(User),getAmigosUser(User,ListaAux),listaTdaUserToString(ListaAux,ListaAmigos).

% Predicado que escribe la lista de amigos de un tda usuario en el
% socialnetworkToString
% DOM: lista string
% REC: string
% Tipo de meta: Secundaria
traduceAmigosString([],String,String).
traduceAmigosString([X|Xs],StringAux,String):-atomics_to_string([StringAux,"\n",X,"\n"],' ',Str),
                                               traduceAmigosString(Xs,Str,String).

%Predicado que convierte en un string el tda usuarios
%DOM: tda usuario x output
%REC: String
%Tipo de meta: Secundaria
traduceUser(User,UserString):-validaUser(User),getAmigosUser(User,Amigos),getPerfilUser(User,Perfil),getFechaUser(User,Fecha),
                              traduceFecha(Fecha,FechaStr),getNameUser(User,Name),
                              length(Amigos,LAmigos),LAmigos==0,length(Perfil,LPerfil),LPerfil==0,
                              atomics_to_string([Name,"registrado el",FechaStr,"\n",
                                                 "no tiene amigos","\n"],' ',UserString),!.

traduceUser(User,UserString):-validaUser(User),getFechaUser(User,Fecha),
                              traduceFecha(Fecha,FechaStr),getNameUser(User,Name),
                              atomics_to_string([Name,"registrado el",FechaStr,"\n",
                                                 "No sigue a nadie","\n"],' ',UserString),!.


traduceUser(User,UserString):-validaUser(User),getAmigosUser(User,Amigos),getPerfilUser(User,Perfil),getFechaUser(User,Fecha),
                              traduceFecha(Fecha,FechaStr),getNameUser(User,Name),
                              traduceAmigos(User,ListaAmigos),traduceAmigosString(ListaAmigos,"",AmigosString),
                              length(Amigos,LAmigos),LAmigos\=0,length(Perfil,LPerfil),LPerfil==0,
                              atomics_to_string([Name,"registrado el",FechaStr,"\n",
                                                 "sigue a:","\n",AmigosString,"\n"],' ',UserString),!.


traduceUser(User,UserString):-validaUser(User),getAmigosUser(User,Amigos),getPerfilUser(User,Perfil),getFechaUser(User,Fecha),
                              traduceFecha(Fecha,FechaStr),
                              length(Amigos,LAmigos),LAmigos\=0,length(Perfil,LPerfil),LPerfil\=0,getNameUser(User,Name),
                              traduceAmigos(User,ListaAmigos),traduceAmigosString(ListaAmigos,"",AmigosString),


                              atomics_to_string([Name," registrado el",FechaStr,"\n","sigue a:","\n",AmigosString,"\n"
                                                ],' ',UserString),!.

/*
ig(Sn1),publicaEnUno(Sn1,[6,6,2021],"Comelo",SN),socialNetworkLogin(SN,"BenjaminParra","benja123",SN2),socialNetworkShare(SN2,[1,2,2021],0,["BobbyParra","caca","ChiloParra"],SN3),socialNetworkLogin(SN3,"BenjaminParra","benja123",SN4),socialNetworkRegister(SN4,[6,8,2021],"BorisParra","boris123",SN5),socialNetworkLogin(SN5,"BorisParra","boris123",SN6),socialNetworkPost(SN6,[10,6,2021],"Primer post Boris",[],SN7),getUsersSN(SN7,Users),nth0(3,Users,User),traduceUser(User,STR),write(STR).                              */

% Predicado que transforma una lista de tda User a un string
% DOM: Lista tda user x output
% REC: String
% Tipo de meta: Secundaria
traduceListaUser([],String,String).
traduceListaUser([User|UserCola],String,StrOut):-traduceUser(User,UserStr),
                                                 atomics_to_string([String,UserStr,"\n",
                                                  "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%","\n"],Str),
                                                 traduceListaUser(UserCola,Str,StrOut).
/*
traduceListaUserOff([],String,String).
traduceListaUserOff([User|UserCola],Users,String,StrOut):- getNameUser(User,Name),getSeguidores(User,Users,Seguidores),
                                                           traduceSeguidores(Seguidores,"",SeguidoresStr),
                                                           atomics_to_string([String,Name,"\n",
                                                                              "     Sigue a:","\n",
                                                                              SeguidoresStr,"\n"],' ',STRAUX),
                                                           traduceListaUserOff(UserCola,Users,STRAUX,StrOut).*/



%Predicado que traduce lista de usuarios cuando no hay usuario online
%DOM: Lista usuarios x lista usuarios x "" x output
%REC: string
%Tipo de meta: Secundaria
traduceListaUserOff([],_,String,String).
traduceListaUserOff([User|ColaUser],Users,String,StrOut):-traduceUserOff(User,Users,StrAUX),
                                                           atomics_to_string([String,StrAUX,"\n",
                                                                             "__________________________","\n"],
                                                                             StrAUX2),
                                                           traduceListaUserOff(ColaUser,Users,StrAUX2,StrOut).


%Predicado que traduce un usuario cuando no hay usuario online
%DOM: tda user x lista user x output
%REC: String
%Tipo de meta: Secundaria
traduceUserOff(User,Users,UserSTR):-not(tieneSeguidores(User,Users)),getNameUser(User,Name),
                                    getAmigosUser(User,Amigos),length(Amigos,LAMIGOS),LAMIGOS==0,
                                     atomics_to_string([Name,"\n",
                                                       "        No sigue a nadie","\n",
                                                       "No es seguido por nadie","\n"
                                                       ]," ", UserSTR),!.

traduceUserOff(User,Users,UserSTR):-tieneSeguidores(User,Users),getNameUser(User,Name),
                                    getAmigosUser(User,Amigos),length(Amigos,LAMIGOS),LAMIGOS==0,
                                    getSeguidores(User,Users,Seguidores),
                                    traduceSeguidores(Seguidores,"",SeguidoresStr),
                                    atomics_to_string([Name,"\n",
                                                       "        No sigue a nadie","\n","\n",
                                                       "Es seguido por:","\n",
                                                       SeguidoresStr]," ", UserSTR),!.


traduceUserOff(User,Users,UserSTR):-not(tieneSeguidores(User,Users)),getNameUser(User,Name),
                                    getAmigosUser(User,Amigos),length(Amigos,LAMIGOS),LAMIGOS\=0,

                                    traduceAmigos(User,ListaAmigos),traduceAmigosString(ListaAmigos,"",AmigosString),

                                    atomics_to_string([Name,"\n",
                                             "     Sigue a:","\n",
                                           AmigosString,"\n",
                                           "No es seguido por nadie","\n","\n"]," ",UserSTR),!.

traduceUserOff(User,Users,UserSTR):-tieneSeguidores(User,Users),getNameUser(User,Name),
                                    getAmigosUser(User,Amigos),length(Amigos,LAMIGOS),LAMIGOS\=0,
                                    getSeguidores(User,Users,Seguidores),
                                    traduceAmigos(User,ListaAmigos),traduceAmigosString(ListaAmigos,"",AmigosString),
                                    traduceSeguidores(Seguidores,"",SeguidoresStr),
                                    atomics_to_string([Name,"\n",
                                             "     Sigue a:","\n",
                                           AmigosString,"\n",
                                           "Es seguido por:","\n","\n",
                                           SeguidoresStr]," ",UserSTR),!.




%Predicado que verifica si un usuario tiene seguidores
%DOM: tda user x lista tda user
%REC: boolean
%Tipo de meta: Secundaria
tieneSeguidores(_,[]):-false,!.
tieneSeguidores(User,[User1|_]):- %getAmigosUser(User1,Amigos),estaEnLista(User,Amigos),!.
                                  getNameUser(User,NameUSer),
                                  getAmigosUser(User1,Amigos),getListaNames(Amigos,AmigosStr),member(NameUSer,AmigosStr),!.

tieneSeguidores(User,[User1|UserCola1]):- %getAmigosUser(User1,Amigos),not(estaEnLista(User,Amigos)),
                                         getNameUser(User,NameUSer),
                                         getAmigosUser(User1,Amigos),getListaNames(Amigos,AmigosStr),not(member(NameUSer,AmigosStr)),
                                         tieneSeguidores(User,UserCola1).


traduceSeguidores([],String,String).
traduceSeguidores([User|UserCola],String,StrOut):-atomics_to_string([String,User,"\n"],' ',StrAux),
                                                  traduceSeguidores(UserCola,StrAux,StrOut).


% Predicado que se utiliza para traducir un post cuando se intenta
% traducir un PostShare
% DOM: tda post, Output
% REC: String
% Tipo de meta: Secundaria
postEnPalabra(Post,PostStr):-getUserPost(Post,NamePost),getNameUser(NamePost,Name),getDatePost(Post,FechaPost),
                           getContenidoPost(Post,Contenido),getUsersPost(Post,ListaUserPost),length(ListaUserPost,L),
                           L==0,
                           getIDPost(Post,PostID),
                           traduceFecha(FechaPost,FechaStr),
                           atomics_to_string(["ID:",PostID,"\n",
                                              "el día",FechaStr,Name,"publicó:","\n",
                                              '"',Contenido,'"',"\n",
                                              "Destinatarios: Su Perfil"],' ',PostStr),!.

postEnPalabra(Post,PostStr):-getUserPost(Post,NamePost),getNameUser(NamePost,Name),getDatePost(Post,FechaPost),
                           getContenidoPost(Post,Contenido),getUsersPost(Post,ListaUserPost),length(ListaUserPost,L),
                           L\=0,
                           getIDPost(Post,PostID),
                           traduceFecha(FechaPost,FechaStr),
                           listaTdaUserToString(ListaUserPost,ListaUserStr),
                           nth0(0,ListaUserStr,UserDest),
                           atomics_to_string(["ID:",PostID,"\n",
                                              "el día",FechaStr,Name,"publicó:","\n",
                                              '"',Contenido,'"',"\n",
                                              "Destinatarios:",UserDest],' ',PostStr),!.


%Predicado que se encarga de traducir la lista de post a un string
%DOM: Lista, "", Output
%REC: String
%Tipo de meta: Secundaria
traduceListaPost([],String,String).
traduceListaPost([Post|PostCola],String,StrOut):-traducePost(Post,PostStr),atomics_to_string([String,PostStr,"\n"],Str),
                                                 traduceListaPost(PostCola,Str,StrOut).



%Predicado que crea un tda PostShare
%DOM: tda user x tda fecha x tda post x lista string x output
%REC: tda PostShare
%Tipo de meta: Secundaria
postShare(User,Fecha,Post,ListaAmigos,PostShare):-
                                                  PostShare=[User,Fecha,Post,ListaAmigos].
% Predicado de pertencia que verifica que un elemento sea
% del tipo tda postshare
%DOM: elemento
%REC: boolean
%Tipo de meta: Secundaria
validaPostShare([User,Fecha,Post,ListaAmigos]):-validaUser(User),esFecha(Fecha),validaPost(Post),is_list(ListaAmigos).

/*############################################################Selectores PostShare##############################################*/

%Predicado que obtiene el usuario creador del postShare
%DOM: tda postShare x ouput
%REC: tda User
%Tipo de meta: Primario
getUserPostShare(PostShare,UserPostShare):-validaPostShare(PostShare),nth0(0,PostShare,UserPostShare).


%Predicado que obtiene la fecha del postShare
%DOM: tda postShare x ouput
%REC: tda fecha
%Tipo de meta: Primario
getFechaPostShare(PostShare,FechaPostShare):-validaPostShare(PostShare),nth0(1,PostShare,FechaPostShare).


%Predicado que obtiene el tda post del tda postShare
%DOM: tda postShare x ouput
%REC: tda post
%Tipo de meta: Primario
getPostPostShare(PostShare,PostPostShare):-validaPostShare(PostShare),nth0(2,PostShare,PostPostShare).

%Predicado que obtiene la lista de usuarios (string) del tda postShare
%DOM: tda postShare x output
%REC: lista string
%Tipo de meta: Primario
getUsersPostShare(PostShare,UsersPostShare):-validaPostShare(PostShare),nth0(3,PostShare,UsersPostShare).


% Predicado que permite compartir una publicacion en el espacio propio o
% en el de un o varios amigos
% DOM: tda socialnetwork x tda fecha x int x lista string x output
% REC: tda socialnetwork
% Tipo de meta: Primario
socialNetworkShare(Sn1,Fecha,PostId,ListaUsernamesDest,Sn2):- validaSocialNetwork(Sn1),esFecha(Fecha),number(PostId),PostId >= 0,
                                                              getUsersSN(Sn1,Users),
                                                              getUserOnline(Users,UserOnline),getPostSN(Sn1,PostId,Post),
                                                              length(ListaUsernamesDest,LDest),LDest == 0,
                                                              postShare(UserOnline,Fecha,Post,[],PostShare),
                                                              agregaPostUser(UserOnline,PostShare,UserPost),
                                                              agregaPostSN(Sn1,PostShare,SNAUX),
                                                             getIndex(UserOnline,Users,0,Index),
                                                             %setEstadoUser(UserPost,"offline",UserOff),
                                                             setElemLista(Index,Users,UserPost,NewListaUser),
                                                             setUsersSN(SNAUX,NewListaUser,SnOut),
                                                             turnOffSN(SnOut,Sn2),!.

socialNetworkShare(Sn1,Fecha,PostId,ListaUsernamesDest,Sn2):- validaSocialNetwork(Sn1),esFecha(Fecha),number(PostId),PostId >= 0,
                                                              dejaListaDest(Sn1,ListaUsernamesDest,ListaAUX),
                                                              listaTdaUserToString(ListaAUX,ListaOut),
                                                              length(ListaOut,LDest),LDest > 0,
                                                              aplicaShareToAmigo(Sn1,Fecha,PostId,ListaOut,SnOut),
                                                              turnOffSN(SnOut,Sn2),!.

% predicado que permite compartir un post en el perfil de un amigo
%DOM: tda socialnetwork x tda fecha x int x string x output
%REC: tda socialnetwork
%Tipo de meta: Secundario
shareToAmigo(Sn1,Fecha,PostId,UserDest,Sn2):-validaSocialNetwork(Sn1),esFecha(Fecha),number(PostId),PostId >= 0,
                                                       getUsersSN(Sn1,Users),
                                                       getUserOnline(Users,UserOnline),getPostSN(Sn1,PostId,Post),
                                                       getUserConNombre(UserDest,Users,ReceptorTda),
                                                       getAmigosUser(UserOnline,Amigos),getListaNames(Amigos,AmigosStr),member(UserDest,AmigosStr),
                                                       %esAmigo(UserOnline,ReceptorTda),
                                                       postShare(UserOnline,Fecha,Post,[ReceptorTda],PostShare),
                                                       agregaPostUser(UserOnline,PostShare,UserPost),
                                                       agregaPostSN(Sn1,PostShare,SNAUX),
                                                       getIndex(UserOnline,Users,0,Index),
                                                       %setEstadoUser(UserPost,"offline",UserOff),
                                                       setElemLista(Index,Users,UserPost,NewListaUser),
                                                       setUsersSN(SNAUX,NewListaUser,Sn2).



%Predicado que comparte una publicacion en el muro de varios amigos
%Dom: tda socialnetwork x tda fecha x int x lista string x output
%Rec: tda socialnetwork
%Tipo de meta: Secundario
aplicaShareToAmigo(SnOut,_,_,[],SnOut):-!.
aplicaShareToAmigo(Sn1,Fecha,PostId,[X|Xs],Sn2):-validaSocialNetwork(Sn1),esFecha(Fecha),
                                                 shareToAmigo(Sn1,Fecha,PostId,X,SNAUX),
                                                 aplicaShareToAmigo(SNAUX,Fecha,PostId,Xs,Sn2).


%Predicado que cambia el estado de un tda user a offline
%DOM: tda user x output
%REC: tda user
%Tipo de meta: Secundario
turnOffUser(User,UserOff):-validaUser(User),setEstadoUser(User,"offline",UserOff).


% Predicado que cambia el estado de cada tda user a offline perteneciente
% a una lista de tda user
%DOM: lista tda user x output
%REC: lista tda user
%Tipo de meta: Secundario

turnOffLista([],[]).
turnOffLista([User|UserCola],[UserOff|ColaOff]):-turnOffUser(User,UserOff),turnOffLista(UserCola,ColaOff).


% Predicado que cambia el estado de todos los usuarios de un tda
% socialnetwork a un estado offline
%DOM: tda socialnetwork x output
%REC: tda socialnetwork
%Tipo de meta: Secundario
turnOffSN(SN,SNOUT):-validaSocialNetwork(SN),getUsersSN(SN,ListaUser),turnOffLista(ListaUser,ListaOFF),
                      setUsersSN(SN,ListaOFF,SNOUT).



%Predicado que convierte un tda socialnetwork a un string visible
%DOM: Tda socialnetwork x output
%REC: string
%Tipo de meta: Primaria

socialNetworkToString(Sn1,StrOut):-not(existeUserOnline(Sn1)),getNameSN(Sn1,Name),
                                   getDateSN(Sn1,Date),getPerfilSN(Sn1,Perfil),
                                   traduceFecha(Date,DateStr),getUsersSN(Sn1,Users),
                                   traduceListaUserOff(Users,Users,"",UserStr),
                                   traduceListaPost(Perfil,"",PerfilSTR),
                                  atomics_to_string(["#######Red Social",Name,"#######","\n",
                                                       "Creada el día",DateStr,"\n",
                                                     "*** Usuarios Registrados ***","\n",
                                                     UserStr,"\n",
                                                     "--------------------------------------------------","\n",
                                                     "*** Publicaciones ***","\n",
                                                     PerfilSTR,"\n",
                                                     "*** Fin Publicaciones ***"],' ',StrOut),!.



socialNetworkToString(Sn1,StrOut):-existeUserOnline(Sn1),getNameSN(Sn1,Name),getDateSN(Sn1,Date),
                                   traduceFecha(Date,DateStr),
                                   getUsersSN(Sn1,Users), getUserOnline(Users,UserOnline),
                                   %getUsersSN(Sn1,Users),%getPerfilSN(Sn1,Perfil),
                                   %getNameUser(UserOnline,NameUser),traduceAmigosString(UserOnline,"",AmigosSTR),
                                   getPerfilUser(UserOnline,Perfil),traduceListaPost(Perfil,"",PerfilStr),
                                    traduceUserOff(UserOnline,Users,UserString),
                                    atomics_to_string(["#######Red Social",Name,"#######","\n",
                                                       "Creada el día",DateStr,"\n",
                                                       "*** Usuario con sesión iniciada ***","\n",
                                                      UserString,"\n",
                                                      "--------------------------------------------------","\n",
                                                      "*** Su perfil contiene ***","\n",
                                                      PerfilStr],' ',StrOut),!.


/*ig(Sn1),publicaEnUno(Sn1,[6,6,2021],"Este es un post de prueba",SN),socialNetworkLogin(SN,"BenjaminParra","benja123",SN2),socialNetworkShare(SN2,[1,2,2021],0,["BobbyParra","caca","ChiloParra"],SN4),socialNetworkRegister(SN4,[6,8,2021],"BorisParra","boris123",SN5),socialNetworkLogin(SN5,"BorisParra","boris123",SN6),socialNetworkFollow(SN6,"BenjaminParra",SN7),socialNetworkToString(SN7,STR),write(STR).

                                    */






%Predicado que verifica si un usuario es seguidor de otro
%DOM: Tda User x Lista Tda User
%REC: boolean
%Tipo de meta: Secundario
esSeguidor(User,ListaAmigos):-estaEnLista(User,ListaAmigos).

%Predicado que obtiene el nombre del usuario seguidor
%DOM: tda user x tda user x output
%REC: string
%Tipo de meta: Secundario
obtieneSeguidor(User,User2,UserOut):-validaUser(User),validaUser(User2),getAmigosUser(User2,AmigosUser),
                                esSeguidor(User,AmigosUser),getNameUser(User2,UserOut).

%Predicado que obtiene uan lista de seguidores
%DOM:tda user x lista tda user x output
%REC: lista string
%Tipo de meta: Secundario
getSeguidores(_,[],[]):-!.
%getListaNames(Amigos,AmigosStr),member(NameUser,AmigosStr)
/*
getSeguidores(User,[Seguidor|ColaSeguidor],[X|Xs]):-getAmigosUser(Seguidor,AmigosSeguidor),

                                                    esSeguidor(User,AmigosSeguidor),obtieneSeguidor(User,Seguidor,X),
                                                    getSeguidores(User,ColaSeguidor,Xs).*/

getSeguidores(User,[Seguidor|ColaSeguidor],[X|Xs]):-getNameUser(User,NameUser),
                                                    getAmigosUser(Seguidor,AmigosSeguidor),
                                                    getListaNames(AmigosSeguidor,AmigosSeguidorStr),
                                                    getNameUser(Seguidor,SeguidorStr),
                                                    member(NameUser,AmigosSeguidorStr),X = SeguidorStr,
                                                    getSeguidores(User,ColaSeguidor,Xs).
/*
getSeguidores(User,[Seguidor|ColaSeguidor],Xs):-getAmigosUser(Seguidor,AmigosSeguidor),
                                                    not(esSeguidor(User,AmigosSeguidor)),
                                                    getSeguidores(User,ColaSeguidor,Xs).
*/

getSeguidores(User,[Seguidor|ColaSeguidor],Xs):-    getNameUser(User,NameUser),
                                                    getAmigosUser(Seguidor,AmigosSeguidor),
                                                    getListaNames(AmigosSeguidor,AmigosSeguidorStr),
                                                    not(member(NameUser,AmigosSeguidorStr)),
                                                    getSeguidores(User,ColaSeguidor,Xs).



/*#####################################################################################################################################################*/
/*                             BLOQUE DE PRUEBAS */
%Creacion redes sociales
%
%
%Prueba Numero 1
%fecha(1,7,2021,Fecha),socialNetwork("instagram",Fecha,Sn).

%Prueba Numero 2
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn).

%Prueba Numero 3
%fecha(28,2,2021,Fecha),socialNetwork("tw",Fecha,Sn).


%Test socialNetworkRegister

%Prueba Numero 1
%Registro del usuario "Michael Scott" con la contraseña "Michael123"
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1).

%Prueba Numero 2
% Registro del usuario "Dwigth Schrute" con la
% contraseña"ilovemichael123"
/*
 fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael
 Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth
 Schrute","ilovemichael123",Sn2).
*/

%Prueba Numero 3
% Registro del usuario "Jim Halpert" con la contraseña "pamAndJim952"
/*
fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael
Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth
Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim
Halpert","pamAndJim952",Sn3).
*/



%Test socialNetworkLogin
%
%Prueba Numero 1
% Inicio de sesión del usuario "Michel Scott"

%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4).

%Prueba Numero 2
% Inicio de sesión del usuario "Dwigth Schrute"
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkLogin(Sn4,"Dwigth Schrute","ilovemichael123",Sn5).

%Prueba Numero 3
% Inicio de sesión del usuario "Jim Halpert"
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkLogin(Sn4,"Dwigth Schrute","ilovemichael123",Sn5),socialNetworkLogin(Sn5,"Jim Halpert","pamAndJim952",Sn6).

%Test socialNetworkFollow
%
%Prueba Numero 1
% Usuario "Michael Scott"  sigue al usuario "Dwigth Schrute"
% fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5).

%Prueba Numero 2
% Usuario "Michael Scott" sigue al usuario "Jim Halpert"
% fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7).

%Prueba Numero 3
% Usuario "Dwigth Schrute" sigue al usuario "Michael Scott"
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9).

%Test socialNetworkPost
%
%Prueba Numero 1
%Usuario "Michael Scott" publica un mensaje en su propio muro
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11).

%Prueba Numero 2
% Usuario "Michael Scott" publica un mensaje en el muro de "Dwigth
% Schrute" y "Jim Halpert"
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13).

%Prueba Numero 3
% Usuario "Dwigth Schrute publica un mensaje en el muro de "Michael
% Scott"
% fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13),socialNetworkLogin(Sn13,"Dwigth Schrute","ilovemichael123",Sn14),socialNetworkPost(Sn14,Fecha,"You are the best boss",["Michael Scott"],Sn15).


%Test socialNetworkShare

%Prueba Numero 1
% Usuario "Jim Halpert" comparte una publicación en su muro
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13),socialNetworkLogin(Sn13,"Dwigth Schrute","ilovemichael123",Sn14),socialNetworkPost(Sn14,Fecha,"You are the best boss",["Michael Scott"],Sn15),socialNetworkLogin(Sn15,"Jim Halpert","pamAndJim952",Sn16),socialNetworkShare(Sn16,Fecha,0,[],Sn17).

%Prueba Numero 2
% Usuario "Dwigth Schrute" comparte publicacion en el muro de "Michael
% Scott"

%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13),socialNetworkLogin(Sn13,"Dwigth Schrute","ilovemichael123",Sn14),socialNetworkPost(Sn14,Fecha,"You are the best boss",["Michael Scott"],Sn15),socialNetworkLogin(Sn15,"Jim Halpert","pamAndJim952",Sn16),socialNetworkShare(Sn16,Fecha,0,[],Sn17),socialNetworkLogin(Sn17,"Dwigth Schrute","ilovemichael123",Sn18),socialNetworkShare(Sn18,Fecha,1,["Michael Scott"],Sn19).
%

%Prueba Numero 3
% Usuario "Michael Scott" comparte publucacion en el muro de "Jim
% Halpert" y "Dwigth Schrute"
%
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13),socialNetworkLogin(Sn13,"Dwigth Schrute","ilovemichael123",Sn14),socialNetworkPost(Sn14,Fecha,"You are the best boss",["Michael Scott"],Sn15),socialNetworkLogin(Sn15,"Jim Halpert","pamAndJim952",Sn16),socialNetworkShare(Sn16,Fecha,0,[],Sn17),socialNetworkLogin(Sn17,"Dwigth Schrute","ilovemichael123",Sn18),socialNetworkShare(Sn18,Fecha,1,["Michael Scott"],Sn19),socialNetworkLogin(Sn19,"Michael Scott","Michael123",Sn20),socialNetworkShare(Sn20,Fecha,3,["Dwigth Schrute","Jim Halpert"],Sn21).
%

%Test SocialNetworkToString
%
%Prueba Numero 1
%Sin sesión iniciada
%fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13),socialNetworkLogin(Sn13,"Dwigth Schrute","ilovemichael123",Sn14),socialNetworkPost(Sn14,Fecha,"You are the best boss",["Michael Scott"],Sn15),socialNetworkLogin(Sn15,"Jim Halpert","pamAndJim952",Sn16),socialNetworkShare(Sn16,Fecha,0,[],Sn17),socialNetworkLogin(Sn17,"Dwigth Schrute","ilovemichael123",Sn18),socialNetworkShare(Sn18,Fecha,1,["Michael Scott"],Sn19),socialNetworkLogin(Sn19,"Michael Scott","Michael123",Sn20),socialNetworkShare(Sn20,Fecha,3,["Dwigth Schrute","Jim Halpert"],Sn21),socialNetworkToString(Sn21,Str),write(Str).

%Prueba Numero 2
%Con la sesión iniciada del usuario "Dwigth Schrute"
% fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13),socialNetworkLogin(Sn13,"Dwigth Schrute","ilovemichael123",Sn14),socialNetworkPost(Sn14,Fecha,"You are the best boss",["Michael Scott"],Sn15),socialNetworkLogin(Sn15,"Jim Halpert","pamAndJim952",Sn16),socialNetworkShare(Sn16,Fecha,0,[],Sn17),socialNetworkLogin(Sn17,"Dwigth Schrute","ilovemichael123",Sn18),socialNetworkShare(Sn18,Fecha,1,["Michael Scott"],Sn19),socialNetworkLogin(Sn19,"Michael Scott","Michael123",Sn20),socialNetworkShare(Sn20,Fecha,3,["Dwigth Schrute","Jim Halpert"],Sn21),socialNetworkLogin(Sn21,"Dwigth Schrute","ilovemichael123",Sn22),socialNetworkToString(Sn22,Str),write(Str).

%Prueba Numero 3
%Con la sesión iniciada del usuario "Michael Scott"
% fecha(28,2,2021,Fecha),socialNetwork("facebook",Fecha,Sn),socialNetworkRegister(Sn,Fecha,"Michael Scott","Michael123",Sn1),socialNetworkRegister(Sn1,Fecha,"Dwigth Schrute","ilovemichael123",Sn2),socialNetworkRegister(Sn2,Fecha,"Jim Halpert","pamAndJim952",Sn3),socialNetworkLogin(Sn3,"Michael Scott","Michael123",Sn4),socialNetworkFollow(Sn4,"Dwigth Schrute",Sn5),socialNetworkLogin(Sn5,"Michael Scott","Michael123",Sn6),socialNetworkFollow(Sn6,"Jim Halpert",Sn7),socialNetworkLogin(Sn7,"Dwigth Schrute","ilovemichael123",Sn8),socialNetworkFollow(Sn8,"Michael Scott",Sn9),socialNetworkLogin(Sn9,"Michael Scott","Michael123",Sn10),socialNetworkPost(Sn10,Fecha,"World's best boss",[],Sn11),socialNetworkLogin(Sn11,"Michael Scott","Michael123",Sn12),socialNetworkPost(Sn12,Fecha,"Do you want to go to the Hotties?",["Dwigth Schrute","Jim Halpert"],Sn13),socialNetworkLogin(Sn13,"Dwigth Schrute","ilovemichael123",Sn14),socialNetworkPost(Sn14,Fecha,"You are the best boss",["Michael Scott"],Sn15),socialNetworkLogin(Sn15,"Jim Halpert","pamAndJim952",Sn16),socialNetworkShare(Sn16,Fecha,0,[],Sn17),socialNetworkLogin(Sn17,"Dwigth Schrute","ilovemichael123",Sn18),socialNetworkShare(Sn18,Fecha,1,["Michael Scott"],Sn19),socialNetworkLogin(Sn19,"Michael Scott","Michael123",Sn20),socialNetworkShare(Sn20,Fecha,3,["Dwigth Schrute","Jim Halpert"],Sn21),socialNetworkLogin(Sn21,"Michael Scott","Michael123",Sn22),socialNetworkToString(Sn22,Str),write(Str).
