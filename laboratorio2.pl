
%predicado que cumple constructora
%date(DD,MM,YYYY,Fecha)
%Permite Construir una fecha
%dom:int x int x int x Outpout
%rec: tda Fecha

fecha(DD,MM,YYYY,Fecha):-number(DD),DD=<31,MM=<12,YYYY > 0,DD > 0, YYYY >0 ,Fecha = [DD,MM,YYYY].



%predicado que Permite calcular si un n�mero es par o no
%DOM: int
%REC: boolean
esPar(0):-!, true.
esPar(1):-!, false.
esPar(Numero):- Aux1 is Numero-2, esPar(Aux1).



%modulo(N,Out):- Out is rem(N,400).
/**/


%predicado que Permite saber si un elemento cualquiera es del tipo fecha
%dom: elemento cualquiera
%rec: boolean
esFecha([DD,MM,YYYY]):- number(DD), number(MM),number(YYYY),checkLargoFecha([DD,MM,YYYY])%verifica que sean numeros
                       ,getDiasMes([_,MM,YYYY],MaximoDias), MaximoDias >= DD.

checkLargoFecha(Fecha):- length(Fecha,Aux), Aux==3,!.

% predicado que Obtiene el numero de dias maximo que puede tener un mes
% considerando si es a�o bisiseto o no
%DOM: tda fecha, argumento de salid
% REC: int
getDiasMes([_,MM,YYYY],MaximoDias):-
number(MM),number(YYYY),MM>0,MM<13, mesesPares([_,MM,YYYY],MaximoDias).
% getDiasMes(Fecha,MaximoDias):-getMM(Fecha,Mes),number(Mes),getYYYY(Fecha,Annio),number(Annio),Mes>0,Mes<13,
%
  %  mesesPares([_,Mes,Annio],MaximoDias).
%predicado que Obtiene el maximo de d�a si es que el mes es par o impar
%DOM: tda fecha, outpout
%REC: int
mesesPares([_,MM,_],MaximoDias):- MM\=2, esPar(MM),MaximoDias=31,!.
mesesPares([_,MM,_],MaximoDias):- not(esPar(MM)), MaximoDias=30,!.
mesesPares([_,MM,YYYY],MaximoDias):- MM == 2, esBisiesto([_,_,YYYY]),MaximoDias = 29,!;
                                       MaximoDias = 28.

%predicado que Permite saber si un a�o es bisiesto o no
%Dom: tda Fecha
%Rec: boolean
esBisiesto([_,_,YYYY]):- Aux1 is mod(YYYY,400), Aux1 == 0,!;
    Aux2 is mod(YYYY,4), Aux2 == 0,true, Aux3 is mod(YYYY,100), not(Aux3 == 0).

%Selectores
%predicado que Obtiene el d�a perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int
getDD([DD,MM,YYYY],Dia):- esFecha([DD,MM,YYYY]),DD = Dia.

%predicado que Obtiene el mes perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int
%
getMM([DD,MM,YYYY],Mes):- esFecha([DD,MM,YYYY]),MM = Mes.

%predicado que Obtiene el a�o perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int

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
string_not_empty(String):- string_length(String,Cont) , Cont\=0.
/*#########################SELECTORES#######################*/

%predicado que Obtiene el Nombre del tda Usuario
%DOM: tda user
%REC: string
getNameUser(User,Name):-validaUser(User), nth0(0,User,Name).

%Obtiene la password del tda Usuario
%DOM: tda User
%REC: string
getPasswordUser(User,Password):-validaUser(User), nth0(1,User,Password).

%predicado que Obtiene la fecha de registro del tda Usuario
%DOM: tda User
%REC: tda fecha
%getFechaUser(User,Password):-validaUser(User), nth0(2,User,Password).



%predicado que Obtiene la lista de amigos del tda Usuario
%DOM: tda User
%REC: list tda user
getAmigosUser(User,Amigos):-validaUser(User), nth0(2,User,Amigos).
%[Name,Password,Amigos,Perfil,Fecha,Estado]
%

%predicado que Obtiene el perfil del tda Usuario
%DOM: tda User
%REC: lista tda post
getPerfilUser(User,Perfil):-validaUser(User), nth0(3,User,Perfil).


%predicado que Obtiene la fecha de registro del tda Usuario
%DOM: tda User
%REC: tda fecha
getFechaUser(User,Fecha):-validaUser(User), nth0(4,User,Fecha).

% predicado que Obtiene l estado del tda Usuario,el cual apunta si es
% que est� offline o no
%DOM: tda User
%REC: string
getEstadoUser(User,Estado):-validaUser(User), nth0(5,User,Estado).

/*########################### Funciones Modificadoras#######*/

%predicado que Se encarga de cambiar el estado de un tda usuario
%DOM: tda user, string
%REC: tda user
setEstadoUser(User,NewEstado,NewUser):- validaUser(User),string(NewEstado),setElemLista(5,User,NewEstado,NewUser).
% Predicado que se encarga de cambiar el perfil de un usaurio por uno
% nuevo
% DOM: tda User x list tda post x Output
% REC: tda User
setPerfilUser(User,NewPerfil,NewUser):-validaUser(User),setElemLista(3,User,NewPerfil,NewUser).
% predicado que Se encarga de cambiar la lista de amigos de un tda
% usuario
%DOM: tda user, lista de amigos REC: tda user se agreg� el "!"
% para que el validaListaUser no retornara un F
setListaAmigosUser(User,NewLista,NewUser):- validaUser(User),validaListaUser(NewLista,AUX),!,setElemLista(2,User,AUX,NewUser).




%predicado que Cambia un elemento de una lista dado un indicador asi entregando una
% nueva lista
%DOM: int x list x cualquier elemento x Output
%REC: List
setElemLista(Index,Lista,NewElem,ListaOut):-Index >= 0,length(Lista,L),L>0,Index =< L, nth0(Index,Lista,_,AUX),nth0(Index, ListaOut,NewElem,AUX).











/*TDA SOCIALNETWORK*/
%Representacion
%[NombreRedSocial,Fecha,ListaUser,ListaPost]



% predicado que Verifica si un string corresponde a los nombre de una red
% social
%dom: string
%rec: boolean
checkNameSN(Name):- Name == "facebook",!;
                    Name == "fb",!;
                    Name == "instagram",!;
                    Name == "ig",!;
                    Name == "twitter",!;
                    Name == "tw".



%predicado que Permite construir una tda socialnetwork
%DOM: string x tda fecha x Output
%Rec: tda socialNetwork

socialNetwork(Name,Date,SOut):- string(Name), esFecha(Date), checkNameSN(Name), SOut = [Name,Date,[],[]].

%Pertenencia
%predicado que Permite valida que un elemento sea un socialnetwork
%dom: elemento
%rec: boolean
validaSocialNetwork([Name,Date,ListaUser,ListaPost]):-length([Name,Date,ListaUser,ListaPost],L), L==4,checkNameSN(Name),
                                                       esFecha(Date),is_list(ListaUser),is_list(ListaPost).

%["Facebook",[1,3,2021],[],[]]
/*############################Selectores Tda SocialNewtwork####################*/
%predicado que Entrega el nombre un tda socialnetwork
%DOM: tda SN
%REC: string
getNameSN(SN,Name):-validaSocialNetwork(SN), nth0(0,SN,Name).

%predicado que Entrega el nombre un tda socialnetwork
%DOM: tda SN
%REC: tda Fecha
getDateSN(SN,Fecha):-validaSocialNetwork(SN), nth0(1,SN,Fecha1), esFecha(Fecha1),Fecha1 = Fecha.

%predicado que Entrega lista User un tda socialnetwork
%DOM: tda SN
% REC: list
%  getUsersSN( ["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []],USERS).
/* ------------------------------------------------------------------------------------------------------------>! aqui es un or###*/
getUsersSN(SN,ListaUser):-validaSocialNetwork(SN), nth0(2,SN,ListaUser).
/*,length(ListaUser1,L),L==0,ListaUser = [],!;validaSocialNetwork(SN), nth0(2,SN,ListaUser1),
validaListaUser(ListaUser1,Out),ListaUser=Out,!.*/

%Predicado que entrega lista de post de un tda socialnetwork
%DOM: tda sn
%REC: list
getPerfilSN(SN,Perfil):-validaSocialNetwork(SN),nth0(3,SN,Perfil).

%predicado que Cambia la lista de Usuarios por otra nueva
%DOM: tda socialnetork x lista de elementos x output
%REC: tda socialnetwork
setUsersSN(SN,NewUsers,NewSN):-validaSocialNetwork(SN),setElemLista(2,SN,NewUsers,NewSN).

%Predicado que cambia la lista de post por otra nueva
%DOM: tda sn x lista elementos x output
%REC: tda sn
setPerfilSN(SN,NewPerfil,NewSN):-validaSocialNetwork(SN),setElemLista(3,SN,NewPerfil,NewSN).

%Predicado que dado un id obtiene el post de un SN
%DOM: tda sn x int x output
%REC: tda post
getPostSN(SN,ID,Post):-validaSocialNetwork(SN),getPerfilSN(SN,Perfil),nth0(ID,Perfil,Post).

/*TDA POST*/
% predicado que Contruye el tda Post
%DOM: tda user x tda date x string x lista tda user x int x Output
%REC: tda post
% post(["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],[04,06,2021],"Esto es un texto",[["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]],1,Post).
post(Usuario,Date,Contenido,ListaUsers,PostID,Post):-validaUser(Usuario),esFecha(Date),
                                                     validaContenido(Contenido),is_list(ListaUsers),/*verificarListaUser*/number(PostID),
                                                     Post = [Usuario,Date,Contenido,ListaUsers,PostID].


/*publicaEnUno(Sn,User,Date,Contenido,Tipo,SNOut):-validaSocialNetwork(Sn),validaUser(User),esFecha(Date),validaContenido(Contenido),
post(User,Date,Contenido,[],1 aqui debo hacer un length del lista post del user ).*/

% predicado que Comprueba si un elemento cualquiera es tda post es valido
% o no
%dom: cualquier elemento
%rec: boolean
validaPost([Usuario,Date,Contenido,ListaUsers,PostID]):- validaUser(Usuario),esFecha(Date),
                                                     validaContenido(Contenido),is_list(ListaUsers),validaListaUser(ListaUsers,Aux_Users),!,is_list(Aux_Users),number(PostID).

/*##########################SELECTORES TDA POST##################################*/
%predicado que Selecciona el usuario creador del tda post
%DOM: tda Post x output
%REC: tda user
getUserPost(Post,User):-validaPost(Post),nth0(0,Post,User).

%predicado que Selecciona la fecha de creacion del post
%DOM: tda post x output
%REC: tda fecha
getDatePost(Post,Date):-validaPost(Post),nth0(1,Post,Date).

%Selecciona el contenido del post
%DOM: tda post x output
%REC: string
getContenidoPost(Post,Contenido):-validaPost(Post),nth0(2,Post,Contenido).

%predicado que Selecciona a lista de destinarios del post
%DOM: tda post x output
%REC: lista tda user
getUsersPost(Post,Users):-validaPost(Post),nth0(3,Post,Users).

%predicado que Selecciona el identificador del post
%DOM: tda post x output
%REC: int
getIDPost(Post,IDPOST):-validaPost(Post),nth0(4,Post,IDPOST).


%predicado que Valida el contenido de una publicacion
%dom: contenido
%rec: boolean
validaContenido(Contenido):-string(Contenido),string_not_empty(Contenido).

%predicado que Valida que una lista contenga solo elementos Tda Usuarios validos y
% devuelve la lista solo con usuarios correctos
%dom: lista usuarios,output
%rec: lista usuarios
validaListaUser([],[]):-!.
%validaListaUser(L,L).
validaListaUser([User|ColaUser],[User|Result]):-validaUser(User),validaListaUser(ColaUser,Result).
% validaListaUser([User|ColaUser],[User1|ColaUser]):-validaUser(User),validaListaUser(ColaUser,[User1,User|ColaUser]).
validaListaUser([User|ColaUser],Out):-not(validaUser(User)),validaListaUser(ColaUser,Out).








/*cambiarPrecio([A|B],NombreArt,NvoPrecio,[NvoArti|B]):-
esListaDeCompra(B), esArticulo(A),number(NvoPrecio),
NvoPrecio > 0, string(NombreArt),
articuloGetName(A,NombreArt),
articuloSetPrice(A,NvoPrecio,NvoArti).


cambiarPrecio([A|B],NombreArt,NvoPrecio,[A|NvaCola]):-
esListaDeCompra(B), esArticulo(A),number(NvoPrecio),
NvoPrecio > 0, string(NombreArt),
cambiarPrecio(B,NombreArt,NvoPrecio,NvaCola).
*/
sn(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []]).

snConBobby(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []]).

%predicado que Se encarga de registrar un usuaio en tda SocialNetwork
%DOM: tda SocialNetwork x Tda Fecha x string x string x Output
%REC: tda SocialNetwork
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
verificaUserName(Username):-string_not_empty(Username),string_length(Username,Cont),Cont >=6.

% Verifica que una password sea no vacia, mayor a 8 y que contenga al
% menos 2 numeros
%DOM: string
%REC: boolean
verificaPassword(Password):-string_not_empty(Password),string_length(Password,Cont),Cont >=8,
                           lista_numeros(Lista_Numeros),tiene_numeros(Password,Lista_Numeros).

%predicado que Verifica si un Username esta en la lista de usuarios de socialnetwork,
%DOM: string x tda socialnetwork
%REC: boolean
checkRegister(Username,SN):-getUsersSN(SN,Users),getListaNames(Users,ListaString),member(Username,ListaString),!.

%predicado que Entrega los nombres (strings) de una lista de usuarios
%DOM: lista tda user x output
%REC: lista string
getListaNames([],[]):-true,!.
getListaNames([X|Xs],[Name|Ys]):- getNameUser(X,Name),getListaNames(Xs,Ys).


%es una lista de numeros en Ascii
lista_numeros([49, 50, 51, 52, 53, 54, 55, 56, 57]).

%predicado que Comprueba que un string tenga al menos 2 numeros
%dom: string x lista numeros en ascii
%rec: boolean
tiene_numeros(String,Lista_Numeros):- string_to_list(String,List),comparten_elementos(List,Lista_Numeros,0,Cont),Cont >= 2.
% predicado que Entrega cuantas veces un elemento est� contenido en una
% lista
%DOM: Elemento x Lista x 0 x Output REc: int
existe_en_lista(_,[],Out,Out):-true,!.
existe_en_lista(Cabeza1,[Cabeza2|Cola2],CONT,AUX):- Cabeza1 == Cabeza2,Cont1 is CONT+1,existe_en_lista(Cabeza1,Cola2,Cont1,AUX).
existe_en_lista(Cabeza1,[Cabeza2|Cola2],CONT,AUX):- Cabeza1\==Cabeza2,existe_en_lista(Cabeza1,Cola2,CONT,AUX).


% predicado que Entrega cuantos elementos en comun tiene una lista con
% otra
%DOM: Lista x Lista x 0 x Output REC: int
comparten_elementos([],Lista,AUX,Out):-length(Lista,L), L\=0, Out is 0+AUX,!.
comparten_elementos([Cabeza1|Cola1],[Cabeza2|Cola2],CONT,AUX):-existe_en_lista(Cabeza1,[Cabeza2|Cola2],0,CONT1),!,CONTAUX is CONT1+CONT, comparten_elementos(Cola1,[Cabeza2|Cola2],CONTAUX,AUX).




%predicado que Agrega un elemento al final de una lista
%DOM: lista x elemento x output
%REC: lista
append_final([],Elem,[Elem]).
append_final([Cabeza1|Cola1],Elem,[Cabeza1|Cola2]):-append_final(Cola1,Elem,Cola2).



%Predicado que valida el inicio de sesi�n de un usuario
%DOM: tda socialnetwork, string,string, output
%REC: tda socialnetwork
socialNetworkLogin(Sn1,Username,Password,Sn2):- validaSocialNetwork(Sn1),verificaUserName(Username),verificaPassword(Password),
                                                getUsersSN(Sn1,ListaUser),verificaLogin(Username,Password,ListaUser),
                                                getUserConNombre(Username,ListaUser,User),setEstadoUser(User,"online",User2),                                                       getIndex(User,ListaUser,0,Index),setElemLista(Index,ListaUser,User2,NewListaUser),
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
verificaLogin(_,_,[]):-false,!.
verificaLogin(Username,Password,[User|ColaListaUser]):- getNameUser(User,Name),Name == Username,getPasswordUser(User,Pass),
                                                        Pass == Password,!;
                                                        verificaLogin(Username,Password,ColaListaUser).

% predicado que entrega un tda user que pertenece a una lista tda user
% dado un nombre
%Dom: string, lista tda user,
%Rec: tda user
getUserConNombre(_,[],_):-false,!.
getUserConNombre(UserName,[User|_],Usuario):- validaUser(User),getNameUser(User,Name),Name == UserName,Usuario = User,!.
getUserConNombre(UserName,[User|ColaListaUser],Usuario):- validaUser(User),getNameUser(User,Name),Name \= UserName
                                                         ,getUserConNombre(UserName,ColaListaUser,Usuario).
/*verificaLogin("BenjaminParra","benja123",[["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]]).*/



%getIndex(Elem,[X|Xs],Aux,Cont):-
% predicado que entrega en que posicion se encuentra un elem (siempre
% arroja el primer encuentro).
% DOM: elememto,listaElem,0,Output
% REC: int
getIndex(_,[],_,_):-false,!.
getIndex(Elem,[X|_],Aux,Cont):-Elem == X, Cont = Aux,!.
getIndex(Elem,[X|Xs],Aux,Cont):-Elem \= X, Aux1 is Aux + 1, getIndex(Elem,Xs,Aux1,Cont).


%setElemLista(Index,Lista,NewElem,ListaOut)

%Predicado que permite al usuario con sesion activa seguir a un usuario
%DOM: tda socialNetwork,string,OutPut
%REC: tda socialNetwork
%
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
%
registradoEnSn(Username,Sn1):-getUsersSN(Sn1,ListaUser),getUserConNombre(Username,ListaUser,Usuario),
                               estaEnLista(Usuario,ListaUser).



%Predicado que dado una lista de User entrega el que esta online
%DOM: Lista tda User x Output
%REC: tda User
getUserOnline([],_):-false,!.
getUserOnline([X|_],User):-getEstadoUser(X,Estado),Estado == "online", User = X,!.

getUserOnline([X|Xs],User):-getEstadoUser(X,Estado),Estado == "offline", getUserOnline(Xs,User).



% Predicado que verifica si un elemento esta en Lista, al momento de
% encontrarlo retorna un true y si no existe en Lista false
%DOM: elemento x listaElementos
%REC: boolean
estaEnLista(Elem,[Elem|_]):-!.
estaEnLista(Elem,[_|Cola]):-estaEnLista(Elem,Cola).

% Predicado que entrega la lista de tda user en base a una lista de
% usuarios registrados en el socialnetwork
%
%
convierteListaUser(_,[],[]):-true,!.

convierteListaUser(SN,[UserString|ColaString],[UserTda|ColaTda]):-getUsersSN(SN,ListaUser),
                                                                  getUserConNombre(UserString,ListaUser,UserTda),
                                                                  convierteListaUser(SN,ColaString,ColaTda).

%aplica un sort, a convierteListaUser para no publicar 2 veces.

% Predicado que verifica que un user(Amigo) este en la lista del User
% DOM: tda user x tda user
% REC: boolean
esAmigo(User,Amigo):-validaUser(User),validaUser(Amigo),getAmigosUser(User,Amigos),estaEnLista(Amigo,Amigos).
/*
 esAmigo(["BenjaminParra", "benja123", [["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], [], [8, 7, 1997], "online"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]).*/

% Predicado que en base a una lista de TDA User obtiene una lista de los
% nombres en string
% DOM: Lista TDA User
% REC: Lista string
listaTdaUserToString([],[]):-true,!.
listaTdaUserToString([User|ColaUsers],[Name|Xs]):-getNameUser(User,Name),listaTdaUserToString(ColaUsers,Xs).


%Predicado que compara dos listas y solo deja sus elementos en comun
%DOM: lista x lista x output
%REC: lista
%Se debe poner un ! para que solo de el final
dejaIguales([],_,[]).
dejaIguales([X|Xs],Lista,[X|OutCola]):-estaEnLista(X,Lista),
                                       dejaIguales(Xs,Lista,OutCola).
dejaIguales([X|Xs],Lista,OutCola):- not(estaEnLista(X,Lista)),
                                    dejaIguales(Xs,Lista,OutCola).


%Predicado que elimina el usuario online de una lista usuarios
%DOM: Lista tda User x output
%REC: lista tda user
%
eliminaOnline([X|Xs],[X|Ys]):-getEstadoUser(X,E),E=="offline",eliminaOnline(Xs,Ys).

eliminaOnline([X|Xs],Y):-getEstadoUser(X,E),E=="online",eliminaOnline(Xs,Y).

eliminaOnline([],[]):-!.



% Predicado que realiza la publicacion de un usuario Online a una lista
% usuarios que son sus amigos o en su propio perfil
% DOM: tda socialnetwork x tda fecha x string x lista string x Output
% REC: tda socialnetwork
socialNetworkPost(Sn1,Fecha,Texto,ListaUserNameDest,Sn2):- length(ListaUserNameDest,L),L==0,
                                                           publicaEnUno(Sn1,Fecha,Texto,Sn2),!.
socialNetworkPost(Sn1,Fecha,Texto,ListaUserNameDest,Sn2):- length(ListaUserNameDest,L),L\=0,
                                                           aplicaPublicaEnMuroUser(Sn1,Fecha,Texto,ListaUserNameDest,SnOut),
                                                           turnOffSN(SnOut,Sn2).



%Predicado que realiza la publicaci�n en el propio perfil
%DOM: tda sn x tda fecha x string x output
%REC: tda sn
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
                                               setUsersSN(SNAUX,NewListaUser,SnOut).
/*ig(Sn1),publicaEnMuroUser(Sn1,[08,06,2021],"Hola compa","BobbyParra",SnOut),getPerfilSN(SnOut,Perfil),nth0(0,Perfil,Post),traducePost(Post,PostStr),write(PostStr).*/


% Predicado que se encarga de aplicar el publicaEnMuroUser a una lista
% de string
%
%
aplicaPublicaEnMuroUser(SnOut,_,_,[],SnOut):-!.
aplicaPublicaEnMuroUser(Sn1,Fecha,Texto,[X|Xs],SnOut):-validaSocialNetwork(Sn1),esFecha(Fecha),string_not_empty(Texto),
                                                      publicaEnMuroUser(Sn1,Fecha,Texto,X,SNAUX),
                                                      aplicaPublicaEnMuroUser(SNAUX,Fecha,Texto,Xs,SnOut).


/* ig(Sn1),aplicaPublicaEnMuroUser(Sn1,[8,6,2021],"Hola Amigos",["ChiloParra","BobbyParra"],Snout),!,getPerfilSN(Snout,Perfil),nth0(0,Perfil,User1),nth0(1,Perfil,User2),traducePost(User1,STR1),traducePost(User2,STR2),write(STR1),write(STR2).*/


/* ig(Sn1),aplicaPublicaEnMuroUser(Sn1,[8,6,2021],"Hola Amigos",["ChiloParra","BobbyParra"],Snout),getUsersSN(Snout,Users),nth0(0,Users,User1),getPerfilUser(User1,PerfilUser),nth0(0,PerfilUser,Post),traducePost(Post,STR),write(STR),nth0(1,PerfilUser,Post1),traducePost(Post1,STR1),write(STR1).
*/


%getUserConNombre(UserName,[User|ColaListaUser],Usuario)
%esAmigo(User,Amigo)

%Predicado que agrega un post al perfil del usuario
%DOM: tda user x tda post x output
%REC: tda user
agregaPostUser(User,Post,UserPost):-validaUser(User), getPerfilUser(User,Perfil),append_final(Perfil,Post,NewPerfil),
                                    setPerfilUser(User,NewPerfil,UserPost).

%Predicado que agrega un post a la lista de post del tda socialnetwork
%DOM: tda sn x tda post x output
%REC: tda sn
agregaPostSN(Sn,Post,SnOut):-validaSocialNetwork(Sn),getPerfilSN(Sn,ListaPost),append_final(ListaPost,Post,NewListaPost),setPerfilSN(Sn,NewListaPost,SnOut).

% Predicado auxiliar de predicado post, entrega la lista validada de a
% que usuario se le puede publicar
% DOM: tda SoccialNetwork x Lista string x output
% REC: Lista tda user

dejaListaDest(Sn1,ListaUserNameDest,ListaOut):-getUsersSN(Sn1,ListaUser),listaTdaUserToString(ListaUser,ListaUserString),
                                                          dejaIguales(ListaUserNameDest,ListaUserString,DestRegistrados),!,
                                                          convierteListaUser(Sn1,DestRegistrados,DestRegistradosTda),
                                                          eliminaOnline(DestRegistradosTda,DestTdaSinOnline),
                                                         getUserOnline(ListaUser,UserOnline),getAmigosUser(UserOnline,Amigos),
                                                         dejaIguales(DestTdaSinOnline,Amigos,Lista),sort(Lista,ListaOut),
                                                         !.



ig(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], [], [8, 7, 1997], "online"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []]).

destino(["BobbyParra","ChiloParr","BobbyParra","caca","BenjaminParra","ChiloParra","ChiloParra"]).


/*
traduceUser(User):-getAmigosUser(User,ListaAmigos),lenght(ListaAmigos,L),L==0,
                   getPerfilUser(User,ListaPost),length(ListaPost,LP),LP==0,
                   getNameUser(User,Name),.*/
%Predicado que transforma el tda fecha a una representacion en un string
%DOM: tda fecha x Output
%REC: string
traduceFecha(Fecha,FechaString):-getDD(Fecha,DD),getMM(Fecha,MM),getYYYY(Fecha,YY),
                                atomics_to_string([DD, MM, YY], '/', FechaString) .

%atomics_to_string([1, 4, 2021], '/', A),display(A).
%

%Predicado que transforma el tda post a una representacion en un string
%
%
traducePost(Post,PostStr):-getUserPost(Post,NamePost),getNameUser(NamePost,Name),getDatePost(Post,FechaPost),
                           getContenidoPost(Post,Contenido),getUsersPost(Post,ListaUserPost),length(ListaUserPost,L),
                           L==0,
                           getIDPost(Post,PostID),
                           traduceFecha(FechaPost,FechaStr),
                           atomics_to_string(["ID:",PostID,"\n",
                                              "el d�a",FechaStr,Name,"public�:","\n",
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
                                              "el d�a",FechaStr,Name,"public�:","\n",
                                              '"',Contenido,'"',"\n",
                                              "Destinatarios:",UserDest,"\n"],' ',PostStr),!.


traducePost(Post,PostStr):-length(Post,L),L == 4,getUserPostShare(Post,NamePostShare),getNameUser(NamePostShare,Name),
                          getPostPostShare(Post,PostPostShare),postEnPalabra(PostPostShare,PostShareStr),
                          getFechaPostShare(Post,FechaPostShare),traduceFecha(FechaPostShare,FechaStr),
                          getUsersPostShare(Post,UsersPostShare),listaTdaUserToString(UsersPostShare,UserStr),
                          nth0(0,UserStr,UserDest),
                          atomics_to_string(["El d�a",FechaStr,Name,"comparti� con",UserDest,"la publicaci�n con",
                                            PostShareStr,"\n"],' ', PostStr),!.


traducePost(Post,PostStr):-length(Post,L),L == 4,getUserPostShare(Post,NamePostShare),getNameUser(NamePostShare,Name),
                          getPostPostShare(Post,PostPostShare),postEnPalabra(PostPostShare,PostShareStr),
                          getFechaPostShare(Post,FechaPostShare),traduceFecha(FechaPostShare,FechaStr),
                          getUsersPostShare(Post,UsersPostShare),length(UsersPostShare,L2),L2==0,

                          atomics_to_string(["El d�a",FechaStr,Name,"comparti� en su perfil","la publicaci�n con",
                                            PostShareStr,"\n"],' ', PostStr),!.


% Predicado que se utiliza para traducir un post cuando se intenta
% traducir un PostShare
% DOM: tda post, Output
% REC: String
postEnPalabra(Post,PostStr):-getUserPost(Post,NamePost),getNameUser(NamePost,Name),getDatePost(Post,FechaPost),
                           getContenidoPost(Post,Contenido),getUsersPost(Post,ListaUserPost),length(ListaUserPost,L),
                           L==0,
                           getIDPost(Post,PostID),
                           traduceFecha(FechaPost,FechaStr),
                           atomics_to_string(["ID:",PostID,"\n",
                                              "el d�a",FechaStr,Name,"public�:","\n",
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
                                              "el d�a",FechaStr,Name,"public�:","\n",
                                              '"',Contenido,'"',"\n",
                                              "Destinatarios:",UserDest],' ',PostStr),!.


%Predicado que se encarga de traducir la lista de post a un string
%DOM: Lista, "", Output
%REC: String
traduceListaPost([],String,String).
traduceListaPost([Post|PostCola],String,StrOut):-traducePost(Post,PostStr),atomics_to_string([String,PostStr],Str),
                                                 traduceListaPost(PostCola,Str,StrOut).

%Predicado que crea un tda PostShare
%DOM: tda user x tda fecha x tda post x lista string x output
%REC: tda PostShare
postShare(User,Fecha,Post,ListaAmigos,PostShare):-
                                                  PostShare=[User,Fecha,Post,ListaAmigos].
% Predicado de pertencia que verifica que un elemento sea
% del tipo tda postshare
%DOM: elemento
%REC: boolean
validaPostShare([User,Fecha,Post,ListaAmigos]):-validaUser(User),esFecha(Fecha),validaPost(Post),is_list(ListaAmigos).
/*############################################################Selectores PostShare##############################################*/

%Predicado que obtiene el usuario creador del postShare
%DOM: tda postShare x ouput
%REC: tda User
getUserPostShare(PostShare,UserPostShare):-validaPostShare(PostShare),nth0(0,PostShare,UserPostShare).


%Predicado que obtiene la fecha del postShare
%DOM: tda postShare x ouput
%REC: tda fecha
getFechaPostShare(PostShare,FechaPostShare):-validaPostShare(PostShare),nth0(1,PostShare,FechaPostShare).


%Predicado que obtiene el tda post del tda postShare
%DOM: tda postShare x ouput
%REC: tda post
getPostPostShare(PostShare,PostPostShare):-validaPostShare(PostShare),nth0(2,PostShare,PostPostShare).

%Predicado que obtiene la lista de usuarios (string) del tda postShare
%DOM: tda postShare x output
%REC: lista string
getUsersPostShare(PostShare,UsersPostShare):-validaPostShare(PostShare),nth0(3,PostShare,UsersPostShare).


%dejaListaDest(Sn1,ListaUserNameDest,ListaOut)
%
% Predicado que permite compartir una publicacion en el espacio propio o
% en el de un o varios amigos
% DOM: tda socialnetwork x tda fecha x int x lista string x output
% REC: tda socialnetwork
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
shareToAmigo(Sn1,Fecha,PostId,UserDest,Sn2):-validaSocialNetwork(Sn1),esFecha(Fecha),number(PostId),PostId >= 0,
                                                       getUsersSN(Sn1,Users),
                                                       getUserOnline(Users,UserOnline),getPostSN(Sn1,PostId,Post),
                                                       getUserConNombre(UserDest,Users,ReceptorTda),
                                                       esAmigo(UserOnline,ReceptorTda),
                                                       postShare(UserOnline,Fecha,Post,[ReceptorTda],PostShare),
                                                       agregaPostUser(UserOnline,PostShare,UserPost),
                                                       agregaPostSN(Sn1,PostShare,SNAUX),
                                                       getIndex(UserOnline,Users,0,Index),
                                                       %setEstadoUser(UserPost,"offline",UserOff),
                                                       setElemLista(Index,Users,UserPost,NewListaUser),
                                                       setUsersSN(SNAUX,NewListaUser,Sn2).

/*
                                                       ig(Sn1),publicaEnUno(Sn1,[6,6,2021],"Comelo",SN),socialNetworkLogin(SN,"BenjaminParra","benja123",SN2),socialNetworkShare(SN2,[1,2,2021],0,["BobbyParra","cac","ChiloParra"],SN3),getPerfilSN(SN3,Perfil),length(Perfil,L).*/

/*aplicaPublicaEnMuroUser(SnOut,_,_,[],SnOut):-!.
aplicaPublicaEnMuroUser(Sn1,Fecha,Texto,[X|Xs],SnOut):-validaSocialNetwork(Sn1),esFecha(Fecha),string_not_empty(Texto),
                                                      publicaEnMuroUser(Sn1,Fecha,Texto,X,SNAUX),
                                                      aplicaPublicaEnMuroUser(SNAUX,Fecha,Texto,Xs,SnOut).*/

%Predicado que comparte una publicacion en el muro de varios amigos
%Dom: tda socialnetwork x tda fecha x int x lista string x output
%Rec: tda socialnetwork
aplicaShareToAmigo(SnOut,_,_,[],SnOut):-!.
aplicaShareToAmigo(Sn1,Fecha,PostId,[X|Xs],Sn2):-validaSocialNetwork(Sn1),esFecha(Fecha),
                                                 shareToAmigo(Sn1,Fecha,PostId,X,SNAUX),
                                                 aplicaShareToAmigo(SNAUX,Fecha,PostId,Xs,Sn2).

/*ig(Sn1),publicaEnUno(Sn1,[6,6,2021],"Comelo",SN),socialNetworkLogin(SN,"BenjaminParra","benja123",SN2),shareToAmigo(SN2,[1,2,2021],0,"BobbyParra",SN3).*/



/*       #####################ojo para construir lista de post o users
                           atomics_to_string(["hola","\n","chao"],Str),write(Str).*/


%ig(Sn1),publicaEnUno(Sn1,[6,6,2021],"Comelo",SN),getPerfilSN(SN,Posts),nth0(0,Posts,Post),traducePost(Post,OUT).
%Usuario,Date,Contenido,ListaUsers,PostID,Post
/*
 ID: 0
El d�a 25/05/2021 �mmental� public�:
�este es un mensaje destinado a todos�
Destinatarios: Todos

*/

%Predicado que cambia el estado de un tda user a offline
%DOM: tda user x output
%REC: tda user
turnOffUser(User,UserOff):-validaUser(User),setEstadoUser(User,"offline",UserOff).


% Predicado que cambia el estado de cada tda user a offline perteneciente
% a una lista de tda user
%DOM: lista tda user x output
%REC: lista tda user

turnOffLista([],[]).
turnOffLista([User|UserCola],[UserOff|ColaOff]):-turnOffUser(User,UserOff),turnOffLista(UserCola,ColaOff).


% Predicado que cambia el estado de todos los usuarios de un tda
% socialnetwork a un estado offline
%DOM: tda socialnetwork x output
%REC: tda socialnetwork
%
turnOffSN(SN,SNOUT):-validaSocialNetwork(SN),getUsersSN(SN,ListaUser),turnOffLista(ListaUser,ListaOFF),
                      setUsersSN(SN,ListaOFF,SNOUT).
