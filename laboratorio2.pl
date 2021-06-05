
%Funcion constructora
%date(DD,MM,YYYY,Fecha)
%Permite Construir una fecha
%dom:int x int x int x Outpout
%rec: tda Fecha

fecha(DD,MM,YYYY,Fecha):-number(DD),DD=<31,MM=<12,YYYY > 0,DD > 0, YYYY >0 ,Fecha = [DD,MM,YYYY].



%Permite calcular si un número es par o no
%DOM: int
%REC: boolean
esPar(0):-!, true.
esPar(1):-!, false.
esPar(Numero):- Aux1 is Numero-2, esPar(Aux1).



%modulo(N,Out):- Out is rem(N,400).
/**/


%Permite saber si un elemento cualquiera es del tipo fecha
%dom: elemento cualquiera
%rec: boolean
esFecha([DD,MM,YYYY]):- number(DD), number(MM),number(YYYY),checkLargoFecha([DD,MM,YYYY])%verifica que sean numeros
                       ,getDiasMes([_,MM,YYYY],MaximoDias), MaximoDias >= DD.

checkLargoFecha(Fecha):- length(Fecha,Aux), Aux==3,!.
% Obtiene el numero de dias maximo que puede tener un mes considerando
% si es año bisiseto o no
% DOM: tda fecha, argumento de salid
% REC: int
 getDiasMes([_,MM,YYYY],MaximoDias):-
number(MM),number(YYYY),MM>0,MM<13, mesesPares([_,MM,YYYY],MaximoDias).
% getDiasMes(Fecha,MaximoDias):-getMM(Fecha,Mes),number(Mes),getYYYY(Fecha,Annio),number(Annio),Mes>0,Mes<13,
%
  %  mesesPares([_,Mes,Annio],MaximoDias).
%Obtiene el maximo de día si es que el mes es par o impar
%DOM: tda fecha, outpout
%REC: int
mesesPares([_,MM,_],MaximoDias):- MM\=2, esPar(MM),MaximoDias=31,!.
mesesPares([_,MM,_],MaximoDias):- not(esPar(MM)), MaximoDias=30,!.
mesesPares([_,MM,YYYY],MaximoDias):- MM == 2, esBisiesto([_,_,YYYY]),MaximoDias = 29,!;
                                       MaximoDias = 28.

%Permite saber si un año es bisiesto o no
%Dom: tda Fecha
%Rec: boolean
esBisiesto([_,_,YYYY]):- Aux1 is mod(YYYY,400), Aux1 == 0,!;
    Aux2 is mod(YYYY,4), Aux2 == 0,true, Aux3 is mod(YYYY,100), not(Aux3 == 0).

%Selectores
%Obtiene el día perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int
getDD([DD,MM,YYYY],Dia):- esFecha([DD,MM,YYYY]),DD = Dia.

%Obtiene el mes perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int
%
getMM([DD,MM,YYYY],Mes):- esFecha([DD,MM,YYYY]),MM = Mes.

%Obtiene el año perteneciente a un tda fecha
%DOM: tda fecha x outpout
%REC: int

getYYYY([DD,MM,YYYY],Annio):- esFecha([DD,MM,YYYY]),YYYY = Annio.

/*
length_list([],0).
length_list([_|Xs],C):-length_list(Xs,C_new),C is C_new+1.*/

/* TDA USER */
%Constructor del tda user
%DOM: string x string x tda date x Output
%REC: tda user
user(Name,Password,Fecha,Usuario):- string(Name),string(Password),%getDD(Fecha,DD),getMM(Fecha,MM),getYYYY(Fecha,YYYY),
   esFecha(Fecha), Usuario = [Name,Password,[],[],Fecha,"offline"].
%    getDiasMes(Fecha,MaximoDias),getDD(Fecha,Dia), MaximoDias>= Dia.
%
%Verifica si un usuario es valido
%Dom: tda User
%Rec: boolean.
validaUser([Name,Password,Amigos,Perfil,Fecha,Estado]):-length([Name,Password,Amigos,Perfil,Fecha,Estado],L), L == 6,
   string_not_empty(Name),string_not_empty(Password),is_list(Amigos),is_list(Perfil),esFecha(Fecha), Estado == "offline",!;
   Estado == "online".


%Comprueba que un string no sea vacio
%DOM:string
%REC:boolea
string_not_empty(String):- string_length(String,Cont) , Cont\=0.
/*#########################SELECTORES#######################*/

%Obtiene el Nombre del tda Usuario
%DOM: tda user
%REC: string
getNameUser(User,Name):-validaUser(User), nth0(0,User,Name).

%Obtiene la password del tda Usuario
%DOM: tda User
%REC: string
getPasswordUser(User,Password):-validaUser(User), nth0(1,User,Password).

%Obtiene la fecha de registro del tda Usuario
%DOM: tda User
%REC: tda fecha
%getFechaUser(User,Password):-validaUser(User), nth0(2,User,Password).



%Obtiene la lista de amigos del tda Usuario
%DOM: tda User
%REC: list tda user
getAmigosUser(User,Amigos):-validaUser(User), nth0(2,User,Amigos).
%[Name,Password,Amigos,Perfil,Fecha,Estado]
%

%Obtiene el perfil del tda Usuario
%DOM: tda User
%REC: lista tda post
getPerfilUser(User,Perfil):-validaUser(User), nth0(3,User,Perfil).


%Obtiene la fecha de registro del tda Usuario
%DOM: tda User
%REC: tda fecha
getFechaUser(User,Fecha):-validaUser(User), nth0(4,User,Fecha).

%Obtiene l estado del tda Usuario, apunta si es que está offline o no
%DOM: tda User
% REC: string
getEstadoUser(User,Estado):-validaUser(User), nth0(5,User,Estado).

/*########################### Funciones Modificadoras#######*/

%Se encarga de cambiar el estado de un tda usuario
%DOM: tda user, string
%REC: tda user
setEstadoUser(User,NewEstado,NewUser):- validaUser(User),string(NewEstado),setElemLista(5,User,NewEstado,NewUser).

%Se encarga de cambiar la lista de amigos de un tda usuario
%DOM: tda user, lista de amigos
%REC: tda user
% se agregó el "!" para que el validaListaUser no retornara un F
setListaAmigosUser(User,NewLista,NewUser):- validaUser(User),validaListaUser(NewLista,AUX),!,setElemLista(2,User,AUX,NewUser).




% Cambia un elemento de una lista dado un indicador asi entregando una
% nueva lista
%DOM: int x list x cualquier elemento x Output
%REC: List
setElemLista(Index,Lista,NewElem,ListaOut):-Index >= 0,length(Lista,L),L>0,Index =< L, nth0(Index,Lista,_,AUX),nth0(Index, ListaOut,NewElem,AUX).











/*TDA SOCIALNETWORK*/
%Representacion
%[NombreRedSocial,Fecha,ListaUser,ListaPost]



%Verifica si un string corresponde a los nombre de una red social
%dom: string
%rec: boolean
checkNameSN(Name):- Name == "facebook",!;
                    Name == "fb",!;
                    Name == "instagram",!;
                    Name == "ig",!;
                    Name == "twitter",!;
                    Name == "tw".



%Permite construir una tda socialnetwork
%DOM: string x tda fecha x Output
%Rec: tda socialNetwork

socialNetwork(Name,Date,SOut):- string(Name), esFecha(Date), checkNameSN(Name), SOut = [Name,Date,[],[]].

%Pertenencia
%Permite valida que un elemento sea un socialnetwork
%dom: elemento
%rec: boolean
validaSocialNetwork([Name,Date,ListaUser,ListaPost]):-length([Name,Date,ListaUser,ListaPost],L), L==4,checkNameSN(Name),
                                                       esFecha(Date),is_list(ListaUser),is_list(ListaPost).

%["Facebook",[1,3,2021],[],[]]
/*############################Selectores Tda SocialNewtwork####################*/
%Entrega el nombre un tda socialnetwork
%DOM: tda SN
%REC: string
getNameSN(SN,Name):-validaSocialNetwork(SN), nth0(0,SN,Name).

%Entrega el nombre un tda socialnetwork
%DOM: tda SN
%REC: tda Fecha
getDateSN(SN,Fecha):-validaSocialNetwork(SN), nth0(1,SN,Fecha1), esFecha(Fecha1),Fecha1 = Fecha.

%Entrega lista User un tda socialnetwork
%DOM: tda SN
% REC: list
%  getUsersSN( ["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []],USERS).
/* ------------------------------------------------------------------------------------------------------------>! aqui es un or###*/
getUsersSN(SN,ListaUser):-validaSocialNetwork(SN), nth0(2,SN,ListaUser1),length(ListaUser1,L),L==0,ListaUser = [],!;validaSocialNetwork(SN), nth0(2,SN,ListaUser1),
validaListaUser(ListaUser1,Out),ListaUser=Out,!.

%Cambia la lista de Usuarios por otra nueva
%DOM: tda socialnetork,lista de elementos, output
%REC: tda socialnetwork
setUsersSN(SN,NewUsers,NewSN):-validaSocialNetwork(SN),setElemLista(2,SN,NewUsers,NewSN).




/*TDA POST*/
%Contruye el tda Post
%DOM: tda user x tda date x string x lista tda user x int x Output
%REC: tda post
% post(["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],[04,06,2021],"Esto es un texto",[["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]],1,Post).
post(Usuario,Date,Contenido,ListaUsers,PostID,Post):-validaUser(Usuario),esFecha(Date),
                                                     validaContenido(Contenido),is_list(ListaUsers),/*verificarListaUser*/number(PostID),
                                                     Post = [Usuario,Date,Contenido,ListaUsers,PostID].


/*publicaEnUno(Sn,User,Date,Contenido,Tipo,SNOut):-validaSocialNetwork(Sn),validaUser(User),esFecha(Date),validaContenido(Contenido),
post(User,Date,Contenido,[],1 aqui debo hacer un length del lista post del user ).*/

%Comprueba si un elemento cualquiera es tda post es valido o no
%dom: cualquier elemento
%rec: boolean
validaPost([Usuario,Date,Contenido,ListaUsers,PostID]):- validaUser(Usuario),esFecha(Date),
                                                     validaContenido(Contenido),is_list(ListaUsers),validaListaUser(ListaUsers,Aux_Users),!,is_list(Aux_Users),number(PostID).

/*##########################SELECTORES TDA POST##################################*/
%Selecciona el usuario creador del tda post
%DOM: tda Post x output
%REC: tda user
getUserPost(Post,User):-validaPost(Post),nth0(0,Post,User).

%Selecciona la fecha de creacion del post
%DOM: tda post x output
%REC: tda fecha
getDatePost(Post,Date):-validaPost(Post),nth0(1,Post,Date).

%Selecciona el contenido del post
%DOM: tda post x output
%REC: string
getContenidoPost(Post,Contenido):-validaPost(Post),nth0(2,Post,Contenido).

%Selecciona a lista de destinarios del post
%DOM: tda post x output
%REC: lista tda user
getUsersPost(Post,Users):-validaPost(Post),nth0(3,Post,Users).

%Selecciona el identificador del post
%DOM: tda post x output
%REC: int
getIDPost(Post,IDPOST):-validaPost(Post),nth0(4,Post,IDPOST).


%Valida el contenido de una publicacion
%dom: contenido
%rec: boolean
validaContenido(Contenido):-string(Contenido),string_not_empty(Contenido).

% Valida que una lista contenga solo elementos Tda Usuarios validos y
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

%Se encarga de registrar un usuaio en
%
%
socialNetworkRegister(Sn1,_,_,_,Sn2):-Sn1 == Sn2,!.
socialNetworkRegister(Sn1,Fecha,Username,Password,Sn2):-validaSocialNetwork(Sn1),esFecha(Fecha),verificaUserName(Username),
                                                         verificaPassword(Password),not(checkRegister(Username,Sn1)),
                                                         user(Username,Password,Fecha,User),getUsersSN(Sn1,Users),
                                                         append_final(Users,User,NewUsers),setUsersSN(Sn1,NewUsers,Sn2).%,Sn2 = OUT.

/*
 * Prueba que retorna true
socialNetworkRegister(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []],[04,06,1992],"BobbyParra","bobby123",["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"],["BobbyParra","bobby123",[],[],[04,06,1992],"offline"]], []]).

* Prueba que retorna el Sn2
socialNetworkRegister(["instagram", [29, 2, 1992], [["BenjaminParra", "benja123", [], [], [8, 7, 1997], "offline"],["ChiloParra", "chilo123", [], [], [28, 1, 1987], "offline"]], []],[04,06,1992],"BobbyParra","bobby123",SN).
                                                       */

%Verifica que un nombre de Usuario sea no vacio y mayor a 6
%DOM: string
%REC: boolean
verificaUserName(Username):-string_not_empty(Username),string_length(Username,Cont),Cont >=6.

% Verifica que una password sea no vacia, mayor a 8 y que contenga al
% menos 2 numeros
%DOM: string
%REC: boolean
verificaPassword(Password):-string_not_empty(Password),string_length(Password,Cont),Cont >=8,
                           lista_numeros(Lista_Numeros),tiene_numeros(Password,Lista_Numeros).

% Verifica si un Username esta en la lista de usuarios de socialnetwork,
%DOM: string x tda socialnetwork
%REC: boolean
checkRegister(Username,SN):-getUsersSN(SN,Users),getListaNames(Users,ListaString),member(Username,ListaString),!.

%Entrega los nombres (strings) de una lista de usuarios
%DOM: lista tda user x output
%REC: lista string
getListaNames([],[]):-true,!.
getListaNames([X|Xs],[Name|Ys]):- getNameUser(X,Name),getListaNames(Xs,Ys).


%es una lista de numeros en Ascii
lista_numeros([49, 50, 51, 52, 53, 54, 55, 56, 57]).

%Comprueba que un string tenga al menos 2 numeros
%dom: string x lista numeros en ascii
%rec: boolean
tiene_numeros(String,Lista_Numeros):- string_to_list(String,List),comparten_elementos(List,Lista_Numeros,0,Cont),Cont >= 2.
%Entrega cuantas veces un elemento está contenido en una lista
%DOM: Elemento x Lista x 0 x Output
%REc: int
existe_en_lista(_,[],Out,Out):-true,!.
existe_en_lista(Cabeza1,[Cabeza2|Cola2],CONT,AUX):- Cabeza1 == Cabeza2,Cont1 is CONT+1,existe_en_lista(Cabeza1,Cola2,Cont1,AUX).
existe_en_lista(Cabeza1,[Cabeza2|Cola2],CONT,AUX):- Cabeza1\==Cabeza2,existe_en_lista(Cabeza1,Cola2,CONT,AUX).


%Entrega cuantos elementos en comun tiene una lista con otra
%DOM: Lista x Lista x 0 x Output
%REC: int
comparten_elementos([],Lista,AUX,Out):-length(Lista,L), L=\=0, Out is 0+AUX,!.
comparten_elementos([Cabeza1|Cola1],[Cabeza2|Cola2],CONT,AUX):-existe_en_lista(Cabeza1,[Cabeza2|Cola2],0,CONT1),!,CONTAUX is CONT1+CONT, comparten_elementos(Cola1,[Cabeza2|Cola2],CONTAUX,AUX).




%Agrega un elemento al final de una lista
%DOM: lista x elemento x output
%REC: lista
append_final([],Elem,[Elem]).
append_final([Cabeza1|Cola1],Elem,[Cabeza1|Cola2]):-append_final(Cola1,Elem,Cola2).
