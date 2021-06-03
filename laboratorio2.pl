
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
%Comprueba que un string no sea vacio
%DOM:string
%REC:boolean

string_not_empty(String):- string_length(String,Cont) , Cont\=0.

%Verifica si un usuario es valido
%Dom: tda User
%Rec: boolean.
validaUser([Name,Password,Amigos,Perfil,Fecha,Estado]):-length([Name,Password,Amigos,Perfil,Fecha,Estado],L), L == 6,
   string_not_empty(Name),string_not_empty(Password),is_list(Amigos),is_list(Perfil),esFecha(Fecha), Estado == "offline",!;
   Estado == "online".


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


/*TDA POST*/
post(Usuario,Date,Contenido,Tipo,ListaUsers,PostID,Post):-validaUser(Usuario),esFecha(Date),publicacion(Contenido,Tipo,AUX1),
                                                     validaContenido(AUX1),is_list(ListaUsers)/*verificarListaUser*/,number(PostID),
                                                     Post = [Usuario,Date,Contenido,Tipo,ListaUsers,PostID].


/*publicaEnUno(Sn,User,Date,Contenido,Tipo,SNOut):-validaSocialNetwork(Sn),validaUser(User),esFecha(Date),validaContenido(Contenido),
                                            post(User,Date,Contenido,[],1 aqui debo hacer un length del lista post del user ).*/
%Arma el contenido de una Publicacion ejemplo "SoyUnaFoto photo"
%dom: string x string x OutPut
%rec: lista
publicacion(Contenido,Tipo,P):- P = [Contenido,Tipo].


%Valida el contenido de una publicacion
%dom: publicacion
%rec: boolean
validaContenido([Contenido,Tipo]):-string(Contenido), string(Tipo),
                                 string_not_empty(Contenido),string_not_empty(Tipo),
                                 Tipo == "text",!;
                                 Tipo == "audio",!;
                                 Tipo == "video",!;
                                 Tipo == "url",!;
                                 Tipo == "photo".
validaListaUser([],_).

validaListaUser([],[]):-true,!.
%validaListaUser(L,L).
validaListaUser([User|ColaUser],[User|Result]):-validaUser(User),validaListaUser(ColaUser,Result).
% validaListaUser([User|ColaUser],[User1|ColaUser]):-validaUser(User),validaListaUser(ColaUser,[User1,User|ColaUser]).
%
%
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
