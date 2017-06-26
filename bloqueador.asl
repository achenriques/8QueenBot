// Agent colocator in project mars.mas2j

/* Initial beliefs and rules */

aux(0).
bloquesActuales(0).	//Numero e bloques olocados
agujerosActuales(0).
libresBlancas(0,0,0).
libresNegras(0,0,0).

/* Initial goals */

!start.

/* Plans */

+!start <- !generarLibres.	//Empieza generando posiciones libres

+queen(X,Y) <-!eliminarLibres(X,Y).

+block(X,Y)[source(A)]: size(T) & bloquesActuales(B) & not A=percept <- 
	if(B<(T/4)){
		.wait(500);
		!comprobar(X,Y,A);
	}else{
		.print("No se pueden colocar más bloques");
		.send(A,tell,denie);
	}.

//+block(X,Y) <-!eliminarLibresB(X,Y).


+!generarLibres: size(T)<-
	for(.range(I,0,T-1))
	{
		for(.range(J,0,T-1))
		{
			+libre(J,I);
		}
	}.

+!comprobar(X,Y,A) : aux(B) & bloquesActuales(N) & size(T) <- // Prueba si es posible la colocacion del bloque
		if(aux<2){
			if(A==white & libre(X,Y)){
				.findall(pos(Col,Fil),(libre(Col,Fil)[source(self)]),ListaLibres);
				.length(ListaLibres,Li);
				-+libresBlanca(X,Y,Li);
				-+aux(N+1);
			};
			if(A==black){
				.findall(pos(Col,Fil),(libre(Col,Fil)[source(self)]),ListaLibres);
				.length(ListaLibres,Lj);
				-+libresBlanca(X,Y,Lj);
				-+aux(N+1);
			};
		}else{//Cuando aux es dos es qe contamos los dos jugadores y sus posiciones libres y atendemos a colocar las posiciones.
			-+aux(0);
			!ponerBloque;
		}.

+!ponerBloque:libresBlancas(A,B,C) & libresNegras(X,Y,Z) & bloquesActuales(N) <- 
	if(C>Z){
		block(A,B);
		!eliminarLibresB(X,Y);
		-+bloquesActuales(N+1);
		.send(white,tell,accept);
		.send(black,tell,denie);
	}
	if(Z>C){
		block(X,Y);
		!eliminarLibresB(X,Y);
		-+bloquesActuales(N+1);
		.send(white,tell,accept);
		.send(white,tell,denie);
	}
	if(C==Z){//Si no hay una funcion mejor que otra coloco en un hueco libre
		.send(white,tell,denie);
		.send(black,tell,denie);
		!colocarBloqueEnLibre(0,0);
	}.

// Intenta ocupar la primera posición libre. Si no es posible avanzo una columna hasta llegar al final.
+!colocarBloqueEnLibre(X,Y) : libre(X,Y) & size(T) <-
	block(X,Y);
	-+bloquesActuales(N+1);
	!eliminarLibresB(X,Y).
	

// Si llega a la última fila avanza a la siguiente fila. Si se comprueban todas las posiciones y no se pueden colocar mas bloques.
+!colocarBloqueEnLibre(X,Y) : size(T) <-
	if(X==T)
	{
		!colocarBloqueEnLibre(0,Y+1);
	}
	else
	{
		if(Y==T)
		{
			.print("No mas huecos para bloques");
		}
		else
		{
			!colocarBloqueEnLibre(X+1,Y);
		}
	}.
	
//Eliminar las casillas ocupadas o amenazadas(filas, columnas y diagonales respectivamente). Aqui se ejerce el control de amenazadas.
+!eliminarLibres(X,Y): size(T)<-
	-libre(X,Y);
	for(.range(I,0,T-1))
	{
		/*if(block(I,Y))
		{
			!eliminarLibresB(I,Y);
		}*/
		-libre(I,Y);
		for(.range(J,0,T-1))
		{
			/*if(block(X,J))
			{
				!eliminarLibresB(X,J);
			}*/
			-libre(X,J);
			if((I+J == X+Y) | (I-J == X-Y))
			{
				/*if(block(I,J))
				{
					!eliminarLibresB(I,J);
				}*/
				-libre(I,J);
			}
		}
	}.
	
+!eliminarLibresB(X,Y): size(T)<-
	-libre(X,Y);
	for(.range(I,0,T-1))	//Para Vertical
	{
		if(queen(X,I) & I<Y)
		{
			for(.range(J,Y+1,T-1))
			{
				+libre(X,J);
			}
		}
		if(queen(X,I) & I>Y)
		{
			for(.range(J,0,Y-1))
			{
				+libre(X,J);
			}
		}
	}
	
	for(.range(I,0,T-1))	//Para Horizontal
	{
		if(queen(I,Y) & I<X)
		{
			for(.range(J,X+1,T-1))
			{
				+libre(J,Y);
			}
		}
		if(queen(X,I) & I>X)
		{
			for(.range(J,0,X-1))
			{
				+libre(J,Y);
			}
		}
	}
	
	for(.range(I,0,T-1))	//Para Diagonales
	{
		for(.range(J,0,T-1))
		{
			if((I+J == X+Y) & I<X) //Superior
			{
				-+v1(X);	-+v2(Y);
				while(not v1(T-1) & not v2(T-1))	//Controlamos que las libres no se salgan del tablero
				{
					-+v1(X+1);	-+v2(Y+1);
					+libre(K,L) & v1(K) & v2(L);
				}
			}
			if((I+J == X+Y) & I>X) //Superior
			{
				-+v1(X);	-+v2(Y);
				while(not v1(0) & not v1(0))	//Controlamos que las libres no se salgan del tablero
				{
					-+v1(X-1);	-+v2(Y-1);
					+libre(K,L) & v1(K) & v2(L);	
				}
			}
			if((I-J == X-Y) & I<X) //Inferior	//Controlamos que las libres no se salgan del tablero
			{
				-+v1(X);	-+v2(Y);
				while(not v1(T-1) & not v2(0))
				{
					-+v1(X+1);	-+v2(Y-1);
					+libre(K,L) & v1(K) & v2(L);	
				}
			}
			if((I-J == X-Y) & I>X) //Inferior	//Controlamos que las libres no se salgan del tablero
			{
				-+v1(X);	-+v2(Y);
				while(not v1(0) & not v2(T-1))
				{
					-+v1(X-1);	-+v2(Y+1);
					+libre(K,L) & v1(K) & v2(L);	
				}
			}
		}
	}.



