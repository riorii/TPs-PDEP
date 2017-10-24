object joaquin {
	var solista = true
	/*Setter */
	method solista(unValor) {
		solista = unValor
	}
	/*Getter */
	method solista() {
		return solista
	}

	method habilidad() {
		if(solista) {
			return 20
		} else {
			return 50
		}
	}
	
	method interpretaBien(unaCancion) {
		return (unaCancion.duracion() > 300)
	}
	
	method cobra(unaPresentacion) {
		if(solista){
			return 100 * unaPresentacion.duracion()
		} else {
			return 50
		}		
	}

}

object lucia {
	var solista = true
	/*Setter */
	method solista(unValor) {
		solista = unValor
	}
	/*Getter */
	method solista() {
		return solista
	}
	
	method habilidad() {
		if(solista) {
			return 70
		} else {
			return (self.habilidad() - 20)
		}
	}
	
	method interpretaBien(unaCancion) {
		return (unaCancion.tienePalabra("familia"))
	}
	
	method cobra(unaPresentacion) {
		if(self.lugarConcurrido(unaPresentacion)){
			return 500
		} else {
			return 400
		}		
	}
	
	method lugarConcurrido(unaPresentacion) {
		return unaPresentacion.capacidad() > 5000	
	}
	
}

object luisAlberto {
	var guitarraToca
	/*Setter */
	method guitarraToca(unaGuitarra) {
		guitarraToca = unaGuitarra
	}
	/*Getters */
	method guitarraToca() {
		return guitarraToca
	}
	
	method habilidad() {
		if((guitarraToca.precio()*8) > 100) {
			return 100
		} else {
			return (guitarraToca.precio() *8)
		}
	}
	
	method interpretaBien () = true
		
	method cobra(unaPresentacion) {
		if(self.anterior(unaPresentacion)) {
			return 100
		} else {
			return 1200
		}
	}
	
	method anterior(unaPresentacion) {
		return unaPresentacion.fecha().year() == 2017 && unaPresentacion.fecha().month() < 9
	}
}

class Canciones {
	var duracion
	var letra
	/*Setters */
	method duracion(unaDuracion) {
		duracion = unaDuracion
	}
	method letra(unaLetra) {
		letra = unaLetra
	}
	/*Getters */
	method duracion() {
		return duracion
	}
	method letra() {
		return letra
	}
}

class Presentacion {
	var fecha
	var lugar
	var cantantes = []
	/*Setters */
	method fecha(unaFecha) {
		fecha = unaFecha
	}
	method lugar(unLugar) {
		lugar = unLugar
	}
	/*Getters */
	method fecha() {
		return fecha
	}
	method lugar() {
		return lugar
	}
}

class Lugar {
	var capacidad
	/*Setter */
	method capacidad(unaCapacidad) {
		capacidad = unaCapacidad
	}
	/*Getter */
	method capacidad() {
		return capacidad
	}
}