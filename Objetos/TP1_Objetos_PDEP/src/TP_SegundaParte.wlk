class Musico {
	var habilidad
	var solista
	var albumes
	
	constructor(_habilidad, _solista, _albumes){
		habilidad = _habilidad
		solista = _solista
		albumes = _albumes
	}
	
	method esMinimalista(){
		return albumes.all{album => album.cancionesMenoresA(3)}
	}
}

class MusicoDeGrupo inherits Musico{
	var plusPorGrupo
	/*Setter */
	method plusPorGrupo(unValor){
		plusPorGrupo = unValor
	}
}

class VocalistaPopular inherits Musico{
	var palabraInspiradora
	/*Setter */
	method palabraInspiradora(unaPalabra){
		palabraInspiradora = unaPalabra
	}	
}

class MusicoUnico inherits Musico{
	var guitarraToca
	/*Setter */
	method guitarraToca(unaGuitarra) {
		guitarraToca = unaGuitarra
	}
	/*Getters */
	method guitarraToca() {
		return guitarraToca
	}
	
	override method habilidad() {
		if((guitarraToca.precio()*8) > 100) {
			return 100
		} else {
			return (guitarraToca.precio() *8)
		}
	}
}

class Album{
	var titulo
	var fechaLanzamiento
	var unidadesALaVenta
	var unidadesVendidas
	var canciones
	
	constructor(_titulo, _fechaLanzamiento, _unidadesALaVenta, _unidadesVendidas, _canciones){
		titulo = _titulo
		fechaLanzamiento = _fechaLanzamiento
		unidadesALaVenta = _unidadesALaVenta
		unidadesVendidas = _unidadesVendidas
		canciones = _canciones
	}
	
	method cancionesMenoresA(unValor){
		return canciones.all{cancion => cancion.duracionMenorA(unValor)}
	}
}

class Cancion{
	var titulo
	var letra
	var duracion
	
	constructor(_titulo, _letra, _duracion){
		titulo = _titulo
		letra = _letra
		duracion = _duracion
	}
	
	method duracionMenorA(unValor){
		return ((duracion/60) < unValor)
	}
}
