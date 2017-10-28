class Musico {
	var habilidad
	var solista
	var albumes
	/*Setters */
	method habilidad(unaCantidad){
		habilidad = unaCantidad
	}	
	method solista(unValor){
		solista = unValor
	}
	method albumes(listaDeAlbumes){
		albumes = listaDeAlbumes
	}
	/*Getters */
	method habilidad(){
		return habilidad
	}
	method solista(){
		return solista
	}
	method albumesPublicados(){
		return albumes
	}
	
	method esMinimalista(){
		return albumes.all{album => album.cancionesMenoresA(3)}
	}
}

class MusicoDeGrupo inherits Musico{
	var plusPorGrupo
	
	constructor(_habilidad, _solista, _albumes, _plusPorGrupo){
		habilidad = _habilidad
		solista = _solista
		albumes = _albumes
		plusPorGrupo = _plusPorGrupo 
	}
}

class VocalistaPopular inherits Musico{
	var palabraInspiradora
	constructor(_habilidad, _solista, _albumes, _palabraInspiradora){
		habilidad = _habilidad
		solista = _solista
		albumes = _albumes
		palabraInspiradora = _palabraInspiradora
	}
}

object luisAlberto inherits Musico{
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
