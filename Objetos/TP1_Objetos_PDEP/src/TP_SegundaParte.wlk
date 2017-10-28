class Musico {
	var habilidad
	var solista
	var albumes = []
	/*Setters */
	method habilidad(unaCantidad){
		habilidad = unaCantidad
	}	
	method solista(unValor){
		solista = unValor
	}
	method agregarAlbum(album){
		albumes.add(album)
	}
	method agregarMuchosAlbumes(listaDeAlbumes){
		albumes.addAll(listaDeAlbumes)
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
	
	// Funcionalidad 1
	method esMinimalista(){
		return albumes.all({album => album.cancionesMenoresA(3)})
	}
	// Funcionalidad 2
	method cancionesConPalabra(unaPalabra){
		return (albumes.map({album => album.cancionesConPalabra(unaPalabra)})).flatten()
	}
	// Funcionalidad 3
	method segundosDeObra(){ 
		return (albumes.map({album => album.duracionAlbum()})).sum()
	}
	// Funcionalidad 5
	method laPego(){
		var porcentajes = albumes.map({album => album.porcentajeVendido()})
		return ((porcentajes.sum())/porcentajes.size() > 75)
	}
}

class MusicoDeGrupo inherits Musico{
	var plusPorGrupo
	
	constructor(_habilidad, _plusPorGrupo){
		habilidad = _habilidad
		plusPorGrupo = _plusPorGrupo 
	}
	
	method interpretaBien(unaCancion) {
		return (unaCancion.duracion() > 300)
	}
	
	method cobra(unaPresentacion) {
		if(unaPresentacion.tocaSolo()){
			return 100 * unaPresentacion.duracion()
		} else {
			return 50+plusPorGrupo
		}		
	}
	override method habilidad() {
		if(solista) {
			return habilidad
		} else {
			return (habilidad + 5)
		}
	}
}

class VocalistaPopular inherits Musico{
	var palabraInspiradora
	
	constructor(_habilidad, _palabraInspiradora){
		habilidad = _habilidad
		palabraInspiradora = _palabraInspiradora
	}
	
	method interpretaBien(unaCancion) {
		return (unaCancion.tienePalabra(palabraInspiradora))
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
	
	override method habilidad() {
		if(solista) {
			return habilidad
		} else {
			return (habilidad - 20)
		}
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
	
	method interpretaBien () = true
	method cobra(unaPresentacion) {
		if(self.anterior(unaPresentacion)) {
			return 1000
		} else {
			return 1200
		}
	}
	method anterior(unaPresentacion) {
		return unaPresentacion.fecha().year() == 2017 && unaPresentacion.fecha().month() < 9
	}
	
	override method habilidad() {
		if((guitarraToca.precio()*8) > 100) {
			return 100
		} else {
			return (guitarraToca.precio() *8)
		}
	}
}

object fender {
	var precio = 10
	/*Setter */
	method precio(unPrecio) {
		precio = unPrecio
	}
	/*Getter */
	method precio () {
		return precio
	}
}

object gibson {
	var estaSana = true
	/*Setter */
	method estaSana(unValor) {
		estaSana = unValor
	}
	/*Getter */
	method estaSana() {
		return estaSana
	}
	
	method precio() {
		if(estaSana) {
			return 15
		} else {
			return 5
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
		return canciones.all({cancion => cancion.duracionMenorA(unValor)})
	}
	
	method porcentajeVendido(){
		return (unidadesVendidas*100)/unidadesALaVenta
	}
	method cancionesConPalabra(unaPalabra){
		var listaCanciones = []
		canciones.forEach({cancion =>
			if(cancion.tienePalabra(unaPalabra)){
				listaCanciones.add(cancion)
			}
		})
		return listaCanciones
	}
	
	method duracionAlbum(){
		return (canciones.map({cancion => cancion.duracion()})).sum()
	}
//	Funcionalidad 4
	method cancionMasLarga(){
		return canciones.max({cancion => cancion.largoCancion()})
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
	/*Getter */
	method duracion(){
		return duracion
	}
	
	method tienePalabra(unaPalabra) {
		return (letra.contains(unaPalabra))
	}
	
	method duracionMenorA(unValor){
		return ((duracion/60) < unValor)
	}
	
	method largoCancion(){
		return letra.size()
	}
}

class Presentacion {
	var fecha
	var lugar
	var cantantes = []
	var duracion
	/*Setters */
	method fecha(unaFecha) {
		fecha = unaFecha
	}
	method lugar(unLugar) {
		lugar = unLugar
	}
	method duracion(unaDuracion) {
		duracion = unaDuracion
	}
	/*Getters */
	method fecha() {
		return fecha
	}
	method lugar() {
		return lugar
	}
	method duracion() {
		return duracion
	}

	method capacidad() {
		return lugar.capacidad()
	}	
	method agregarCantante(unCantante) {
		cantantes.add(unCantante)	
	}
	method tocaSolo() {
		return (cantantes.size() == 1)
	}
	method calcularCosto() {
		return cantantes.sum({cantante => cantante.cobra(self)})
	}
}

class Lugar {
	var capacidad
	var nombre
	/*Setter */
	method capacidad(unaPresentacion) {
		if(nombre.contains("Luna Park")) {
			capacidad = 9290
		} else {
			self.calcularCapacidad(unaPresentacion)
		}
	}
	method nombre(unNombre) {
		nombre = unNombre
	}
	/*Getter */
	method capacidad() {
		return capacidad
	}
	method nombre() {
		return nombre
	}
	
	method calcularCapacidad(unaPresentacion) {
		if(unaPresentacion.fecha().dayOfWeek() == 6) {
			capacidad =  700
		} else {
			capacidad = 400
		}	
	}	
}
