module modulo_tabla_hash
    implicit none
    private
    integer :: tamano_tabla = 7
    integer, parameter :: porcentaje_maximo = 70

    type tecnico
        integer(8) :: dpi
        integer(8) :: telefono
        character(:), allocatable::nombre
        character(:), allocatable::apellido
        character(:), allocatable::genero
        character(:), allocatable::direccion
    end type tecnico

    type, public :: TablaHash
        integer :: elemento = 0
        type(tecnico), allocatable :: arreglo(:)
        contains
        procedure :: insertar
        procedure :: imprimir
        procedure :: buscar
        procedure, private :: resolver_colision
    end type TablaHash

contains
    subroutine insertar(self, dpi, nombre, apellido, direccion, telefono, genero)
        class(TablaHash), intent(inout) :: self
        type(TablaHash) :: nueva_tabla
        integer(8), intent(in) :: dpi, telefono
        character(:), allocatable::nombre, apellido, direccion, genero
        type(tecnico), allocatable :: arreglo_anterior(:)
        real :: porcentaje_utilizado
        integer(8) :: posicion

        if(.not. allocated(self%arreglo)) then
            allocate(self%arreglo(0:tamano_tabla-1))
            self%arreglo(:)%dpi = -1
        end if

        posicion = obtener_posicion(dpi)
        if(self%arreglo(posicion)%dpi /= -1 .and. self%arreglo(posicion)%dpi /= dpi) then
            call self%resolver_colision(posicion)
        end if

        self%arreglo(posicion)%dpi=dpi
        self%arreglo(posicion)%nombre=nombre
        self%arreglo(posicion)%apellido= apellido
        self%arreglo(posicion)%direccion=direccion
        self%arreglo(posicion)%telefono=telefono
        self%arreglo(posicion)%genero=genero
        self%elemento = self%elemento + 1
        porcentaje_utilizado = (self%elemento * 1.0/tamano_tabla) * 100
        if(porcentaje_utilizado > porcentaje_maximo) then
            arreglo_anterior = self%arreglo
            deallocate(self%arreglo)
            nueva_tabla = rehashing(arreglo_anterior)
            self%arreglo = nueva_tabla%arreglo
            self%elemento = nueva_tabla%elemento
        end if
    end subroutine insertar

    function rehashing(arreglo_anterior) result(nueva_tabla)
        type(tecnico), intent(in) :: arreglo_anterior(:)
        integer :: i
        type(TablaHash) :: nueva_tabla
        tamano_tabla = tamano_tabla*2
        allocate(nueva_tabla%arreglo(0:tamano_tabla-1))
        nueva_tabla%arreglo(:)%dpi = -1
        do i = 1, size(arreglo_anterior)
            if(arreglo_anterior(i)%dpi /= -1) then
            call nueva_tabla%insertar(arreglo_anterior(i)%dpi,arreglo_anterior(i)%nombre,&
            arreglo_anterior(i)%apellido, &
            arreglo_anterior(i)%direccion,&
            arreglo_anterior(i)%telefono, arreglo_anterior(i)%genero)
            end if
        end do
    end function rehashing

    subroutine resolver_colision(self, posicion)
        class(TablaHash), intent(inout) :: self
        integer(8), intent(inout) :: posicion
        do while(self%arreglo(posicion)%dpi /= -1)
            posicion = posicion + 1
            posicion = mod(posicion, tamano_tabla)
        end do
    end subroutine resolver_colision

    function obtener_posicion(dpi) result(posicion)
        integer(8), intent(in) :: dpi
        integer(8) :: posicion
        posicion = mod(dpi,tamano_tabla)
    end function obtener_posicion

    subroutine buscar(self, dpi)
        class(TablaHash), intent(inout) :: self
        integer(8), intent(in) :: dpi
        integer(8) :: posicion
        posicion = obtener_posicion(dpi)
        print '(a i0 a i0)' , "Posición: ", posicion, " Clave: ", self%arreglo(posicion)%dpi
    end subroutine buscar
    
    subroutine imprimir(self)
        class(TablaHash), intent(inout) :: self
        print '(a, i0)', "Tamaño de la tabla: ", tamano_tabla
        print '(a, i0)', "Elementos en la tabla: ", self%elemento
        print '(I0)', self%arreglo%dpi
    end subroutine imprimir
end module modulo_tabla_hash
