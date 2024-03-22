module modulo_pixel
    implicit none
    private
    type :: nodo_pixel
        integer :: fila
        integer :: columna
        character(len=7) :: color_hexadecimal
        type(nodo_pixel), pointer :: siguiente => null()
    end type nodo_pixel
    type, public :: pixeles
        type(nodo_pixel), pointer :: cabeza => null()
    contains
        procedure :: insertar
        procedure :: imprimir
    end type pixeles
contains
    subroutine insertar(self, fila, columna, color_hexadecimal)
        class(pixeles), intent(inout) :: self
        integer, intent(in) :: fila, columna
        character(len=7), intent(in) :: color_hexadecimal
        type(nodo_pixel), pointer :: nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%fila = fila
        nuevo_nodo%columna = columna
        nuevo_nodo%color_hexadecimal = color_hexadecimal
        nuevo_nodo%siguiente => self%cabeza
        self%cabeza => nuevo_nodo
    end subroutine insertar
    !---
    subroutine imprimir(self)
        class(pixeles), intent(in) :: self
        type(nodo_pixel), pointer :: actual
        actual => self%cabeza
        do while(associated(actual))
            print*, "Fila: ", actual%fila, " Columna: ", actual%columna, " Color Hexadecimal: ", actual%color_hexadecimal
            actual => actual%siguiente
        end do
    end subroutine imprimir
end module modulo_pixel
!------------------------------------------------------------------------------------------------------------------------
module modulo_capa
    use modulo_pixel 
    implicit none
    private
    type :: nodo_capa
        integer :: id_capa
        type(pixeles) :: lista_pixeles
        type(nodo_capa), pointer :: siguiente => null()
    end type nodo_capa
    type, public :: capas
        type(nodo_capa), pointer :: cabeza => null()
    contains
        procedure :: insertar
        procedure :: imprimir
    end type capas
contains
    subroutine insertar(self, id_capa, lista_pixeles)
        class(capas), intent(inout) :: self
        integer, intent(in) :: id_capa
        type(pixeles), intent(in) :: lista_pixeles
        type(nodo_capa), pointer :: nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%id_capa = id_capa
        nuevo_nodo%lista_pixeles = lista_pixeles
        nuevo_nodo%siguiente => self%cabeza
        self%cabeza => nuevo_nodo
    end subroutine insertar
    !---
    subroutine imprimir(self)
        class(capas), intent(in) :: self
        type(nodo_capa), pointer :: actual
        actual => self%cabeza
        do while(associated(actual))
            print*, "ID Capa: ", actual%id_capa
            print*, "Lista de Pixeles: "
            call actual%lista_pixeles%imprimir()
            actual => actual%siguiente
        end do
    end subroutine imprimir
end module modulo_capa