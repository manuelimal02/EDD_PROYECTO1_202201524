module modulo_lista_imagen
    implicit none
    type :: nodo_imagen
        character(len=:), allocatable :: imagen
        type(nodo_imagen), pointer :: siguiente => null()
    end type nodo_imagen
    type :: lista_imagen
        type(nodo_imagen), pointer :: cabeza => null()
        contains
        procedure :: insertar_imagen
    end type lista_imagen

    contains
    subroutine insertar_imagen(self, imagen)
        class(lista_imagen), intent(inout) :: self
        character(len=*), intent(in) :: imagen
        type(nodo_imagen), pointer :: nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%imagen = imagen
        nuevo_nodo%siguiente => self%cabeza
        self%cabeza => nuevo_nodo
    end subroutine insertar_imagen

end module modulo_lista_imagen