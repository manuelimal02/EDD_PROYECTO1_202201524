module modulo_lista_imagen
    implicit none
    type :: nodo_imagen
        integer :: imagen
        type(nodo_imagen), pointer :: siguiente => null()
    end type nodo_imagen
    type :: lista_imagen
        type(nodo_imagen), pointer :: cabeza => null()
        contains
        procedure :: insertar_imagen
        procedure :: imprimir_lista_imagen
    end type lista_imagen

    contains
    subroutine insertar_imagen(self, imagen)
        class(lista_imagen), intent(inout) :: self
        integer, intent(in) :: imagen
        type(nodo_imagen), pointer :: nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%imagen = imagen
        nuevo_nodo%siguiente => self%cabeza
        self%cabeza => nuevo_nodo
    end subroutine insertar_imagen

    subroutine imprimir_lista_imagen(self)
        class(lista_imagen), intent(in) :: self
        type(nodo_imagen), pointer :: actual
        actual => self%cabeza
        do while(associated(actual))
            print*, actual%imagen
            actual => actual%siguiente
        end do
    end subroutine imprimir_lista_imagen
end module modulo_lista_imagen

module modulo_lista_album
    use modulo_lista_imagen
    implicit none
    type :: nodo_album
        character(len=:), allocatable :: album
        type(lista_imagen) :: lista_imagenes
        type(nodo_album), pointer :: anterior, siguiente
    end type nodo_album
    type :: lista_album
        type(nodo_album), pointer :: cabeza => null()
        contains
        procedure :: insertar_album
        procedure :: imprimir_lista_album
    end type lista_album

    contains
    subroutine insertar_album(self, album, imagenes)
        class(lista_album), intent(inout) :: self
        character(len=*), intent(in) :: album
        type(lista_imagen), intent(in) :: imagenes
        type(nodo_album), pointer :: nuevo_nodo
    
        allocate(nuevo_nodo)
        nuevo_nodo%album = album
        nuevo_nodo%lista_imagenes = imagenes
        if (.not. associated(self%cabeza)) then
            self%cabeza => nuevo_nodo
            nuevo_nodo%siguiente => self%cabeza
            nuevo_nodo%anterior => self%cabeza
        else
            nuevo_nodo%siguiente => self%cabeza
            nuevo_nodo%anterior => self%cabeza%anterior
            self%cabeza%anterior%siguiente => nuevo_nodo
            self%cabeza%anterior => nuevo_nodo
        end if
    end subroutine insertar_album

    subroutine imprimir_lista_album(self)
        class(lista_album), intent(in) :: self
        type(nodo_album), pointer :: actual
    
        actual => self%cabeza
        do
            print*, "Album: ", actual%album
            print*, "Imagenes:"
            call actual%lista_imagenes%imprimir_lista_imagen()
            actual => actual%siguiente
            if (associated(actual, self%cabeza)) exit
        end do
    end subroutine imprimir_lista_album
end module modulo_lista_album
