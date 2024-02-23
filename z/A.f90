module modulo_lista_ventanilla
    implicit none
    private
    type, public :: nodo_lista_ventanilla
        private
        integer :: numero_ventanilla
        character(len=:), allocatable :: id_cliente
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: img_grande
        character(len=:), allocatable :: img_pequena
        type(nodo_lista_ventanilla), pointer :: siguiente   
    end type nodo_lista_ventanilla

    type, public :: lista_ventanilla
        private
        type(nodo_lista_ventanilla), pointer :: cabeza => null()
    contains
        procedure :: insertar_ventanilla
    end type lista_ventanilla

contains
    subroutine insertar_ventanilla(this, numero_ventanilla, id_cliente, nombre, img_grande, img_pequena)
        class(lista_ventanilla), intent(inout) :: this

        integer, intent(in) :: numero_ventanilla
        character(len=*), intent(in) :: id_cliente, nombre, img_grande, img_pequena
        
        type(nodo_lista_ventanilla), pointer :: temp
        type(nodo_lista_ventanilla), pointer :: actual

        temp%numero_ventanilla = numero_ventanilla
        temp%id_cliente = id_cliente
        temp%nombre = nombre
        temp%img_grande = img_grande
        temp%img_pequena = img_pequena

        temp%siguiente => null()
        if (.not. associated(this%cabeza)) then
            this%cabeza => temp
        else
            actual => this%cabeza
            do while (associated(actual%siguiente))
                actual => actual%siguiente
            end do
            actual%siguiente => temp
        end if
        print *, "Ventanilla Creada: ", numero_ventanilla
        print *, id_cliente
        print *, nombre
        print *, img_grande
        print *, img_pequena
    end subroutine insertar_ventanilla
end module modulo_lista_ventanilla

program main
    use modulo_lista_ventanilla
    
    type(lista_ventanilla) :: lista

    call lista%insertar_ventanilla(1, "cliente1", "Ventanilla A", "imagenA_grande", "imagenA_pequena")
    call lista%insertar_ventanilla(2, "cliente2", "Ventanilla B", "imagenB_grande", "imagenB_pequena")


end program main
