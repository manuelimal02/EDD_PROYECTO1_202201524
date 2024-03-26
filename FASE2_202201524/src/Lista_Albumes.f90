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
        procedure :: graficar_album
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

    subroutine graficar_album(self, filename)
        class(lista_album), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer :: unit, contador, contador_imagenes
        type(nodo_album), pointer :: actual
        type(nodo_imagen), pointer :: actual_imagen
        character(len=:), allocatable :: filepath
        if (.not. associated(self%cabeza)) then
            print*,"Lista Albumes Vacia."
            return
        end if
        filepath = 'graph/' // trim(filename) 
        open(unit, file=filepath, status='replace')
        write(unit, *) 'digraph cola {node [fontname="Courier New"]'
        write(unit, *) 'node [shape=component, style=filled, color=blue, fillcolor="#65babf"];'
        actual => self%cabeza
        contador = 0
        write(unit, *) '"Node',contador,'" [shape=box3d, color=black, fillcolor="#d43440" label="',"Lista Albumes",'"];'
        do while (associated(actual))
            contador = contador + 1
            write(unit, *) '"Node', contador, '" [label="',actual%album,'"];'
            if (associated(actual%siguiente) .and. .not. associated(actual%siguiente, self%cabeza)) then
                write(unit, *) '"Node', contador,'" -> "Node',contador+1, '";'
                write(unit, *) '"Node', contador+1,'" -> "Node',contador, '";'
            end if
            actual_imagen => actual%lista_imagenes%cabeza
            contador_imagenes = 0
            if (associated(actual_imagen)) then
                contador_imagenes = contador_imagenes + 1
                write(unit, *) '"Node', contador, 'Imagen', contador_imagenes,&
                '" [shape=box, color=black, fillcolor="#FFCA33" label="',actual_imagen%imagen,'"];'
                write(unit, *) '"Node', contador,'" -> "Node',contador,'Imagen',contador_imagenes, '";'
                actual_imagen => actual_imagen%siguiente
                do while (associated(actual_imagen))
                    contador_imagenes = contador_imagenes + 1
                    write(unit, *) '"Node', contador, 'Imagen', contador_imagenes,&
                    '" [shape=box, color=black, fillcolor="#FFCA33" label="',actual_imagen%imagen,'"];'
                    write(unit, *) '"Node', contador, 'Imagen', contador_imagenes-1,& 
                    '" -> "Node',contador,'Imagen',contador_imagenes, '";'
                    actual_imagen => actual_imagen%siguiente
                    if (associated(actual_imagen, actual%lista_imagenes%cabeza)) exit
                end do
            end if
            actual => actual%siguiente
            if (associated(actual, self%cabeza)) exit
        end do 
        write(unit, *) '}'
        close(unit)
        call system('dot -Tpdf ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.pdf')
        call system('start ' // trim(adjustl(filepath)) // '.pdf')
        print *, "Grafica '"//trim(filename)//"' Generada Correctamente."
    end subroutine graficar_album
    
end module modulo_lista_album