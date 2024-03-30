module modulo_lista_cliente
    use modulo_arbol_abb_c
    use modulo_arbol_avl_c
    use md
    implicit none
    type :: nodo_cliente
        character(len=:), allocatable :: dpi
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: contrasena
        type(arbol_abb) :: arbol_abb_capa
        type(arbol_avl) :: arbol_avl_imagen
        type(lista_album) :: lista_doble_album
        type(nodo_cliente), pointer :: siguiente => null()
    end type nodo_cliente

    type :: lista_cliente
        type(nodo_cliente), pointer :: cabeza => null()
    contains
        procedure :: insertar_cliente
        procedure :: eliminar_cliente
        procedure :: modificar_cliente
        procedure :: grafica_cliente
        procedure :: cliente_existe
        procedure :: iniciar_sesion_c
        procedure :: obtener_cliente
        procedure :: mostrar_cliente
        procedure :: reporte_albumes_cliente
        procedure :: reporte_imagenes_cliente
        procedure :: reporte_capas_cliente
        procedure :: reporte_listar_cliente
    end type lista_cliente

contains
    subroutine insertar_cliente(self, dpi, nombre, contrasena)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi, nombre, contrasena
        type(nodo_cliente), pointer :: nuevo_cliente
        logical :: existe
        existe = self%cliente_existe(dpi)
        if (existe) then
            print*, "Usuario: ",trim(dpi)," Ya Esta Registrado."
            return
        end if
        allocate(nuevo_cliente)
        nuevo_cliente%dpi = dpi
        nuevo_cliente%nombre = nombre
        nuevo_cliente%contrasena = contrasena
        nuevo_cliente%siguiente => self%cabeza
        self%cabeza => nuevo_cliente
        print*, "Usuario: ",trim(dpi)," Registrado Correctamente."
    end subroutine insertar_cliente

    function cliente_existe(self, dpi) result(existe)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi
        logical :: existe
        type(nodo_cliente), pointer :: actual
        existe = .false.
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                existe = .true.
                exit
            end if
            actual => actual%siguiente
        end do
    end function cliente_existe

    subroutine modificar_cliente(self, dpi, nombre, contrasena)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi, nombre, contrasena
        type(nodo_cliente), pointer :: actual
        logical :: existe
        existe = self%cliente_existe(dpi)
        if (.not. existe) then
            print*, "Cliente: ",trim(dpi)," No Esta Registrado."
            return
        end if
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                if (nombre /= '') actual%nombre = nombre
                if (contrasena /= '') actual%contrasena = contrasena
                exit
            end if
            actual => actual%siguiente
        end do
        print*, "Usuario: ",trim(dpi)," Modificado Correctamente."
    end subroutine modificar_cliente

    subroutine eliminar_cliente(self, dpi)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi
        type(nodo_cliente), pointer :: actual, previo
        logical :: existe
        existe = self%cliente_existe(dpi)
        if (.not. existe) then
            print*, "Cliente: ",trim(dpi)," No Esta Registrado."
            return
        end if
        actual => self%cabeza
        previo => null()
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                if (associated(previo)) then
                    previo%siguiente => actual%siguiente
                else
                    self%cabeza => actual%siguiente
                end if
                deallocate(actual)
                exit
            end if
            previo => actual
            actual => actual%siguiente
        end do
        print*, "Cliente: ",trim(dpi)," Eliminado Correctamente."
    end subroutine eliminar_cliente

    function iniciar_sesion_c(self, dpi, contrasena) result(coincide)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi, contrasena
        logical :: coincide
        type(nodo_cliente), pointer :: actual
        coincide = .false.
        if (.not. self%cliente_existe(dpi)) then
            print*, "Usuario: ",trim(dpi)," No Esta Registrado."
            print*,"-----"
            return
        end if
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi) .and. trim(actual%contrasena) == trim(contrasena)) then
                coincide = .true.
                exit
            end if
            actual => actual%siguiente
        end do
    end function iniciar_sesion_c

    function obtener_cliente(self, dpi) result(nodo)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi
        type(nodo_cliente), pointer :: nodo
        type(nodo_cliente), pointer :: actual
        nodo => null()
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                nodo => actual
                exit
            end if
            actual => actual%siguiente
        end do
    end function obtener_cliente

    subroutine mostrar_cliente(self, dpi)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi
        type(nodo_cliente), pointer :: actual
        logical :: existe
        existe = self%cliente_existe(dpi)
        if (.not. existe) then
            print*, "Cliente: ",trim(dpi)," No Esta Registrado."
            return
        end if
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                print*, "Datos Del Cliente:"
                print *, "---------------------------------------"
                print*, "Nombre: ", trim(actual%nombre)
                print*, "DPI: ", trim(actual%dpi)
                print*, "Password: ", trim(actual%contrasena)
                exit
            end if
            actual => actual%siguiente
        end do
    end subroutine mostrar_cliente

    subroutine reporte_albumes_cliente(self, dpi)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi
        type(nodo_cliente), pointer :: actual
        logical :: existe
        existe = self%cliente_existe(dpi)
        if (.not. existe) then
            print*, "Cliente: ",trim(dpi)," No Esta Registrado."
            return
        end if
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                print *, "Albumes Del Cliente: ", trim(dpi)
                print *, "---------------------------------------"
                call actual%lista_doble_album%imprimir_albumes()
                exit
            end if
            actual => actual%siguiente
        end do
    end subroutine reporte_albumes_cliente

    subroutine reporte_imagenes_cliente(self, dpi)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi
        type(nodo_cliente), pointer :: actual
        logical :: existe
        existe = self%cliente_existe(dpi)
        if (.not. existe) then
            print*, "Cliente: ",trim(dpi)," No Esta Registrado."
            return
        end if
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                print *, "Imagenes Del Cliente: ", trim(dpi)
                print *, "---------------------------------------"
                call actual%arbol_avl_imagen%cantidad_imagenes()
                print *, "---------------------------------------"
                exit
            end if
            actual => actual%siguiente
        end do
    end subroutine reporte_imagenes_cliente

    subroutine reporte_capas_cliente(self, dpi)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: dpi
        type(nodo_cliente), pointer :: actual
        logical :: existe
        existe = self%cliente_existe(dpi)
        if (.not. existe) then
            print*, "Cliente: ",trim(dpi)," No Esta Registrado."
            return
        end if
        actual => self%cabeza
        do while (associated(actual))
            if (trim(actual%dpi) == trim(dpi)) then
                print *, "Capas Del Cliente: ", trim(dpi)
                print *, "---------------------------------------"
                call actual%arbol_abb_capa%cantidad_capas()
                print *, "---------------------------------------"
                exit
            end if
            actual => actual%siguiente
        end do
    end subroutine reporte_capas_cliente

    subroutine reporte_listar_cliente(self)
        class(lista_cliente), intent(inout) :: self
        type(nodo_cliente), pointer :: actual
        if (.not. associated(self%cabeza)) then
            print*, "No hay clientes registrados."
            return
        end if
        actual => self%cabeza
        do while (associated(actual))
            print *, "---------------------------------------"
            print *, "Nombre del Cliente: ", trim(actual%nombre)
            print *, "DPI del Cliente: ", trim(actual%dpi)
            print *, "Imagenes Del Cliente: "
            print *, "----------"
            call actual%arbol_avl_imagen%cantidad_imagenes()
            actual => actual%siguiente
        end do
    end subroutine reporte_listar_cliente
    

    subroutine grafica_cliente(self, filename)
        class(lista_cliente), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer :: unit
        integer :: contador
        type(nodo_cliente), pointer :: actual
        character(len=:), allocatable :: filepath
        if (.not. associated(self%cabeza)) then
            print*,"Lista De Clientes Vacia."
            return
        end if
        filepath = 'graph/' // trim(filename) 
        open(unit, file=filepath, status='replace')
        write(unit, *) 'digraph cola {node [fontname="Courier New"]'
        write(unit, *) '    node [shape=component, style=filled, color=blue, fillcolor="#65babf"];'
        actual => self%cabeza
        contador = 0
        write(unit, *) '"Node',contador,'"[shape=box3d, color=black, fillcolor="#d43440" label="',"Lista Clientes",'"];'
        do while (associated(actual))
            contador = contador + 1
            write(unit, *) '    "Node', contador, '" [label="', &
                                "DPI: ", trim(actual%dpi), "\n", &
                                "Nombre: ", trim(actual%nombre), "\n", &
                                "Password: ",trim(actual%contrasena),'"];'
            if (associated(actual%siguiente)) then
                write(unit, *) '    "Node', contador, '" -> "Node', contador+1, '";'
            end if
            actual => actual%siguiente
        end do 
        write(unit, *) '}'
        close(unit)
        call system('dot -Tpdf ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.pdf')
        call system('start ' // trim(adjustl(filepath)) // '.pdf')
        print *, "Grafica '"//trim(filename)//"' Generada Correctamente."
    end subroutine grafica_cliente

end module modulo_lista_cliente