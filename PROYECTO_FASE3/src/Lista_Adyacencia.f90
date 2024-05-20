module modulo_lista_adyacencia
    implicit none
    private

    type :: nodo_conexion
        integer :: num_sucursal
        integer :: distancia
        integer :: impresora
        type(nodo_conexion), pointer :: siguiente => null()
    end type nodo_conexion

    type :: nodo_grafo
        integer :: num_sucursal_conexion
        type(nodo_grafo), pointer :: siguiente => null()
        type(nodo_grafo), pointer :: anterior => null()
        type(nodo_conexion), pointer :: lista_conexion => null()
    contains
        procedure :: agregar_conexion
    end type nodo_grafo

    type, public :: lista_adyacencia
        type(nodo_grafo), pointer :: cabeza => null()
        type(nodo_grafo), pointer :: cola => null()
    contains
        procedure :: insertar_nodo
        procedure :: generar_grafo
    end type lista_adyacencia

    contains
    subroutine insertar_nodo(self, num_sucursal_conexion, num_sucursal, distancia, impresora)
        class(lista_adyacencia), intent(inout) :: self
        integer, intent(in) :: num_sucursal
        integer, intent(in) :: num_sucursal_conexion
        integer, intent(in) :: distancia
        integer, intent(in) :: impresora
        type(nodo_grafo), pointer :: nodo_auxiliar
        type(nodo_grafo), pointer :: nodo_nuevo
        allocate(nodo_nuevo)
        if(.not. associated(self%cabeza)) then
            allocate(nodo_auxiliar)
            nodo_auxiliar%num_sucursal_conexion = num_sucursal_conexion
            self%cabeza => nodo_auxiliar
            self%cola => nodo_auxiliar
            call nodo_auxiliar%agregar_conexion(num_sucursal, distancia, impresora)
        else
            if(num_sucursal_conexion < self%cabeza%num_sucursal_conexion) then
                self%cabeza%anterior => nodo_nuevo
                nodo_nuevo%siguiente => self%cabeza
                self%cabeza => nodo_nuevo
                nodo_nuevo%num_sucursal_conexion = num_sucursal_conexion
                call nodo_nuevo%agregar_conexion(num_sucursal, distancia, impresora)
            else
                nodo_auxiliar => self%cabeza
                do while (associated(nodo_auxiliar%siguiente))
                    if(num_sucursal_conexion < nodo_auxiliar%siguiente%num_sucursal_conexion) then
                        if(num_sucursal_conexion == nodo_auxiliar%num_sucursal_conexion) then
                            call nodo_auxiliar%agregar_conexion(num_sucursal, distancia, impresora)
                        else
                            nodo_nuevo%siguiente => nodo_auxiliar%siguiente
                            nodo_nuevo%anterior => nodo_auxiliar
                            nodo_auxiliar%siguiente%anterior => nodo_nuevo
                            nodo_auxiliar%siguiente => nodo_nuevo
                            nodo_nuevo%num_sucursal_conexion = num_sucursal_conexion
                            call nodo_nuevo%agregar_conexion(num_sucursal, distancia, impresora)
                        end if
                        return
                    end if
                    nodo_auxiliar => nodo_auxiliar%siguiente
                end do
                if(num_sucursal_conexion == nodo_auxiliar%num_sucursal_conexion) then
                    call nodo_auxiliar%agregar_conexion(num_sucursal, distancia, impresora)
                else
                    self%cola%siguiente => nodo_nuevo
                    nodo_nuevo%anterior => self%cola
                    self%cola => nodo_nuevo
                    nodo_nuevo%num_sucursal_conexion = num_sucursal_conexion
                    call nodo_nuevo%agregar_conexion(num_sucursal, distancia, impresora)
                end if
            end if
        end if
    end subroutine insertar_nodo

    subroutine agregar_conexion(self, num_sucursal, distancia, impresora)
        class(nodo_grafo), intent(inout) :: self
        integer, intent(in) :: num_sucursal
        integer, intent(in) :: distancia
        integer, intent(in) :: impresora
        type(nodo_conexion), pointer :: nodo_auxiliar
        type(nodo_conexion), pointer :: nodo_nuevo
        allocate(nodo_nuevo)
        nodo_nuevo%num_sucursal = num_sucursal
        nodo_nuevo%distancia = distancia
        nodo_nuevo%impresora = impresora
        if(.not. associated(self%lista_conexion)) then
            self%lista_conexion => nodo_nuevo
        else
            nodo_auxiliar => self%lista_conexion
            do while(associated(nodo_auxiliar%siguiente))
                nodo_auxiliar => nodo_auxiliar%siguiente
            end do
            nodo_auxiliar%siguiente => nodo_nuevo
        end if
    end subroutine agregar_conexion

    subroutine generar_grafo(self, nombre_grafica)
        class(lista_adyacencia), intent(in) :: self
        character(len=*), intent(in) :: nombre_grafica
        character(len=:), allocatable :: ruta_archivo
        character(len=100) :: comando
        character(len=32) :: num1, num2
        integer :: io, i
        type(nodo_grafo), pointer :: actual_nodo
        type(nodo_conexion), pointer :: actual_nodo_conexion
        comando = "dot -Tpdf ./graph/" // trim(nombre_grafica) // ".dot -o ./graph/" // trim(nombre_grafica) // ".pdf"
        io = 1
        open(newunit=io, file="./graph/" // trim(nombre_grafica) // ".dot")
        write(io, *) "digraph G {"
        write(io, *) "node [shape=doublecircle];"
        write(io, *) 'node [fontname="Courier New"]'
        write(io, *) 'Titulo [fontname="Courier New", color=red shape=box3d label="Grafo Rutas Sucursales"]'
        write(io, *) "{rank=same; Titulo;}"
        actual_nodo => self%cabeza
        do while(associated(actual_nodo))
            write(num1, '(I0)') actual_nodo%num_sucursal_conexion
            write(io, *) actual_nodo%num_sucursal_conexion, "[label = """,trim(num1), """]"
            actual_nodo_conexion => actual_nodo%lista_conexion
            do while(associated(actual_nodo_conexion))
                write(num2, '(I0)') actual_nodo_conexion%num_sucursal
                write(io, *) actual_nodo%num_sucursal_conexion, " -> ",trim(num2), " [dir = both];"
                actual_nodo_conexion => actual_nodo_conexion%siguiente
            end do
            actual_nodo => actual_nodo%siguiente
        end do
        write(io, *) "}"
        close(io)
        call execute_command_line(comando, exitstat=i)
        if (i /= 0) then
            print *, "Error al crear la imagen."
        else
            ruta_archivo = 'graph/' // trim(nombre_grafica) 
            call system('start ' // trim(adjustl(ruta_archivo)) // '.pdf')
            print *, "Grafica '"//trim(nombre_grafica)//"' Generada Correctamente."
        end if
    end subroutine generar_grafo

end module modulo_lista_adyacencia