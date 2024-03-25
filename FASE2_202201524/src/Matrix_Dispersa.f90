module modulo_matrix_dispersa
    implicit none
    private
    type, public :: nodo_valor
        logical :: existe = .false.
        character(len=30) :: color
    end type nodo_valor

    type :: nodo_pixel
        private
        integer :: columna, fila
        character(len=:), allocatable :: color
        type(nodo_pixel), pointer :: arriba => null()
        type(nodo_pixel), pointer :: abajo => null()
        type(nodo_pixel), pointer :: derecha => null()
        type(nodo_pixel), pointer :: izquierda => null()
    end type nodo_pixel

    type, public :: matriz
        private
        type(nodo_pixel), pointer :: raiz => null()
        integer, public :: ancho = 0
        integer, public :: alto = 0
    contains
        procedure :: insertar_nodo
        procedure :: insertar_matriz
        procedure :: insertar_cabecera_fila
        procedure :: insertar_cabecera_columna
        procedure :: buscar_fila
        procedure :: buscar_columna
        procedure :: existe_nodo
        procedure :: graficar_matriz
    end type matriz
contains

    subroutine insertar_nodo(self,  i, j, color)
        class(matriz), intent(inout) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(len=30), intent(in) :: color
        type(nodo_pixel), pointer :: nuevo
        type(nodo_pixel), pointer :: fila
        type(nodo_pixel), pointer :: columna
        allocate(nuevo)
        nuevo = nodo_pixel(fila=j, columna=i, color=color)
        if(.not. associated(self%raiz)) then
            allocate(self%raiz)
            self%raiz = nodo_pixel(fila=-1, columna=-1)
        end if
        fila => self%buscar_fila(j)
        columna => self%buscar_columna(i)
        if(i > self%ancho) self%ancho = i
        if(j > self%alto) self%alto = j
        if(.not. self%existe_nodo(nuevo)) then
            if(.not. associated(columna)) then
                columna => self%insertar_cabecera_columna(i)
            end if
            if(.not. associated(fila)) then
                fila => self%insertar_cabecera_fila(j)
            end if
            call insertarEnColuma(nuevo, fila)
            call insertarEnFila(nuevo, columna)
        end if
    end subroutine insertar_nodo

    subroutine insertar_matriz(self, m)
        class(matriz), intent(inout) :: self
        class(matriz), intent(in) :: m
        type(nodo_pixel), pointer :: fila_aux
        type(nodo_pixel), pointer :: columna_aux
        fila_aux => m%raiz%abajo
        do while(associated(fila_aux))
            columna_aux => fila_aux%derecha
            do while(associated(columna_aux))
                call self%insertar_nodo(columna_aux%columna, columna_aux%fila, columna_aux%color)
                columna_aux => columna_aux%derecha
            end do
            fila_aux => fila_aux%abajo
        end do
    end subroutine insertar_matriz

    function insertar_cabecera_columna(self, i) result(nuevaCabeceraColumna)
        class(matriz), intent(inout) :: self
        integer, intent(in) :: i
        type(nodo_pixel), pointer :: nuevaCabeceraColumna
        allocate(nuevaCabeceraColumna)
        nuevaCabeceraColumna = nodo_pixel(fila=-1, columna=i)
        call insertarEnColuma(nuevaCabeceraColumna, self%raiz)
        
    end function insertar_cabecera_columna

    function insertar_cabecera_fila(self, j) result(nuevaCabeceraFila)
        class(matriz), intent(inout) :: self
        integer, intent(in) :: j
        type(nodo_pixel), pointer :: nuevaCabeceraFila
        allocate(nuevaCabeceraFila)
        nuevaCabeceraFila = nodo_pixel(fila=j, columna=-1)
        call insertarEnFila(nuevaCabeceraFila, self%raiz)
    end function insertar_cabecera_fila

    subroutine insertarEnFila(nuevo,  CabeceraFila)
        type(nodo_pixel), pointer :: nuevo
        type(nodo_pixel), pointer :: CabeceraFila
        type(nodo_pixel), pointer :: actual
        actual => CabeceraFila
        do while(associated(actual%abajo))
            if(nuevo%fila < actual%abajo%fila) then
                nuevo%abajo => actual%abajo
                nuevo%arriba => actual
                actual%abajo%arriba => nuevo
                actual%abajo => nuevo
                exit
            end if
            actual => actual%abajo
        end do
        if ( .not. associated(actual%abajo) ) then
            actual%abajo => nuevo
            nuevo%arriba => actual
        end if
    end subroutine insertarEnFila

    subroutine insertarEnColuma(nuevo,  CabeceraColumna)
        type(nodo_pixel), pointer :: nuevo
        type(nodo_pixel), pointer :: CabeceraColumna
        type(nodo_pixel), pointer :: actual
        actual => CabeceraColumna
        do while(associated(actual%derecha))
            if(nuevo%columna < actual%derecha%columna) then
                nuevo%derecha => actual%derecha
                nuevo%izquierda => actual
                actual%derecha%izquierda => nuevo
                actual%derecha => nuevo
                exit
            end if
            actual => actual%derecha
        end do
        if ( .not. associated(actual%derecha) ) then
            actual%derecha => nuevo
            nuevo%izquierda => actual
        end if
    end subroutine insertarEnColuma

    function buscar_fila(self, j) result(actual)
        class(matriz), intent(in) :: self
        integer, intent(in) :: j
        type(nodo_pixel), pointer :: actual
        actual => self%raiz
        do while(associated(actual))
            if ( actual%fila == j ) return
            actual => actual%abajo
        end do   
    end function buscar_fila

    function buscar_columna(self, i) result(actual)
        class(matriz), intent(in) :: self
        integer, intent(in) :: i
        type(nodo_pixel), pointer :: actual
        actual => self%raiz
        do while(associated(actual))
            if ( actual%columna == i ) return
            actual => actual%derecha
        end do   
    end function buscar_columna

    function existe_nodo(self, nodo) result(existe)
        class(matriz), intent(in) :: self
        type(nodo_pixel), pointer, intent(in) :: nodo
        logical :: existe
        type(nodo_pixel), pointer :: encabezadoFila
        type(nodo_pixel), pointer :: columna
        encabezadoFila => self%raiz
        existe = .false.
        do while(associated(encabezadoFila))
            if ( encabezadoFila%fila == nodo%fila ) then
                columna => encabezadoFila
                do while(associated(columna))
                    if ( columna%columna == nodo%columna ) then
                        columna%color = nodo%color
                        existe = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            encabezadoFila => encabezadoFila%abajo
        end do
        return
    end function existe_nodo

    subroutine graficar_matriz(self, filename)
        class(matriz), intent(in) :: self
        character(len=*), intent(in) :: filename
        integer :: io
        integer :: i
        character(len=10) :: str_i
        character(len=10) :: str_j
        character(len=10) :: str_i_aux
        character(len=10) :: str_j_aux
        character(len=150) :: node_dec
        character(len=20) :: nombre
        character(len=100) :: comando
        character(len=50) :: contenido
        character(:), allocatable :: rank
        character(:), allocatable :: conexion
        character(:), allocatable :: conexionRev
        type(nodo_pixel), pointer :: fila_aux
        type(nodo_pixel), pointer :: columna_aux
        io = 1
        fila_aux => self%raiz
        comando = "dot -Gnslimit=2 -Tpng ./graph/"//trim(filename)//".dot -o ./graph/"//trim(filename)//".png"
        open(newunit=io, file="./graph/"//trim(filename)//".dot")
        write(io, *) "digraph Matrix {"
        write(io, *) 'node[shape = "box", style=filled]'
        do while (associated(fila_aux))
            rank = "{rank=same"
            columna_aux => fila_aux
            do while(associated(columna_aux)) 
                write(str_i, '(I10)') columna_aux%columna + 1
                write(str_j, '(I10)') columna_aux%fila + 1
                nombre = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                if (columna_aux%columna == -1 .and. columna_aux%fila == -1) then
                    node_dec = trim(adjustl(nombre))//'[label = "raiz", group="'//trim(adjustl(str_i))//'"]'
                else if(columna_aux%columna == -1) then
                    write(str_j_aux, '(I10)') columna_aux%fila
                    contenido = trim(adjustl(str_j_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                else if(columna_aux%fila == -1) then
                    write(str_i_aux, '(I10)') columna_aux%columna
                    contenido = trim(adjustl(str_i_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                else
                    contenido = " "
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))//'",'&
                    //' fillcolor="'//trim(adjustl(columna_aux%color))//'"'
                    node_dec = trim(adjustl(node_dec))//', group="'//trim(adjustl(str_i))//'"]'
                end if
                write(io, *) node_dec
                if(associated(columna_aux%derecha)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'
                    write(str_i_aux, '(I10)') columna_aux%derecha%columna + 1
                    write(str_j_aux, '(I10)') columna_aux%derecha%fila + 1
                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if
                if(associated(columna_aux%abajo)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'
                    write(str_i_aux, '(I10)') columna_aux%abajo%columna + 1
                    write(str_j_aux, '(I10)') columna_aux%abajo%fila + 1
                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if
                rank = rank//';"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                columna_aux => columna_aux%derecha
            end do
            rank = rank//'}'
            write(io, *) rank
            fila_aux => fila_aux%abajo
        end do
        write(io, *) "}"
        close(io)
        call execute_command_line(comando, exitstat=i)
        if ( i == 1 ) then
            print *, "Error Al Generar Imagen: '"//trim(filename)//"' ."
        else
            print *, "Imagen '"//trim(filename)//"' Generada Correctamente."
        end if
    end subroutine graficar_matriz
    
end module modulo_matrix_dispersa