module modulo_matriz_dispersa
    implicit none
    private
    type :: nodo_valor
        private
        logical :: existe = .false.
        character(len=:), allocatable :: valor
    end type nodo_valor
    type :: nodo_matriz
        private
        integer :: i,j
        character(len=:), allocatable :: valor
        type(nodo_matriz), pointer :: arriba => null()
        type(nodo_matriz), pointer :: abajo => null()
        type(nodo_matriz), pointer :: derecha => null()
        type(nodo_matriz), pointer :: izquierda => null()
    end type nodo_matriz
    type, public :: matriz_dispersa
        private
        type(nodo_matriz), pointer :: raiz => null()
        integer :: ancho = 0
        integer :: altura = 0
    contains
        procedure :: insertar
        procedure :: insertarCabeceraFila
        procedure :: insertarCabeceraColumna
        procedure :: buscarFila
        procedure :: buscarColumna
        procedure :: existeNodo
        procedure :: print
        procedure :: imprimirEncabezadoColumnas
        procedure :: graficar
        procedure :: obtenerValor
    end type matriz_dispersa

contains
    subroutine insertar(self, i, j, valor)
        class(matriz_dispersa), intent(inout) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(len=*), intent(in) :: valor
        type(nodo_matriz), pointer :: nuevo
        type(nodo_matriz), pointer :: fila
        type(nodo_matriz), pointer :: columna
        allocate(nuevo)
        nuevo = nodo_matriz(i=i, j=j, valor=valor)
        if(.not. associated(self%raiz)) then
            allocate(self%raiz)
            self%raiz = nodo_matriz(i=-1, j=-1)
        end if
        fila => self%buscarFila(j)
        columna => self%buscarColumna(i)
        if(i > self%ancho) self%ancho = i
        if(j > self%altura) self%altura = j
        if(.not. self%existeNodo(nuevo)) then
            if(.not. associated(columna)) then
                columna => self%insertarCabeceraColumna(i)
            end if
            if(.not. associated(fila)) then
                fila => self%insertarCabeceraFila(j)
            end if
            call insertarEnColumna(nuevo, fila)
            call insertarEnFila(nuevo, columna)
        end if
    end subroutine insertar


    function insertarCabeceraColumna(self, i) result(nuevaCabeceraColumna)
        class(matriz_dispersa), intent(inout) :: self
        integer, intent(in) :: i
        type(nodo_matriz), pointer :: nuevaCabeceraColumna
        allocate(nuevaCabeceraColumna)
        nuevaCabeceraColumna = nodo_matriz(i=i, j=-1)
        call insertarEnColumna(nuevaCabeceraColumna, self%raiz)
    end function insertarCabeceraColumna


    function insertarCabeceraFila(self, j) result(nuevaCabeceraFila)
        class(matriz_dispersa), intent(inout) :: self
        integer, intent(in) :: j
        type(nodo_matriz), pointer :: nuevaCabeceraFila
        allocate(nuevaCabeceraFila)
        nuevaCabeceraFila = nodo_matriz(i=-1, j=j)
        call insertarEnFila(nuevaCabeceraFila, self%raiz)
    end function insertarCabeceraFila


    subroutine insertarEnFila(nuevo, CabeceraFila)
        type(nodo_matriz), pointer :: nuevo
        type(nodo_matriz), pointer :: cabeceraFila 
        !head 
        type(nodo_matriz), pointer :: actual
        actual => cabeceraFila
        do while(associated(actual%abajo))
            if(nuevo%j < actual%abajo%j) then
                nuevo%abajo => actual%abajo
                nuevo%arriba => actual
                actual%abajo%arriba => nuevo
                actual%abajo => nuevo
                exit
            end if
            actual => actual%abajo
        end do
        if(.not. associated(actual%abajo)) then
            actual%abajo => nuevo
            nuevo%arriba => actual
        end if
    end subroutine insertarEnFila


    subroutine insertarEnColumna(nuevo, CabeceraColumna)
        type(nodo_matriz), pointer :: nuevo
        type(nodo_matriz), pointer :: CabeceraColumna 
        !head 
        type(nodo_matriz), pointer :: actual
        actual => CabeceraColumna
        do while(associated(actual%derecha))
            if(nuevo%i < actual%derecha%i) then
                nuevo%derecha => actual%derecha
                nuevo%izquierda => actual
                actual%derecha%izquierda => nuevo
                actual%derecha => nuevo
                exit
            end if
            actual => actual%derecha
        end do
        if(.not. associated(actual%derecha)) then
            actual%derecha => nuevo
            nuevo%izquierda => actual
        end if
    end subroutine insertarEnColumna  


    function buscarFila(self, j) result(actual)
        class(matriz_dispersa), intent(in) :: self
        integer, intent(in) :: j
        type(nodo_matriz), pointer :: actual
        actual => self%raiz
        do while(associated(actual)) 
            if(actual%j == j) return
            actual => actual%abajo
        end do
    end function buscarFila


    function buscarColumna(self, i) result(actual)
        class(matriz_dispersa), intent(in) :: self
        integer, intent(in) :: i
        type(nodo_matriz), pointer :: actual
        actual => self%raiz 
        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%derecha
        end do
    end function buscarColumna


    function existeNodo(self, nodo) result(existe)
        class(matriz_dispersa), intent(inout) :: self
        type(nodo_matriz), pointer, intent(in) :: nodo
        logical :: existe
        type(nodo_matriz), pointer :: encabezadoFila
        type(nodo_matriz), pointer :: columna
        encabezadoFila => self%raiz
        existe = .false.
        do while(associated(encabezadoFila))
            if(encabezadoFila%j == nodo%j) then
                columna => encabezadoFila
                do while(associated(columna)) 
                    if(columna%i == nodo%i) then
                        columna%valor = nodo%valor
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
    end function existeNodo


    subroutine graficar(self)
        class(matriz_dispersa), intent(in) :: self
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
        type(nodo_matriz), pointer :: fila_aux
        type(nodo_matriz), pointer :: columna_aux
        io = 1
        fila_aux => self%raiz
        comando = "dot -Tpng ./matrix.dot -o ./matrix.png"
        open(newunit=io, file="./matrix.dot")
        write(io, *) "digraph Matrix {"
        write(io, *) 'node[shape = "box"]'
        do while (associated(fila_aux))
            rank = "{rank=same"
            columna_aux => fila_aux
            do while(associated(columna_aux)) 
                write(str_i, '(I10)') columna_aux%i + 1
                write(str_j, '(I10)') columna_aux%j + 1
                nombre = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"'
                if (columna_aux%i == -1 .and. columna_aux%j == -1) then
                    node_dec = trim(adjustl(nombre))//'[label = "CAMBIO", group="'//trim(adjustl(str_i))//'"]'
                else if(columna_aux%i == -1) then
                    write(str_j_aux, '(I10)') columna_aux%j
                    contenido = trim(adjustl(str_j_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                    
                else if(columna_aux%j == -1) then
                    write(str_i_aux, '(I10)') columna_aux%i
                    contenido = trim(adjustl(str_i_aux))
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'", group="'//trim(adjustl(str_i))//'"]'
                        
                else
                    if(columna_aux%valor .NE. "") then
                        contenido = columna_aux%valor
                    else
                        contenido = 'F'
                    end if 
                    node_dec = trim(adjustl(nombre))//'[label = "'//trim(adjustl(contenido))
                    node_dec = trim(adjustl(node_dec))//'",  style = filled, fillcolor = "'//&
                    & trim(contenido)//'" group="'// &
                    & trim(adjustl(str_i))//'"]'
                end if
                write(io, *) node_dec
                if(associated(columna_aux%derecha)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'
                    write(str_i_aux, '(I10)') columna_aux%derecha%i + 1
                    write(str_j_aux, '(I10)') columna_aux%derecha%j + 1
                    conexion = conexion//'"Nodo'//trim(adjustl(str_i_aux))//'_'//trim(adjustl(str_j_aux))//'"'
                    conexionRev = conexion//'[dir = back]'
                    write(io, *) conexion
                    write(io, *) conexionRev
                end if
                if(associated(columna_aux%abajo)) then
                    conexion = '"Nodo'//trim(adjustl(str_i))//'_'//trim(adjustl(str_j))//'"->'
                    write(str_i_aux, '(I10)') columna_aux%abajo%i + 1
                    write(str_j_aux, '(I10)') columna_aux%abajo%j + 1
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
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficar


    subroutine print(self)
        class(matriz_dispersa), intent(in) :: self
        integer :: i
        integer :: j
        type(nodo_matriz), pointer :: aux
        type(nodo_valor) :: val
        aux => self%raiz%abajo
        call self%imprimirEncabezadoColumnas()
        do j = 0, self%altura
            print *, ""
            write(*, fmt='(I3)', advance='no') j
            do i = 0, self%ancho
                val = self%obtenerValor(i,j)
                if(.not. val%existe) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%valor
                end if
            end do
        end do
    end subroutine print



    subroutine imprimirEncabezadoColumnas(self)
        class(matriz_dispersa), intent(in) :: self
        integer :: i
        do i=-1, self%ancho
            write(*, fmt='(I3)', advance='no') i
        end do
    end subroutine imprimirEncabezadoColumnas


    function obtenerValor(self, i, j) result(val)
        class(matriz_dispersa), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        type(nodo_matriz), pointer :: cabeceraFila
        type(nodo_matriz), pointer :: columna
        type(nodo_valor) :: val
        cabeceraFila => self%raiz
        do while(associated(cabeceraFila))
            if(cabeceraFila%j == j) then
                columna => cabeceraFila
                do while(associated(columna)) 
                    if(columna%i == i) then
                        val%valor = columna%valor
                        val%existe = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            cabeceraFila => cabeceraFila%abajo
        end do
        return
    end function obtenerValor
end module modulo_matriz_dispersa