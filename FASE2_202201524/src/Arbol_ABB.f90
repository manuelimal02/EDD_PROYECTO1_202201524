module modulo_arbol_abb
    use modulo_matriz_dispersa
    implicit none
    private
    type :: nodo_abb
        integer :: valor
        type(nodo_abb), pointer :: derecha => null()
        type(nodo_abb), pointer :: izquierda => null()
        type(matriz_dispersa) :: matriz
    end type nodo_abb

    type, public :: arbol_abb
        type(nodo_abb), pointer :: raiz => null()
    contains
        procedure :: insertar_nodo
        procedure :: recorrido_preorden
        procedure :: recorrido_inorden
        procedure :: recorrido_postorden
        procedure :: recorrido_amplitud
        procedure :: graficar_arbol
        procedure :: buscar_matriz
    end type arbol_abb

contains    
    !-----------------------------------------------------------------
    subroutine insertar_nodo(self, valor, matriz)
        class(arbol_abb), intent(inout) :: self
        integer, intent(in) :: valor
        type(matriz_dispersa), intent(in) :: matriz
        type(nodo_abb), pointer :: nuevo
        allocate(nuevo)
        nuevo = nodo_abb(valor=valor, matriz=matriz)
        if (.not. associated(self%raiz)) then
            self%raiz => nuevo
        else
            call insertRec(self%raiz, nuevo)
        end if
    end subroutine insertar_nodo
    !---
    recursive subroutine insertRec(raiz, nuevo)
        type(nodo_abb), pointer, intent(inout) :: raiz
        type(nodo_abb), pointer, intent(in) :: nuevo
        if (nuevo%valor < raiz%valor) then
            if (.not. associated(raiz%izquierda)) then
                raiz%izquierda => nuevo
            else
                call insertRec(raiz%izquierda, nuevo)
            end if
        else if (nuevo%valor > raiz%valor) then
            if (.not. associated(raiz%derecha)) then
                raiz%derecha => nuevo
            else
                call insertRec(raiz%derecha, nuevo)
            end if
        end if
    end subroutine insertRec

    !-----------------------------------------------------------------
    function buscar_matriz(self, valor) result(matriz)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: valor
        type(matriz_dispersa) :: matriz
        type(nodo_abb), pointer :: nodo
        nodo => buscar_nodo(self%raiz, valor)
        if (associated(nodo)) then
            matriz = nodo%matriz
        else
            print *, "No Se encontro La Matriz."
        end if
    end function buscar_matriz
    !---
    recursive function buscar_nodo(raiz, valor) result(nodo)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: valor
        type(nodo_abb), pointer :: nodo
        if (.not. associated(raiz)) then
            nodo => null()
        else if (valor == raiz%valor) then
            nodo => raiz
        else if (valor < raiz%valor) then
            nodo => buscar_nodo(raiz%izquierda, valor)
        else
            nodo => buscar_nodo(raiz%derecha, valor)
        end if
    end function buscar_nodo    

    !-----------------------------------------------------------------
    subroutine recorrido_preorden(self, num_nodos, cadena)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: num_nodos
        character(len=:), allocatable, intent(out) :: cadena
        integer :: contador_nodos
        contador_nodos = 0
        cadena = ""
        call preorderRec(self%raiz, num_nodos, contador_nodos, cadena)
    end subroutine recorrido_preorden
    !---
    recursive subroutine preorderRec(raiz, num_nodos, contador_nodos, cadena)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: num_nodos
        integer, intent(inout) :: contador_nodos
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
        if(associated(raiz) .and. contador_nodos < num_nodos) then
            write(valor_str, '(I0)') raiz%valor
            cadena = trim(cadena) // trim(valor_str) // " - "
            contador_nodos = contador_nodos + 1
            call preorderRec(raiz%izquierda, num_nodos, contador_nodos, cadena)
            call preorderRec(raiz%derecha, num_nodos, contador_nodos, cadena)
        end if
    end subroutine preorderRec 
    
    !-----------------------------------------------------------------
    subroutine recorrido_inorden(self, num_nodos, cadena)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: num_nodos
        character(len=:), allocatable, intent(out) :: cadena
        integer :: contador_nodos
        contador_nodos = 0
        cadena = ""
        call inordenRec(self%raiz, num_nodos, contador_nodos, cadena)
    end subroutine recorrido_inorden
    !---
    recursive subroutine inordenRec(raiz, num_nodos, contador_nodos, cadena)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: num_nodos
        integer, intent(inout) :: contador_nodos
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
        if(associated(raiz) .and. contador_nodos < num_nodos) then
            call inordenRec(raiz%izquierda, num_nodos, contador_nodos, cadena)
            if (contador_nodos < num_nodos) then
                write(valor_str, '(I0)') raiz%valor
                cadena = trim(cadena) // trim(valor_str) // " - "
                contador_nodos = contador_nodos + 1
            end if
            call inordenRec(raiz%derecha, num_nodos, contador_nodos, cadena)
        end if
    end subroutine inordenRec
    
    !-----------------------------------------------------------------
    subroutine recorrido_postorden(self, num_nodos, cadena)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: num_nodos
        character(len=:), allocatable, intent(out) :: cadena
        integer :: contador_nodos
        contador_nodos = 0
        cadena = ""
        call posordenRec(self%raiz, num_nodos, contador_nodos, cadena)
    end subroutine recorrido_postorden
    !---
    recursive subroutine posordenRec(raiz, num_nodos, contador_nodos, cadena)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: num_nodos
        integer, intent(inout) :: contador_nodos
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
        if(associated(raiz) .and. contador_nodos < num_nodos) then
            call posordenRec(raiz%izquierda, num_nodos, contador_nodos, cadena)
            call posordenRec(raiz%derecha, num_nodos, contador_nodos, cadena)
            if (contador_nodos < num_nodos) then
                write(valor_str, '(I0)') raiz%valor
                cadena = trim(cadena) // trim(valor_str) // " - "
                contador_nodos = contador_nodos + 1
            end if
        end if
    end subroutine posordenRec
    
    !-----------------------------------------------------------------
    subroutine recorrido_amplitud(self, cadena)
        class(arbol_abb), intent(in) :: self
        character(len=:), allocatable, intent(out) :: cadena
        integer :: h, i
        cadena = ""
        h = altura(self%raiz)
        do i = 1, h
            call agregar_nivel_arbol(self%raiz, i, cadena)
        end do
    end subroutine recorrido_amplitud
    !---
    recursive subroutine agregar_nivel_arbol(raiz, nivel, cadena)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: nivel
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
        if (.not. associated(raiz)) then
            return
        else if (nivel == 1) then
            write(valor_str, '(I0)') raiz%valor
            cadena = trim(cadena) // trim(valor_str) // " - "
        else if (nivel > 1) then
            call agregar_nivel_arbol(raiz%izquierda, nivel-1, cadena)
            call agregar_nivel_arbol(raiz%derecha, nivel-1, cadena)
        end if
    end subroutine agregar_nivel_arbol
    !---
    recursive function altura(raiz) result(altura_arbol)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer :: altura_arbol, altura_aux_1, altura_aux_2
        if (.not. associated(raiz)) then
            altura_arbol = 0
        else
            altura_aux_1 = altura(raiz%izquierda)
            altura_aux_2 = altura(raiz%derecha)
            if (altura_aux_1 > altura_aux_2) then
                altura_arbol = altura_aux_1 + 1
            else
                altura_arbol = altura_aux_2 + 1
            end if
        end if
    end function altura
    
    !-----------------------------------------------------------------
    subroutine graficar_arbol(self, filename)
        class(arbol_abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        createNodes = ''
        linkNodes = ''
        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=doublecircle];" // new_line('a')
        dotStructure = dotStructure //&
        "Titulo [shape=component, label=""Arbol Binario De Busqueda Capas""];" &
        // new_line('a')
        dotStructure = dotStructure // "{rank=same; Titulo;}" // new_line('a')
        if (associated(self%raiz)) then
            call RoamTree(self%raiz, createNodes, linkNodes)
        end if
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Grafica '"//trim(filename)//"' Generada Correctamente."
    end subroutine graficar_arbol
    !---
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(nodo_abb), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_valor
        if (associated(current)) then
            address = get_address_memory(current)
            write(str_valor, '(I0)') current%valor
            createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_valor) // '"];' // new_line('a')
            if (associated(current%izquierda)) then
                linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
                address = get_address_memory(current%izquierda)
                linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')
            end if
            if (associated(current%derecha)) then
                address = get_address_memory(current)
                linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
                address = get_address_memory(current%derecha)
                linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "R"];' // new_line('a')
            end if
            call RoamTree(current%izquierda, createNodes, linkNodes)
            call RoamTree(current%derecha, createNodes, linkNodes)
        end if
    end subroutine RoamTree
    !---
    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        open(10, file="graph/"//dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)
        call system("dot -Tpng graph/"// dot_filename //" -o graph/" // png_filename)
    end subroutine write_dot
    !--
    function get_address_memory(node) result(address)
        type(nodo_abb), pointer :: node
        character(len=20) :: address
        integer*8 :: i
        i = loc(node)
        write(address, 10) i 
        10 format(I0)
    end function get_address_memory

end module modulo_arbol_abb