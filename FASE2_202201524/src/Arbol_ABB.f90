module modulo_arbol_abb
    use modulo_capa
    implicit none
    private
    type :: nodo_abb
        integer :: valor
        type(nodo_abb), pointer :: derecha => null()
        type(nodo_abb), pointer :: izquierda => null()
    end type nodo_abb
    type, public :: arbol_abb
        type(nodo_abb), pointer :: raiz => null()
        type(capas) :: capas_cliente
    contains
        procedure :: insertar_nodo
        procedure :: recorrido_preorden
        procedure :: recorrido_inorden
        procedure :: recorrido_postorden
        procedure :: graficar_arbol
    end type arbol_abb

contains    
    !-----------------------------------------------------------------
    subroutine insertar_nodo(self, valor)
        class(arbol_abb), intent(inout) :: self
        integer, intent(in) :: valor
        if (.not. associated(self%raiz)) then
            allocate(self%raiz)
            self%raiz%valor = valor
        else
            call insertRec(self%raiz, valor)
        end if
    end subroutine insertar_nodo
    !---
    recursive subroutine insertRec(raiz, valor)
        type(nodo_abb), pointer, intent(inout) :: raiz
        integer, intent(in) :: valor
        if (valor < raiz%valor) then
            if (.not. associated(raiz%izquierda)) then
                allocate(raiz%izquierda)
                raiz%izquierda%valor = valor
            else
                call insertRec(raiz%izquierda, valor)
            end if
        else if (valor > raiz%valor) then
            if (.not. associated(raiz%derecha)) then
                allocate(raiz%derecha)
                raiz%derecha%valor = valor
            else
                call insertRec(raiz%derecha, valor)
            end if
        end if
    end subroutine insertRec

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