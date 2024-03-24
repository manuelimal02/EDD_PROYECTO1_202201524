module modulo_arbol_avl
    use modulo_abb
    implicit none
    integer, parameter :: peso_izquierda = -1
    integer, parameter :: equilibrio = 0
    integer, parameter :: peso_derecha = +1
    type nodo_avl
        integer :: valor
        integer :: factor
        type(nodo_avl), pointer :: izquierda => null()
        type(nodo_avl), pointer :: derecha => null()
        type(arbol_abb_simple) :: arbol_interno
    end type nodo_avl

    type arbol_avl
        type(nodo_avl), pointer :: raiz => null()
        contains
        procedure :: nuevo_arbol
        procedure :: insertar_nodo
        procedure :: graficar_arbol
    end type arbol_avl

    contains

    subroutine insertar_nodo(tree, valor, arbol)
        class(arbol_avl), intent(inout) :: tree
        integer, intent(in) :: valor
        type(arbol_abb_simple), intent(in) :: arbol
        logical :: incremento
        incremento = .false.
        tree%raiz => insertar_recursivo(tree%raiz, valor, arbol, incremento)
    end subroutine insertar_nodo

    recursive function insertar_recursivo(raiz, valor, arbol, incremento) result(nodo_resultado)
        type(nodo_avl), pointer :: raiz, nodo_resultado, n1
        logical, intent(out) :: incremento
        integer, intent(in) :: valor
        type(arbol_abb_simple), intent(in) :: arbol
        if (.not. associated(raiz)) then
            raiz => nuevo_nodo(valor, arbol)
            incremento = .true.
        else if (valor < raiz%valor) then
            raiz%izquierda => insertar_recursivo(raiz%izquierda, valor, arbol, incremento)
            if (incremento) then
                select case (raiz%factor)
                    case (peso_derecha)
                        raiz%factor = 0
                        incremento = .false.
                    case (equilibrio)
                        raiz%factor = -1
                    case (peso_izquierda)
                        n1 => raiz%izquierda
                        if (n1%factor == -1) then
                            raiz => rotacionII(raiz, n1)
                        else
                            raiz => rotationID(raiz, n1)
                        end if
                        incremento = .false.
                end select
            end if
        else if (valor > raiz%valor) then
            raiz%derecha => insertar_recursivo(raiz%derecha, valor, arbol, incremento)
            if (incremento) then
                select case (raiz%factor)
                case (peso_derecha)
                    n1 => raiz%derecha
                    if (n1%factor == 1) then
                        raiz => rotacionDD(raiz, n1)
                    else
                        raiz => rotacionDI(raiz, n1)
                    end if
                    incremento = .false.
                case (equilibrio)
                    raiz%factor = 1
                case (peso_izquierda)
                    raiz%factor = 0
                    incremento = .false.
                end select
            end if
        end if
        nodo_resultado => raiz
    end function insertar_recursivo

    function nuevo_nodo(valor, arbol) result(nodePtr)
        type(nodo_avl), pointer :: nodePtr
        integer, intent(in) :: valor
        type(arbol_abb_simple), intent(in) :: arbol
        allocate(nodePtr)
        nodePtr%valor = valor
        nodePtr%factor = 0
        nodePtr%izquierda => null()
        nodePtr%derecha => null()
        nodePtr%arbol_interno = arbol
    end function nuevo_nodo

    subroutine nuevo_arbol(self)
        class(arbol_avl), intent(inout) :: self
        self%raiz => null()
    end subroutine nuevo_arbol

    function rotacionII(n, n1) result(nodo_resultado)
        type(nodo_avl), pointer :: n, n1, nodo_resultado
        n%izquierda => n1%derecha
        n1%derecha => n
        if (n1%factor == -1) then
            n%factor = 0
            n1%factor = 0
        else
            n%factor = -1
            n1%factor = 1
        end if
        nodo_resultado => n1
    end function rotacionII

    function rotacionDD(n, n1) result(nodo_resultado)
        type(nodo_avl), pointer :: n, n1, nodo_resultado
        n%derecha => n1%izquierda
        n1%izquierda => n
        if (n1%factor == 1) then
            n%factor = 0
            n1%factor = 0
        else
            n%factor = 1
            n1%factor = -1
        end if
        nodo_resultado => n1
    end function rotacionDD

    function rotacionDI(n, n1) result(nodo_resultado)
        type(nodo_avl), pointer :: n, n1, nodo_resultado, n2
        n2 => n1%izquierda
        n%derecha => n2%izquierda
        n2%izquierda => n
        n1%izquierda => n2%derecha
        n2%derecha => n1
        if (n2%factor == 1) then
            n%factor = -1
        else
            n%factor = 0
        end if
        if (n2%factor == -1) then
            n1%factor = 1
        else
            n1%factor = 0
        end if
        n2%factor = 0
        nodo_resultado => n2
    end function rotacionDI

    function rotationID(n, n1) result(nodo_resultado)
        type(nodo_avl), pointer :: n, n1, nodo_resultado, n2
        n2 => n1%derecha
        n%izquierda => n2%derecha
        n2%derecha => n
        n1%derecha => n2%izquierda
        n2%izquierda => n1
        if (n2%factor == 1) then
            n1%factor = -1
        else
            n1%factor = 0
        end if
        if (n2%factor == -1) then
            n%factor = 1
        else
            n%factor = 0
        end if
        n2%factor = 0
        nodo_resultado => n2
    end function rotationID
    !----------------------------------------
    subroutine graficar_arbol(this, filename)
        class(arbol_avl), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        createNodes = ''
        linkNodes = ''
        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle, color=blue];" // new_line('a')
        if (associated(this%raiz)) then
            call RoamTreeAVL(this%raiz, createNodes, linkNodes)
        end if
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Grafica '"//trim(filename)//"' Generada Correctamente."
    end subroutine graficar_arbol
    
    recursive subroutine RoamTreeAVL(current, createNodes, linkNodes)
    type(nodo_avl), pointer :: current
    character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
    character(len=20) :: address, str_valor
    if (associated(current)) then
        address = get_address_memory_avl(current)
        write(str_valor, '(I0)') current%valor
        createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_valor) &
        // '", color=blue];' // new_line('a')
        call current%arbol_interno%generar_cadenas_grafica(createNodes, linkNodes, address)
        if (associated(current%izquierda)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory_avl(current%izquierda)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                  // '[label = "L"];' // new_line('a')
        end if
        if (associated(current%derecha)) then
            address = get_address_memory_avl(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory_avl(current%derecha)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                  // '[label = "R"];' // new_line('a')
        end if
        call RoamTreeAVL(current%izquierda, createNodes, linkNodes)
        call RoamTreeAVL(current%derecha, createNodes, linkNodes)
    end if
end subroutine RoamTreeAVL




    

    
    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        open(10, file=dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)
        call system("dot -Tpng " // dot_filename // " -o " // png_filename)
    end subroutine write_dot

    function get_address_memory_avl(node) result(address)
        type(nodo_avl), pointer :: node
        character(len=20) :: address
        integer*8 :: i
        i = loc(node)
        write(address, 10) i 
        10 format(I0)
    end function get_address_memory_avl

    



end module modulo_arbol_avl