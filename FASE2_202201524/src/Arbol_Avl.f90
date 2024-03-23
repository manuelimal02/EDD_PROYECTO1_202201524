module modulo_arbol_avl
    implicit none
    integer, parameter :: peso_izquierda = -1
    integer, parameter :: equilibrio = 0
    integer, parameter :: peso_derecha = +1
    type nodo_avl
        integer :: valor
        integer :: factor
        type(nodo_avl), pointer :: izquierda => null()
        type(nodo_avl), pointer :: derecha => null()
    end type nodo_avl
    type arbol_avl
        type(nodo_avl), pointer :: raiz => null()
        contains
        procedure :: nuevo_arbol
        procedure :: insertar_nodo
        procedure :: graficar_arbol
    end type arbol_avl

    contains
    function nuevo_nodo(valor) result(nodePtr)
        type(nodo_avl), pointer :: nodePtr
        integer, intent(in) :: valor
        allocate(nodePtr)
        nodePtr%valor = valor
        nodePtr%factor = 0
        nodePtr%izquierda => null()
        nodePtr%derecha => null()
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

    recursive function insertar_nodo2(raiz, valor, incremento) result(nodo_resultado)
        type(nodo_avl), pointer :: raiz, nodo_resultado, n1
        logical, intent(out) :: incremento
        integer, intent(in) :: valor
        if (.not. associated(raiz)) then
            allocate(nodo_resultado)
            raiz => nuevo_nodo(valor)
            incremento = .true.
        else if (valor < raiz%valor) then
            raiz%izquierda => insertar_nodo2(raiz%izquierda, valor, incremento)
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
            raiz%derecha => insertar_nodo2(raiz%derecha, valor, incremento)
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
    end function insertar_nodo2

    subroutine insertar_nodo(tree, valor)
        class(arbol_avl), intent(inout) :: tree
        integer, intent(in) :: valor
        logical :: incremento
        incremento = .false.
        tree%raiz => insertar_nodo2(tree%raiz, valor, incremento)
    end subroutine insertar_nodo

    subroutine graficar_arbol(this)
        class(arbol_avl), intent(inout) :: this
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        createNodes = ''
        linkNodes = ''
        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')
        if (associated(this%raiz)) then
            call RoamTree(this%raiz, createNodes, linkNodes)
        end if
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graficar_arbol

    recursive subroutine RoamTree(actual, createNodes, linkNodes)
        type(nodo_avl), pointer :: actual
        character(len=:), allocatable, intent(inout) :: createNodes
        character(len=:), allocatable, intent(inout) :: linkNodes
        character(len=20) :: address
        character(len=20) :: str_value
        if (associated(actual)) then
            address = get_address_memory(actual)
            write(str_value, '(I0)') actual%valor
            createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
            ! VIAJAMOS A LA SUBRAMA IZQ
            if (associated(actual%izquierda)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(actual%izquierda)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                        // '[label = "L"];' // new_line('a')
            end if
            ! VIAJAMOS A LA SUBRAMA DER
            if (associated(actual%derecha)) then
            address = get_address_memory(actual)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(actual%derecha)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                        // '[label = "R"];' // new_line('a')
            end if
            call RoamTree(actual%izquierda, createNodes, linkNodes)
            call RoamTree(actual%derecha, createNodes, linkNodes)
        end if
    end subroutine RoamTree

    function get_address_memory(node) result(address)
        type(nodo_avl), pointer :: node
        character(len=20) :: address
        integer*8 :: i
        i = loc(node)
        write(address, 10) i 
        10 format(I0)
    end function get_address_memory

    subroutine write_dot(code)
        character(len=*), intent(in) :: code
        open(10, file='graph.dot', status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)
        call system("dot -Tpng graph.dot -o grafo.png")
    end subroutine write_dot

end module modulo_arbol_avl