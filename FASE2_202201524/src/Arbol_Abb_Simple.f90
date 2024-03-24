module modulo_abb
    implicit none
    private
    type :: nodo_abb_simple
        integer :: valor
        type(nodo_abb_simple), pointer :: derecha => null()
        type(nodo_abb_simple), pointer :: izquierda => null()
    end type nodo_abb_simple
    type, public :: arbol_abb_simple
        type(nodo_abb_simple), pointer :: raiz => null()
    contains
        procedure :: insertar
        procedure :: recorrido_amplitud
        procedure :: generar_cadenas_grafica
    end type arbol_abb_simple

contains
    !-----------------------------------------------------------------
    subroutine insertar(self, valor)
        class(arbol_abb_simple), intent(inout) :: self
        integer, intent(in) :: valor
        if (.not. associated(self%raiz)) then
            allocate(self%raiz)
            self%raiz%valor = valor
        else
            call insertar_recursivo(self%raiz, valor)
        end if
    end subroutine insertar
    !-----------------------------------------------------------------
    recursive subroutine insertar_recursivo(raiz, valor)
        type(nodo_abb_simple), pointer, intent(inout) :: raiz
        integer, intent(in) :: valor
        if (valor < raiz%valor) then
            if (.not. associated(raiz%izquierda)) then
                allocate(raiz%izquierda)
                raiz%izquierda%valor = valor
            else
                call insertar_recursivo(raiz%izquierda, valor)
            end if
        else if (valor > raiz%valor) then
            if (.not. associated(raiz%derecha)) then
                allocate(raiz%derecha)
                raiz%derecha%valor = valor
            else
                call insertar_recursivo(raiz%derecha, valor)
            end if
        end if
    end subroutine insertar_recursivo
    !-----------------------------------------------------------------
    subroutine recorrido_amplitud(self, cadena)
        class(arbol_abb_simple), intent(in) :: self
        character(len=:), allocatable, intent(out) :: cadena
        integer :: h, i
        cadena = ""
        h = altura(self%raiz)
        do i = 1, h
            call agregar_nivel_arbol(self%raiz, i, cadena)
        end do
    end subroutine recorrido_amplitud
    !-----------------------------------------------------------------
    recursive subroutine agregar_nivel_arbol(raiz, nivel, cadena)
        type(nodo_abb_simple), pointer, intent(in) :: raiz
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
    !------------------------------------------------------------------
    recursive function altura(raiz) result(altura_arbol)
        type(nodo_abb_simple), pointer, intent(in) :: raiz
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
    subroutine generar_cadenas_grafica(self, createNodes, linkNodes, address_avl)
        class(arbol_abb_simple), intent(in) :: self
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20), intent(in) :: address_avl
        character(len=20) :: address_abb, str_valor
        if (associated(self%raiz)) then
            address_abb = get_address_memory(self%raiz)
            write(str_valor, '(I0)') self%raiz%valor
            createNodes = createNodes // '"' // trim(address_abb) // '"' // '[label="' // trim(str_valor) &
            // '", color=red];' // new_line('a')
            linkNodes = linkNodes // '"' // trim(address_avl) // '"' // " -> " // '"' // trim(address_abb) &
            // '" ' // new_line('a')
            call RoamTree(self%raiz, createNodes, linkNodes)
        end if
    end subroutine generar_cadenas_grafica
    !-----------------------------------------------------------------
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(nodo_abb_simple), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_valor
        if (associated(current)) then
            address = get_address_memory(current)
            write(str_valor, '(I0)') current%valor
            createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // & 
            trim(str_valor) // '", color=red];' // new_line('a')
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
    !-----------------------------------------------------------------
    function get_address_memory(node) result(address)
        type(nodo_abb_simple), pointer :: node
        character(len=20) :: address
        integer*8 :: i
        i = loc(node)
        write(address, 10) i 
        10 format(I0)
    end function get_address_memory

end module modulo_abb