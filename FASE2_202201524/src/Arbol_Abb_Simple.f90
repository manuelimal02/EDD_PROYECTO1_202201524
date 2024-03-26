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
        procedure :: generar_grafica
        procedure :: numero_nodos
    end type arbol_abb_simple

contains

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

    function numero_nodos(self) result(num_nodos)
        class(arbol_abb_simple), intent(in) :: self
        integer :: num_nodos
        num_nodos = contar_nodos(self%raiz)
    end function numero_nodos
    
    recursive function contar_nodos(raiz) result(num_nodos)
        type(nodo_abb_simple), pointer, intent(in) :: raiz
        integer :: num_nodos
        if (.not. associated(raiz)) then
            num_nodos = 0
        else
            num_nodos = 1 + contar_nodos(raiz%izquierda) + contar_nodos(raiz%derecha)
        end if
    end function contar_nodos
    
    subroutine generar_grafica(self, crear_nodo, enlace_nodo, direccion_nodo_avl)
        class(arbol_abb_simple), intent(in) :: self
        character(len=:), allocatable, intent(inout) :: crear_nodo, enlace_nodo
        character(len=20), intent(in) :: direccion_nodo_avl
        character(len=20) :: direccion_abb, str_valor
        if (associated(self%raiz)) then
            direccion_abb = obtener_direccion_memoria(self%raiz)
            write(str_valor, '(I0)') self%raiz%valor
            crear_nodo = crear_nodo // '"' // trim(direccion_abb) // '"' // '[fontname="Courier New" label="' &
            // trim(str_valor) // '", color=blue];' // new_line('a')
            enlace_nodo = enlace_nodo // '"' // trim(direccion_nodo_avl) // '"' // " -> " // '"' // trim(direccion_abb) &
            // '" ' // new_line('a')
            call recorrer_arbol(self%raiz, crear_nodo, enlace_nodo)
        end if
    end subroutine generar_grafica

    recursive subroutine recorrer_arbol(actual, crear_nodo, enlace_nodo)
        type(nodo_abb_simple), pointer :: actual
        character(len=:), allocatable, intent(inout) :: crear_nodo, enlace_nodo
        character(len=20) :: direccion, str_valor
        if (associated(actual)) then
            direccion = obtener_direccion_memoria(actual)
            write(str_valor, '(I0)') actual%valor
            crear_nodo = crear_nodo // '"' // trim(direccion) // '"' // '[fontname="Courier New" label="' &
            // trim(str_valor) // '", color=blue];' // new_line('a')
            if (associated(actual%izquierda)) then
                enlace_nodo = enlace_nodo // '"' // trim(direccion) // '"' // " -> "
                direccion = obtener_direccion_memoria(actual%izquierda)
                enlace_nodo = enlace_nodo // '"' // trim(direccion) // '" ' &
                      // '[label = "L"];' // new_line('a')
            end if
            if (associated(actual%derecha)) then
                direccion = obtener_direccion_memoria(actual)
                enlace_nodo = enlace_nodo // '"' // trim(direccion) // '"' // " -> "
                direccion = obtener_direccion_memoria(actual%derecha)
                enlace_nodo = enlace_nodo // '"' // trim(direccion) // '" ' &
                      // '[label = "R"];' // new_line('a')
            end if
            call recorrer_arbol(actual%izquierda, crear_nodo, enlace_nodo)
            call recorrer_arbol(actual%derecha, crear_nodo, enlace_nodo)
        end if
    end subroutine recorrer_arbol    
    
    function obtener_direccion_memoria(nodo_arbol) result(direccion)
        type(nodo_abb_simple), pointer :: nodo_arbol
        character(len=20) :: direccion
        integer*8 :: i
        i = loc(nodo_arbol)
        write(direccion, 10) i 
        10 format(I0)
    end function obtener_direccion_memoria

end module modulo_abb