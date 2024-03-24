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
        procedure :: buscar_valor
        procedure :: valor_existe
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
    !----------------------------------------
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
    !----------------------------------------
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
    !----------------------------------------
    subroutine nuevo_arbol(self)
        class(arbol_avl), intent(inout) :: self
        self%raiz => null()
    end subroutine nuevo_arbol
    !----------------------------------------
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
    !----------------------------------------
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
    !----------------------------------------
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
    !----------------------------------------
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
    function valor_existe(self, valor) result(existe)
        class(arbol_avl), intent(inout) :: self
        integer, intent(in) :: valor
        logical :: existe
        type(nodo_avl), pointer :: nodo_encontrado
        nodo_encontrado => buscar_recursivo(self%raiz, valor)
        existe = associated(nodo_encontrado)
    end function valor_existe
    
    !----------------------------------------
    function buscar_valor(self, valor) result(arbol)
        class(arbol_avl), intent(inout) :: self
        integer, intent(in) :: valor
        type(arbol_abb_simple), pointer :: arbol
        type(nodo_avl), pointer :: nodo_encontrado
        nodo_encontrado => buscar_recursivo(self%raiz, valor)
        if (associated(nodo_encontrado)) then
            arbol => nodo_encontrado%arbol_interno
        end if
    end function buscar_valor
    
    !----------------------------------------
    recursive function buscar_recursivo(raiz, valor) result(nodo_resultado)
        type(nodo_avl), pointer :: raiz, nodo_resultado
        integer, intent(in) :: valor
        if (.not. associated(raiz)) then
            nodo_resultado => null()
        else if (valor < raiz%valor) then
            nodo_resultado => buscar_recursivo(raiz%izquierda, valor)
        else if (valor > raiz%valor) then
            nodo_resultado => buscar_recursivo(raiz%derecha, valor)
        else
            nodo_resultado => raiz
        end if
    end function buscar_recursivo

    !----------------------------------------
    subroutine graficar_arbol(this, nombre_grafica)
        class(arbol_avl), intent(in) :: this
        character(len=*), intent(in) :: nombre_grafica
        character(len=:), allocatable :: codigo_dot
        character(len=:), allocatable :: crear_nodo
        character(len=:), allocatable :: direccion_nodo
        crear_nodo = ''
        direccion_nodo = ''
        codigo_dot = "digraph G{" // new_line('a')
        codigo_dot = codigo_dot // "node [shape=doublecircle, color=blue];" // new_line('a')
        codigo_dot = codigo_dot //&
        "Titulo [shape=component, label=""Arbol Binario AVL""];" &
        // new_line('a')
        codigo_dot = codigo_dot // "{rank=same; Titulo;}" // new_line('a')
        if (associated(this%raiz)) then
            call recorrer_arbol_avl(this%raiz, crear_nodo, direccion_nodo)
        end if
        codigo_dot = codigo_dot // trim(crear_nodo) // trim(direccion_nodo) // "}" // new_line('a')
        call generar_grafica(nombre_grafica, codigo_dot)
        print *, "Grafica '"//trim(nombre_grafica)//"' Generada Correctamente."
    end subroutine graficar_arbol
    !----------------------------------------
    recursive subroutine recorrer_arbol_avl(actual, crear_nodo, direccion_nodo)
        type(nodo_avl), pointer :: actual
        character(len=:), allocatable, intent(inout) :: crear_nodo, direccion_nodo
        character(len=20) :: direccion, str_valor
        if (associated(actual)) then
            direccion = obtener_direccion_memoria_avl(actual)
            write(str_valor, '(I0)') actual%valor
            crear_nodo = crear_nodo // '"' // trim(direccion) // '"' // '[label="' // trim(str_valor) &
            // '", color=blue];' // new_line('a')
            call actual%arbol_interno%generar_grafica(crear_nodo, direccion_nodo, direccion)
            if (associated(actual%izquierda)) then
                direccion_nodo = direccion_nodo // '"' // trim(direccion) // '"' // " -> "
                direccion = obtener_direccion_memoria_avl(actual%izquierda)
                direccion_nodo = direccion_nodo // '"' // trim(direccion) // '" ' &
                    // '[label = "L"];' // new_line('a')
            end if
            if (associated(actual%derecha)) then
                direccion = obtener_direccion_memoria_avl(actual)
                direccion_nodo = direccion_nodo // '"' // trim(direccion) // '"' // " -> "
                direccion = obtener_direccion_memoria_avl(actual%derecha)
                direccion_nodo = direccion_nodo // '"' // trim(direccion) // '" ' &
                    // '[label = "R"];' // new_line('a')
            end if
            call recorrer_arbol_avl(actual%izquierda, crear_nodo, direccion_nodo)
            call recorrer_arbol_avl(actual%derecha, crear_nodo, direccion_nodo)
        end if
    end subroutine recorrer_arbol_avl
    !----------------------------------------
    subroutine generar_grafica(nombre_grafica, codigo)
        character(len=*), intent(in) :: codigo, nombre_grafica
        character(len=:), allocatable :: dot_nombre_grafica, png_nombre_grafica
        dot_nombre_grafica = "graph/" // trim(nombre_grafica) // ".dot"
        png_nombre_grafica = "graph/" // trim(nombre_grafica) // ".png"
        open(10, file=dot_nombre_grafica, status='replace', action='write')
        write(10, '(A)') trim(codigo)
        close(10)
        call system("dot -Tpng " // dot_nombre_grafica // " -o " // png_nombre_grafica)
    end subroutine generar_grafica
    !----------------------------------------
    function obtener_direccion_memoria_avl(node) result(direccion)
        type(nodo_avl), pointer :: node
        character(len=20) :: direccion
        integer*8 :: i
        i = loc(node)
        write(direccion, 10) i 
        10 format(I0)
    end function obtener_direccion_memoria_avl
end module modulo_arbol_avl