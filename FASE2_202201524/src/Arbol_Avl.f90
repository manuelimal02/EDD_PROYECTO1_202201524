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
        procedure :: eliminar_nodo
        procedure :: graficar_arbol
        procedure :: buscar_valor
        procedure :: valor_existe
        procedure :: top_5_imagenes
        procedure :: graficar_arbol_imagen
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
                            raiz => rotacionID(raiz, n1)
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

    function rotacionID(n, n1) result(nodo_resultado)
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
    end function rotacionID

    subroutine eliminar_nodo(tree, valor)
        class(arbol_avl), intent(inout) :: tree
        integer, intent(in) :: valor
        tree%raiz => deleteRec(tree%raiz, valor)
    end subroutine eliminar_nodo

    recursive function deleteRec(raiz, valor) result(res)
        type(nodo_avl), pointer :: raiz
        integer, intent(in) :: valor
        type(nodo_avl), pointer :: res, temp
        if (.not. associated(raiz)) then
            res => raiz
            return
        end if
        if (valor < raiz%valor) then
            raiz%izquierda => deleteRec(raiz%izquierda, valor)
        else if (valor > raiz%valor) then
            raiz%derecha => deleteRec(raiz%derecha, valor)
        else
            if (.not. associated(raiz%izquierda)) then
                temp => raiz%derecha
                deallocate(raiz)
                res => temp
            else if (.not. associated(raiz%derecha)) then
                temp => raiz%izquierda
                deallocate(raiz)
                res => temp
            else
                call obtenerMayorDeMenores(raiz%izquierda, temp)
                raiz%valor = temp%valor
                raiz%izquierda => deleteRec(raiz%izquierda, temp%valor)
            end if
        end if
        res => raiz
        if (.not. associated(raiz)) return
        raiz%factor = obtenerBalance(raiz)
        if (raiz%factor > 1) then
            if (obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDD(raiz, raiz%derecha)
                raiz => rotacionII(raiz, raiz%derecha)
            else
                raiz => rotacionII(raiz, raiz%derecha)
            end if
        end if
        if (obtenerBalance(raiz) < -1) then
            if (obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionII(raiz, raiz%izquierda)
                raiz => rotacionDD(raiz, raiz%izquierda)
            else
                raiz => rotacionDD(raiz, raiz%izquierda)
            end if
        end if
        res => raiz
    end function deleteRec

    recursive subroutine obtenerMayorDeMenores(raiz, mayor)
        type(nodo_avl), pointer :: raiz, mayor
        if (associated(raiz%derecha)) then
            call obtenerMayorDeMenores(raiz%derecha, mayor)
        else
            mayor => raiz
        end if
    end subroutine obtenerMayorDeMenores

    recursive function obtenerAltura(n) result(altura)
        type(nodo_avl), pointer :: n
        integer :: altura
        if (.not. associated(n)) then
            altura = 0
        else
            altura = max(obtenerAltura(n%izquierda), obtenerAltura(n%derecha)) + 1
        end if
    end function obtenerAltura

    function obtenerBalance(n) result(balance)
        type(nodo_avl), pointer :: n
        integer :: balance
        if (.not. associated(n)) then
            balance = 0
        else
            balance = obtenerAltura(n%derecha) - obtenerAltura(n%izquierda)
        end if
    end function obtenerBalance

    function valor_existe(self, valor) result(existe)
        class(arbol_avl), intent(inout) :: self
        integer, intent(in) :: valor
        logical :: existe
        type(nodo_avl), pointer :: nodo_encontrado
        nodo_encontrado => buscar_recursivo(self%raiz, valor)
        existe = associated(nodo_encontrado)
    end function valor_existe
    
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

    function trim_m(valor)
        integer, intent(in) :: valor
        character(len=32) :: trim_m
        write(trim_m,'(I0)') valor
        trim_m = trim(adjustl(trim_m))
    end function trim_m

    subroutine top_5_imagenes(self)
        class(arbol_avl), intent(inout) :: self
        integer :: max1, max2, max3, max4, max5
        integer :: id1, id2, id3, id4, id5
        max1 = 0
        max2 = 0
        max3 = 0
        max4 = 0
        max5 = 0
        id1 = 0
        id2 = 0
        id3 = 0
        id4 = 0
        id5 = 0
        call buscar_top_5(self%raiz, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        print *, "ID Imagen: ", trim_m(id1), "Total De Capas:  ", trim_m(max1)
        print *, "ID Imagen: ", trim_m(id2), "Total De Capas:  ", trim_m(max2)
        print *, "ID Imagen: ", trim_m(id3), "Total De Capas:  ", trim_m(max3)
        print *, "ID Imagen: ", trim_m(id4), "Total De Capas:  ", trim_m(max4)
        print *, "ID Imagen: ", trim_m(id5), "Total De Capas:  ", trim_m(max5)
    end subroutine top_5_imagenes
    
    recursive subroutine buscar_top_5(raiz, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        type(nodo_avl), pointer, intent(in) :: raiz
        integer, intent(inout) :: max1, max2, max3, max4, max5
        integer, intent(inout) :: id1, id2, id3, id4, id5
        integer :: num_nodos
        if (.not. associated(raiz)) return
        num_nodos = raiz%arbol_interno%numero_nodos()
        if (num_nodos > max1) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = max2
            id3 = id2
            max2 = max1
            id2 = id1
            max1 = num_nodos
            id1 = raiz%valor
        else if (num_nodos > max2) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = max2
            id3 = id2
            max2 = num_nodos
            id2 = raiz%valor
        else if (num_nodos > max3) then
            max5 = max4
            id5 = id4
            max4 = max3
            id4 = id3
            max3 = num_nodos
            id3 = raiz%valor
        else if (num_nodos > max4) then
            max5 = max4
            id5 = id4
            max4 = num_nodos
            id4 = raiz%valor
        else if (num_nodos > max5) then
            max5 = num_nodos
            id5 = raiz%valor
        end if
        call buscar_top_5(raiz%izquierda, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
        call buscar_top_5(raiz%derecha, max1, max2, max3, max4, max5, id1, id2, id3, id4, id5)
    end subroutine buscar_top_5
    
    subroutine graficar_arbol_imagen(this, nombre_grafica, valor_destacado)
        class(arbol_avl), intent(in) :: this
        character(len=*), intent(in) :: nombre_grafica
        integer, intent(in) :: valor_destacado 
        character(len=:), allocatable :: codigo_dot
        character(len=:), allocatable :: crear_nodo
        character(len=:), allocatable :: direccion_nodo
        crear_nodo = ''
        direccion_nodo = ''
        codigo_dot = "digraph G{" // new_line('a')
        codigo_dot = codigo_dot // "node [shape=doublecircle];" // new_line('a')
        codigo_dot = codigo_dot //&
        'Titulo [fontname="Courier New", color=red shape=box3d label="Arbol Binario AVL Imagen"]' &
        // new_line('a')
        codigo_dot = codigo_dot // "{rank=same; Titulo;}" // new_line('a')
        if (associated(this%raiz)) then
            call recorrer_arbol_avl(this%raiz, crear_nodo, direccion_nodo, valor_destacado) 
        end if
        codigo_dot = codigo_dot // trim(crear_nodo) // trim(direccion_nodo) // "}" // new_line('a')
        call generar_grafica(nombre_grafica, codigo_dot)
        print *, "Grafica '"//trim(nombre_grafica)//"' Generada Correctamente."
    end subroutine graficar_arbol_imagen
    
    recursive subroutine recorrer_arbol_avl(actual, crear_nodo, direccion_nodo, valor_destacado)
        type(nodo_avl), pointer :: actual
        character(len=:), allocatable, intent(inout) :: crear_nodo, direccion_nodo
        integer, intent(in) :: valor_destacado
        character(len=20) :: direccion, str_valor
        if (associated(actual)) then
            direccion = obtener_direccion_memoria_avl(actual)
            write(str_valor, '(I0)') actual%valor
            crear_nodo = crear_nodo // '"' // trim(direccion) // '"' // '[fontname="Courier New" label="' &
            // trim(str_valor) // '"];' // new_line('a')
            if (actual%valor == valor_destacado) then
                call actual%arbol_interno%generar_grafica(crear_nodo, direccion_nodo, direccion)
            end if
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
            call recorrer_arbol_avl(actual%izquierda, crear_nodo, direccion_nodo, valor_destacado) 
            call recorrer_arbol_avl(actual%derecha, crear_nodo, direccion_nodo, valor_destacado)
        end if
    end subroutine recorrer_arbol_avl

    !----------------------------------------------------------------------------------
    subroutine graficar_arbol(this,nombre_grafica)
        class(arbol_avl), intent(inout) :: this
        character(len=:), allocatable :: codigo_dot
        character(len=:), allocatable :: crear_nodo
        character(len=:), allocatable :: direccion_nodo
        character(len=*), intent(in) :: nombre_grafica
        crear_nodo = ''
        direccion_nodo = ''
        codigo_dot = "digraph G{" // new_line('a')
        codigo_dot = codigo_dot // "node [shape=doublecircle];" // new_line('a')
        codigo_dot = codigo_dot //&
        'Titulo [fontname="Courier New", color=red shape=box3d label="Arbol Binario AVL"]' &
        // new_line('a')
        codigo_dot = codigo_dot // "{rank=same; Titulo;}" // new_line('a')
        if (associated(this%raiz)) then
            call RoamTree(this%raiz, crear_nodo, direccion_nodo)
        end if
        codigo_dot = codigo_dot // trim(crear_nodo) // trim(direccion_nodo) // "}" // new_line('a')
        call generar_grafica(nombre_grafica, codigo_dot)
        print *, "Grafica '"//trim(nombre_grafica)//"' Generada Correctamente."
    end subroutine graficar_arbol

    recursive subroutine RoamTree(actual, crear_nodo, direccion_nodo)
            type(nodo_avl), pointer :: actual
            character(len=:), allocatable, intent(inout) :: crear_nodo
            character(len=:), allocatable, intent(inout) :: direccion_nodo
            character(len=20) :: direccion
            character(len=20) :: str_valor
            if (associated(actual)) then
            direccion = obtener_direccion_memoria_avl(actual)
            write(str_valor, '(I0)') actual%valor
            crear_nodo = crear_nodo // '"' // trim(direccion) // '"' // '[fontname="Courier New" label="' &
            // trim(str_valor) // '"];' // new_line('a')
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
            call RoamTree(actual%izquierda, crear_nodo, direccion_nodo)
            call RoamTree(actual%derecha, crear_nodo, direccion_nodo)
            end if
    end subroutine RoamTree

    !-----------------------------------------------------------------------------------------
    function obtener_direccion_memoria_avl(node) result(direccion)
        type(nodo_avl), pointer :: node
        character(len=20) :: direccion
        integer*8 :: i
        i = loc(node)
        write(direccion, 10) i 
        10 format(I0)
    end function obtener_direccion_memoria_avl

    subroutine generar_grafica(nombre_grafica, codigo)
        character(len=*), intent(in) :: codigo, nombre_grafica
        character(len=:), allocatable :: dot_nombre_grafica, png_nombre_grafica
        character(len=:), allocatable :: filepath
        dot_nombre_grafica = "graph/" // trim(nombre_grafica) // ".dot"
        png_nombre_grafica = "graph/" // trim(nombre_grafica) // ".png"
        filepath = 'graph/' // trim(nombre_grafica) 
        open(10, file=filepath, status='replace', action='write')
        write(10, '(A)') trim(codigo)
        close(10)
        call system('dot -Tpdf ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.pdf')
        call system('start ' // trim(adjustl(filepath)) // '.pdf')
    end subroutine generar_grafica

end module modulo_arbol_avl