module modulo_arbol_abb
    use modulo_matrix_dispersa
    implicit none
    private
    type :: nodo_abb
        integer :: valor
        type(nodo_abb), pointer :: derecha => null()
        type(nodo_abb), pointer :: izquierda => null()
        type(matriz) :: matriz
    end type nodo_abb
    type, public :: arbol_abb
        type(nodo_abb), pointer :: raiz => null()
    contains
        procedure :: insertar_nodo
        procedure :: recorrido_preorden
        procedure :: recorrido_inorden
        procedure :: recorrido_postorden
        procedure :: graficar_arbol
        procedure :: buscar_matriz
        procedure :: valor_existe
        procedure :: imprimir_hoja
        procedure :: profundidad_arbol
    end type arbol_abb

contains    
    
    subroutine insertar_nodo(self, valor, ma)
        class(arbol_abb), intent(inout) :: self
        integer, intent(in) :: valor
        type(matriz), intent(in) :: ma
        type(nodo_abb), pointer :: nuevo
        allocate(nuevo)
        nuevo = nodo_abb(valor=valor, matriz=ma)
        if (.not. associated(self%raiz)) then
            self%raiz => nuevo
        else
            call insertar_recursivo(self%raiz, nuevo)
        end if
    end subroutine insertar_nodo
    
    recursive subroutine insertar_recursivo(raiz, nuevo)
        type(nodo_abb), pointer, intent(inout) :: raiz
        type(nodo_abb), pointer, intent(in) :: nuevo
        if (nuevo%valor < raiz%valor) then
            if (.not. associated(raiz%izquierda)) then
                raiz%izquierda => nuevo
            else
                call insertar_recursivo(raiz%izquierda, nuevo)
            end if
        else if (nuevo%valor > raiz%valor) then
            if (.not. associated(raiz%derecha)) then
                raiz%derecha => nuevo
            else
                call insertar_recursivo(raiz%derecha, nuevo)
            end if
        end if
    end subroutine insertar_recursivo

    function valor_existe(self, valor) result(existe)
        class(arbol_abb), intent(inout) :: self
        integer, intent(in) :: valor
        logical :: existe
        type(nodo_abb), pointer :: nodo_encontrado
        nodo_encontrado => buscar_nodo(self%raiz, valor)
        existe = associated(nodo_encontrado)
    end function valor_existe
    
    function buscar_matriz(self, valor) result(ma)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: valor
        type(matriz) :: ma
        type(nodo_abb), pointer :: nodo
        nodo => buscar_nodo(self%raiz, valor)
        if (associated(nodo)) then
            ma = nodo%matriz
        end if
    end function buscar_matriz
    
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
    
    subroutine recorrido_preorden(self, num_nodos, cadena)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: num_nodos
        character(len=:), allocatable, intent(out) :: cadena
        integer :: contador_nodos
        contador_nodos = 0
        cadena = ""
        call preorder_recursivo(self%raiz, num_nodos, contador_nodos, cadena)
    end subroutine recorrido_preorden
    
    recursive subroutine preorder_recursivo(raiz, num_nodos, contador_nodos, cadena)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: num_nodos
        integer, intent(inout) :: contador_nodos
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
        if(associated(raiz) .and. contador_nodos < num_nodos) then
            write(valor_str, '(I0)') raiz%valor
            cadena = trim(cadena) // trim(valor_str) // " - "
            contador_nodos = contador_nodos + 1
            call preorder_recursivo(raiz%izquierda, num_nodos, contador_nodos, cadena)
            call preorder_recursivo(raiz%derecha, num_nodos, contador_nodos, cadena)
        end if
    end subroutine preorder_recursivo 
    
    subroutine recorrido_inorden(self, num_nodos, cadena)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: num_nodos
        character(len=:), allocatable, intent(out) :: cadena
        integer :: contador_nodos
        contador_nodos = 0
        cadena = ""
        call inorden_recursivo(self%raiz, num_nodos, contador_nodos, cadena)
    end subroutine recorrido_inorden
    
    recursive subroutine inorden_recursivo(raiz, num_nodos, contador_nodos, cadena)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: num_nodos
        integer, intent(inout) :: contador_nodos
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
        if(associated(raiz) .and. contador_nodos < num_nodos) then
            call inorden_recursivo(raiz%izquierda, num_nodos, contador_nodos, cadena)
            if (contador_nodos < num_nodos) then
                write(valor_str, '(I0)') raiz%valor
                cadena = trim(cadena) // trim(valor_str) // " - "
                contador_nodos = contador_nodos + 1
            end if
            call inorden_recursivo(raiz%derecha, num_nodos, contador_nodos, cadena)
        end if
    end subroutine inorden_recursivo
    
    subroutine recorrido_postorden(self, num_nodos, cadena)
        class(arbol_abb), intent(in) :: self
        integer, intent(in) :: num_nodos
        character(len=:), allocatable, intent(out) :: cadena
        integer :: contador_nodos
        contador_nodos = 0
        cadena = ""
        call posorden_recursivo(self%raiz, num_nodos, contador_nodos, cadena)
    end subroutine recorrido_postorden
    
    recursive subroutine posorden_recursivo(raiz, num_nodos, contador_nodos, cadena)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer, intent(in) :: num_nodos
        integer, intent(inout) :: contador_nodos
        character(len=:), allocatable, intent(inout) :: cadena
        character(len=20) :: valor_str
        if(associated(raiz) .and. contador_nodos < num_nodos) then
            call posorden_recursivo(raiz%izquierda, num_nodos, contador_nodos, cadena)
            call posorden_recursivo(raiz%derecha, num_nodos, contador_nodos, cadena)
            if (contador_nodos < num_nodos) then
                write(valor_str, '(I0)') raiz%valor
                cadena = trim(cadena) // trim(valor_str) // " - "
                contador_nodos = contador_nodos + 1
            end if
        end if
    end subroutine posorden_recursivo

    subroutine imprimir_hoja(self)
        class(arbol_abb), intent(in) :: self
        print *, "Capas Que Son Hojas"
        call imprimir_hoja_recursivo(self%raiz)
    end subroutine imprimir_hoja
    
    recursive subroutine imprimir_hoja_recursivo(raiz)
        type(nodo_abb), pointer, intent(in) :: raiz
        if (associated(raiz)) then
            if (.not. associated(raiz%izquierda) .and. .not. associated(raiz%derecha)) then
                print*,"Capa: ",raiz%valor
            else
                call imprimir_hoja_recursivo(raiz%izquierda)
                call imprimir_hoja_recursivo(raiz%derecha)
            end if
        end if
    end subroutine imprimir_hoja_recursivo
    
    subroutine profundidad_arbol(self)
        class(arbol_abb), intent(in) :: self
        integer :: profundidad
        profundidad = profundidad_recursivo(self%raiz)
        print *, "Profundidad del Arbol:", profundidad
    end subroutine profundidad_arbol
    
    recursive function profundidad_recursivo(raiz) result(profundidad)
        type(nodo_abb), pointer, intent(in) :: raiz
        integer :: profundidad
        integer :: profundidad_izquierda, profundidad_derecha
        if (.not. associated(raiz)) then
            profundidad = 0
        else
            profundidad_izquierda = profundidad_recursivo(raiz%izquierda)
            profundidad_derecha = profundidad_recursivo(raiz%derecha)
            profundidad = max(profundidad_izquierda, profundidad_derecha) + 1
        end if
    end function profundidad_recursivo
    
    subroutine graficar_arbol(self, nombre_archivo)
        class(arbol_abb), intent(in) :: self
        character(len=*), intent(in) :: nombre_archivo
        character(len=:), allocatable :: codigo_dot
        character(len=:), allocatable :: crear_nodo
        character(len=:), allocatable :: enlace_nodo
        crear_nodo = ''
        enlace_nodo = ''
        codigo_dot = "digraph G{" // new_line('a')
        codigo_dot = codigo_dot // "node [shape=doublecircle];" // new_line('a')
        codigo_dot = codigo_dot //&
        'Titulo [fontname="Courier New", color=red shape=box3d label="Arbol Binario De Busqueda Capas"]' &
        // new_line('a')
        codigo_dot = codigo_dot // "{rank=same; Titulo;}" // new_line('a')
        if (associated(self%raiz)) then
            call recorrer_arbol_abb(self%raiz, crear_nodo, enlace_nodo)
        end if
        codigo_dot = codigo_dot // trim(crear_nodo) // trim(enlace_nodo) // "}" // new_line('a')
        call generar_grafica(nombre_archivo, codigo_dot)
        print *, "Grafica '"//trim(nombre_archivo)//"' Generada Correctamente."
    end subroutine graficar_arbol
    
    recursive subroutine recorrer_arbol_abb(actual, crear_nodo, enlace_nodo)
        type(nodo_abb), pointer :: actual
        character(len=:), allocatable, intent(inout) :: crear_nodo, enlace_nodo
        character(len=20) :: direccion, str_valor
        if (associated(actual)) then
            direccion = obtener_direccion_memoria(actual)
            write(str_valor, '(I0)') actual%valor
            crear_nodo = crear_nodo // '"' // trim(direccion) // '"' // '[fontname="Courier New" label="' &
            // trim(str_valor) // '"];' // new_line('a')
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
            call recorrer_arbol_abb(actual%izquierda, crear_nodo, enlace_nodo)
            call recorrer_arbol_abb(actual%derecha, crear_nodo, enlace_nodo)
        end if
    end subroutine recorrer_arbol_abb
    
    subroutine generar_grafica(nombre_archivo, codigo)
        character(len=*), intent(in) :: codigo, nombre_archivo
        character(len=:), allocatable :: dot_nombre_archivo, png_nombre_archivo
        character(len=:), allocatable :: filepath
        dot_nombre_archivo = "graph/" // trim(nombre_archivo) // ".dot"
        png_nombre_archivo = "graph/" // trim(nombre_archivo) // ".png"
        filepath = 'graph/' // trim(nombre_archivo) 
        open(10, file=filepath, status='replace', action='write')
        write(10, '(A)') trim(codigo)
        close(10)
        call system('dot -Tpdf ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.pdf')
        call system('start ' // trim(adjustl(filepath)) // '.pdf')
    end subroutine generar_grafica

    function obtener_direccion_memoria(nodo) result(direccion)
        type(nodo_abb), pointer :: nodo
        character(len=20) :: direccion
        integer*8 :: i
        i = loc(nodo)
        write(direccion, 10) i 
        10 format(I0)
    end function obtener_direccion_memoria

end module modulo_arbol_abb