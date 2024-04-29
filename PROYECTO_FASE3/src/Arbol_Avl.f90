module modulo_arbol_avl
    use modulo_encriptacion
    use modulo_tabla_hash
    implicit none
    private
    public :: nodo_avl 
    public :: arbol_avl
    type :: nodo_avl
        integer :: valor
        character(:), allocatable :: departamento
        character(:), allocatable :: direccion
        character(:), allocatable :: contrasena
        integer :: altura = 1
        type(nodo_avl), pointer :: derecha => null()
        type(nodo_avl), pointer :: izquierda => null() 
        type(TablaHash) :: tabla
    end type
    type arbol_avl
        type(nodo_avl), pointer :: raiz => null()
    contains
        procedure :: insertar_nodo
        procedure :: obtener_nodo
        procedure :: valor_existe
        procedure :: graficar_arbol
    end type arbol_avl

    contains
    subroutine insertar_nodo(self, valor, departamento, direccion, contrasena)
        class(arbol_avl), intent(inout) :: self
        integer, intent(in) :: valor
        character(len=*), intent(in) :: departamento, direccion, contrasena
        call insertar_recursivo(self%raiz, valor, departamento, direccion, contrasena)
    end subroutine insertar_nodo
    
    recursive subroutine insertar_recursivo(raiz, valor, departamento, direccion, contrasena)
        type(nodo_avl), pointer, intent(inout) :: raiz
        integer, intent(in) :: valor
        character(len=*), intent(in) :: departamento, direccion, contrasena
        if(.not. associated(raiz)) then
            allocate(raiz)
            raiz = nodo_avl(valor=valor, departamento=departamento, direccion=direccion, contrasena=contrasena)
        else if(valor < raiz%valor) then 
            call insertar_recursivo(raiz%izquierda, valor, departamento, direccion, contrasena)
        else if(valor > raiz%valor) then
            call insertar_recursivo(raiz%derecha, valor, departamento, direccion, contrasena)
        end if
        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        if(obtenerBalance(raiz) > 1) then
            if(obtenerBalance(raiz%derecha) < 0) then
                raiz%derecha => rotacionDerecha(raiz%derecha)
                raiz => rotacionIzquierda(raiz)
            else
                raiz => rotacionIzquierda(raiz)
            end if
        end if
        if(obtenerBalance(raiz) < -1) then
            if(obtenerBalance(raiz%izquierda) > 0) then
                raiz%izquierda => rotacionIzquierda(raiz%izquierda)
                raiz => rotacionDerecha(raiz)
            else
                raiz => rotacionDerecha(raiz)
            end if
        end if
    end subroutine insertar_recursivo

    function valor_existe(self, valor, contrasena) result(existe)
        class(arbol_avl), intent(inout) :: self
        integer, intent(in) :: valor
        character(len=*), intent(in) :: contrasena
        logical :: existe
        type(nodo_avl), pointer :: nodo_encontrado
        nodo_encontrado => buscar_recursivo(self%raiz, valor, contrasena)
        existe = associated(nodo_encontrado)
    end function valor_existe
    
    function obtener_nodo(self, valor, contrasena) result(nodo_encontrado)
        class(arbol_avl), intent(inout) :: self
        integer, intent(in) :: valor
        character(len=*), intent(in) :: contrasena
        type(nodo_avl), pointer :: nodo_encontrado
        nodo_encontrado => buscar_recursivo(self%raiz, valor, contrasena)
    end function obtener_nodo

    recursive function buscar_recursivo(raiz, valor, contrasena) result(nodo_resultado)
        type(nodo_avl), pointer :: raiz, nodo_resultado
        integer, intent(in) :: valor
        character(len=*), intent(in) :: contrasena

        if (.not. associated(raiz)) then
            nodo_resultado => null()
        else if (valor < raiz%valor) then
            nodo_resultado => buscar_recursivo(raiz%izquierda, valor, contrasena)
        else if (valor > raiz%valor) then
            nodo_resultado => buscar_recursivo(raiz%derecha, valor, contrasena)
        else if (raiz%contrasena == contrasena) then
            nodo_resultado => raiz
        else
            nodo_resultado => null()
        end if
    end function buscar_recursivo

    function rotacionIzquierda(raiz) result(raizDerecha)
        type(nodo_avl), pointer, intent(in) :: raiz
        type(nodo_avl), pointer :: raizDerecha
        type(nodo_avl), pointer :: temp
        raizDerecha => raiz%derecha
        temp => raizDerecha%izquierda
        raizDerecha%izquierda => raiz
        raiz%derecha => temp
        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizDerecha%altura = maximo(obtenerAltura(raizDerecha%izquierda), obtenerAltura(raizDerecha%derecha)) + 1
    end function rotacionIzquierda
    
    function rotacionDerecha(raiz) result(raizIzquierda)
        type(nodo_avl), pointer, intent(in) :: raiz
        type(nodo_avl), pointer :: raizIzquierda
        type(nodo_avl), pointer :: temp
        raizIzquierda => raiz%izquierda
        temp => raizIzquierda%derecha
        raizIzquierda%derecha => raiz
        raiz%izquierda => temp
        raiz%altura = maximo(obtenerAltura(raiz%izquierda), obtenerAltura(raiz%derecha)) + 1
        raizIzquierda%altura = maximo(obtenerAltura(raizIzquierda%izquierda), obtenerAltura(raizIzquierda%derecha)) + 1
    end function rotacionDerecha
    
    recursive subroutine obtenerMayorDeMenores(raiz, mayor)
        type(nodo_avl), pointer :: raiz, mayor
        if(associated(raiz%derecha)) then
            call obtenerMayorDeMenores(raiz%derecha, mayor)
        else
            mayor => raiz
        end if
    end subroutine obtenerMayorDeMenores
    
    function maximo(izquierda, derecha) result(res)
        integer, intent(in) :: izquierda
        integer, intent(in) :: derecha
        integer :: res
        res = derecha
        if(izquierda >= derecha) then
            res = izquierda
            return
        end if
    end function maximo
    
    function obtenerBalance(raiz) result(res)
        type(nodo_avl), pointer, intent(in) :: raiz
        integer :: res
        res = obtenerAltura(raiz%derecha) - obtenerAltura(raiz%izquierda)
    end function
    
    function obtenerAltura(n) result(res)
        type(nodo_avl), pointer :: n
        integer :: res
        res = 0
        if(.not. associated(n)) return
        res = n%altura
    end function obtenerAltura

    subroutine graficar_arbol(this,nombre_grafica)
        class(arbol_avl), intent(inout) :: this
        character(len=:), allocatable :: codigo_dot
        character(len=:), allocatable :: crear_nodo
        character(len=:), allocatable :: direccion_nodo
        character(len=*), intent(in) :: nombre_grafica
        crear_nodo = ''
        direccion_nodo = ''
        codigo_dot = "digraph G{" // new_line('a')
        codigo_dot = codigo_dot // "node [shape=component];" // new_line('a')
        codigo_dot = codigo_dot //&
        'Titulo [fontname="Courier New", color=red shape=box3d label="Arbol Avl Sucursales"]' &
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
            //"ID: "//trim(str_valor) // new_line('a') // &
            "Departamento: "//actual%departamento // new_line('a') // &
            "Direccion: "//actual%direccion // new_line('a') // &
            "Password: "//sha256(actual%contrasena) // &
            '"];' // new_line('a')
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
        character(len=:), allocatable :: filepath
        filepath = 'graph/' // trim(nombre_grafica) 
        open(10, file=filepath, status='replace', action='write')
        write(10, '(A)') trim(codigo)
        close(10)
        call system('dot -Tpdf ' // trim(filepath) // ' -o ' // trim(adjustl(filepath)) // '.pdf')
        call system('start ' // trim(adjustl(filepath)) // '.pdf')
    end subroutine generar_grafica
    
end module modulo_arbol_avl
