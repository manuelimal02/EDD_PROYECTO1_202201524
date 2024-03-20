module modulo_arbol_abb
    implicit none
    private
    type :: nodo_abb
        integer :: valor
        type(nodo_abb), pointer :: derecha => null()
        type(nodo_abb), pointer :: izquierda => null()
    end type nodo_abb
    type, public :: arbol_abb
        type(nodo_abb), pointer :: raiz => null()
    contains
        procedure :: insertar
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
    end type arbol_abb

contains    

    subroutine insertar(self, valor)
        class(arbol_abb), intent(inout) :: self
        integer, intent(in) :: valor
        if (.not. associated(self%raiz)) then
            allocate(self%raiz)
            self%raiz%valor = valor
        else
            call insertRec(self%raiz, valor)
        end if
    end subroutine insertar


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


    subroutine delete(self, valor)
        class(arbol_abb), intent(inout) :: self
        integer, intent(inout) :: valor
    
        self%raiz => deleteRec(self%raiz, valor)
    end subroutine delete
    recursive function deleteRec(raiz, valor) result(res)
        type(nodo_abb), pointer :: raiz
        integer, intent(in) :: valor
        type(nodo_abb), pointer :: res
        type(nodo_abb), pointer :: temp

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
                return
            else if (.not. associated(raiz%derecha)) then
                temp => raiz%izquierda
                deallocate(raiz)
                res => temp
                return
            else
                call getMajorOfMinors(raiz%izquierda, temp)
                raiz%valor = temp%valor
                raiz%izquierda => deleteRec(raiz%izquierda, temp%valor)
            end if
        end if

        res => raiz
    end function deleteRec


    recursive subroutine getMajorOfMinors(raiz, major)
        type(nodo_abb), pointer :: raiz, major
        if (associated(raiz%derecha)) then
            call getMajorOfMinors(raiz%derecha, major)
        else
            major => raiz
        end if
    end subroutine getMajorOfMinors

    subroutine preorder(self)
        class(arbol_abb), intent(in) :: self
        call preorderRec(self%raiz)
        write(*, '()')
    end subroutine preorder


    recursive subroutine preorderRec(raiz)
        type(nodo_abb), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            ! RAIZ - IZQ - DER
            write(*, '(I0 A)', advance='no') raiz%valor, " - "
            call preorderRec(raiz%izquierda)
            call preorderRec(raiz%derecha)
        end if
    end subroutine preorderRec

    subroutine inorder(self)
        class(arbol_abb), intent(in) :: self
        
        call inordenRec(self%raiz)
        print *, ""
    end subroutine inorder

    
    recursive subroutine inordenRec(raiz)
        type(nodo_abb), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            ! IZQ - RAIZ - DER
            call inordenRec(raiz%izquierda)
            write(*, '(I0 A)', advance='no') raiz%valor, " - "
            call inordenRec(raiz%derecha)
        end if
    end subroutine inordenRec

    subroutine posorder(self)
        class(arbol_abb), intent(in) :: self
        
        call posordenRec(self%raiz)
        print *, ""
    end subroutine posorder
    
    recursive subroutine posordenRec(raiz)
        type(nodo_abb), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            ! IZQ - DER - RAIZ
            call posordenRec(raiz%izquierda)
            call posordenRec(raiz%derecha)
            write(*, '(I0 A)', advance='no') raiz%valor, " - "
        end if
    end subroutine posordenRec

    subroutine graph(self, filename)
        class(arbol_abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

        if (associated(self%raiz)) then
            call RoamTree(self%raiz, createNodes, linkNodes)
        end if
        
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graph
    
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(nodo_abb), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_valor

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address_memory(current)
          write(str_valor, '(I0)') current%valor
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_valor) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%izquierda)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%izquierda)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')
    
          end if
          ! VIAJAMOS A LA SUBRAMA DER
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
    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        
        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        
        open(10, file="graph/"//dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)

        ! Genera la imagen PNG
        call system("dot -Tpng graph/"// dot_filename //" -o graph/" // png_filename)
    end subroutine write_dot

    function get_address_memory(node) result(address)
        type(nodo_abb), pointer :: node
        character(len=20) :: address
        integer*8 :: i
        i = loc(node) ! get the address of x
        ! convert the address to string
        write(address, 10) i 
        10 format(I0)
    
    end function get_address_memory

end module modulo_arbol_abb