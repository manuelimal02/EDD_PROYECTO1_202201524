!COLA DE CLIENTE EN RECEPCION--------------------------------------------------
module modulo_cola_cliente
    implicit none
    type :: cola_cliente
    type(nodo_cola_cliente), pointer :: cabeza => null()
    contains
        procedure :: push_cliente
        procedure :: pop_cliente
        procedure :: print_cliente
    end type cola_cliente

    type :: nodo_cola_cliente
        character(len=:), allocatable :: id_cliente
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: img_grande
        character(len=:), allocatable :: img_pequena
        type(nodo_cola_cliente), pointer :: siguiente
    end type nodo_cola_cliente

    contains
    subroutine push_cliente(self, id_cliente, nombre, img_grande, img_pequena)
        class(cola_cliente), intent(inout) :: self
        character(len=*), intent(in) :: id_cliente, nombre, img_grande, img_pequena
        
        type(nodo_cola_cliente), pointer :: actual, nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%id_cliente = id_cliente
        nuevo_nodo%nombre = nombre
        nuevo_nodo%img_grande = img_grande
        nuevo_nodo%img_pequena = img_pequena
        nuevo_nodo%siguiente => null()
    
        if (.not. associated(self%cabeza)) then
            self%cabeza => nuevo_nodo
        else
            actual => self%cabeza
            do while (associated(actual%siguiente))
                actual => actual%siguiente
            end do
            actual%siguiente => nuevo_nodo
        end if
    end subroutine push_cliente

    subroutine pop_cliente(self, info_cliente)
        class(cola_cliente), intent(inout) :: self
        type(nodo_cola_cliente), pointer :: temp
        character(len=20), dimension(:), intent(out) :: info_cliente
    
        if (.not. associated(self%cabeza)) then
            info_cliente(1) = "NULL"
            info_cliente(2) = "NULL"
            info_cliente(3) = "0"
            info_cliente(4) = "0"
        else
            temp => self%cabeza
            info_cliente(1) = self%cabeza%id_cliente
            info_cliente(2) = self%cabeza%nombre
            info_cliente(3) = self%cabeza%img_pequena 
            info_cliente(4) = self%cabeza%img_grande
            self%cabeza => self%cabeza%siguiente
            deallocate(temp)
        end if
    end subroutine pop_cliente

    subroutine print_cliente(self)
        class(cola_cliente), intent(in) :: self
        type(nodo_cola_cliente), pointer :: actual
        actual => self%cabeza
        do while (associated(actual))
            print *, "------------------------"
            print *, "ID: ", actual%id_cliente
            print *, "Nombre: ",actual%nombre
            print *, "Imagenes Pequenas: ",actual%img_pequena
            print *, "Imagenes Grandes: ",actual%img_grande
            actual => actual%siguiente
        end do
    end subroutine print_cliente
end module modulo_cola_cliente

!PILA DE IMAGENES------------------------------------------------------
module modulo_pila_imagenes
    type :: pila_imagenes
    type(nodo_pila_imagen), pointer :: cabeza => null()
    contains
        procedure :: push_imagen
        procedure :: pop_imagen
        procedure :: print_imagen
        procedure :: clean_imagen
    end type pila_imagenes
    type :: nodo_pila_imagen
        character(len=:), allocatable :: tipo_imagen
        type(nodo_pila_imagen), pointer :: siguiente
    end type nodo_pila_imagen

    contains
    subroutine push_imagen(self,tipo_imagen)
        class(pila_imagenes), intent(inout) :: self
        character(len=*), intent(in) :: tipo_imagen
        type(nodo_pila_imagen), pointer :: nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%tipo_imagen = tipo_imagen
        nuevo_nodo%siguiente => self%cabeza
        self%cabeza => nuevo_nodo
    end subroutine push_imagen  

    subroutine pop_imagen(self, tipo_imagen)
        class(pila_imagenes), intent(inout) :: self
        character(len=20), dimension(:), intent(out) :: tipo_imagen
        type(nodo_pila_imagen), pointer :: temp
        if (.not. associated(self%cabeza)) then
            print *, "LA PILA ESTA VACIA"
        else
            temp => self%cabeza
            tipo_imagen(1) = self%cabeza%tipo_imagen
            self%cabeza => self%cabeza%siguiente
            deallocate(temp)
        end if
    end subroutine pop_imagen

    subroutine print_imagen(self)
        class(pila_imagenes), intent(in) :: self
        type(nodo_pila_imagen), pointer :: actual
        actual => self%cabeza
        do while (associated(actual))
            print *, actual%tipo_imagen
            actual => actual%siguiente
        end do
    end subroutine print_imagen

    subroutine clean_imagen(self)
        class(pila_imagenes), intent(inout) :: self
        type(nodo_pila_imagen), pointer :: temp
        do while (associated(self%cabeza))
            temp => self%cabeza
            self%cabeza => self%cabeza%siguiente
            deallocate(temp)
        end do
    end subroutine clean_imagen
end module modulo_pila_imagenes

!COLA DE IMAGENES PEQUEÑAS--------------------------------------------------
module modulo_cola_impresora_pequena
    use modulo_pila_imagenes
    implicit none
    type :: cola_impresora_pequena
    type(nodo_impresora_pequena), pointer :: cabeza => null()
    contains
        procedure :: push_img_pequena
        procedure :: pop_img_pequena
        procedure :: print_img_pequena
    end type cola_impresora_pequena
    type :: nodo_impresora_pequena
        character(len=:), allocatable :: tipo_imagen
        type(nodo_impresora_pequena), pointer :: siguiente
    end type nodo_impresora_pequena

    contains
    subroutine push_img_pequena(self, pila)
        class(cola_impresora_pequena), intent(inout) :: self
        type(pila_imagenes), intent(in) :: pila
        type(nodo_impresora_pequena), pointer :: actual, nuevo_nodo
        type(nodo_pila_imagen), pointer :: temp
        character(len=:), allocatable :: tipo_imagen
        temp => pila%cabeza
        do while (associated(temp))
            tipo_imagen = temp%tipo_imagen
            if (tipo_imagen == "Pequena") then
                allocate(nuevo_nodo)
                nuevo_nodo%tipo_imagen = tipo_imagen
                nuevo_nodo%siguiente => null()
                if (.not. associated(self%cabeza)) then
                    self%cabeza => nuevo_nodo
                else
                    actual => self%cabeza
                    do while (associated(actual%siguiente))
                        actual => actual%siguiente
                    end do
                    actual%siguiente => nuevo_nodo
                end if
            end if
            temp => temp%siguiente
        end do
    end subroutine push_img_pequena

    subroutine pop_img_pequena(self, tipo_imagen)
        class(cola_impresora_pequena), intent(inout) :: self
        character(len=:), allocatable :: tipo_imagen
        type(nodo_impresora_pequena), pointer :: temp
        if (.not. associated(self%cabeza)) then
            print *, "LA COLA ESTA VACIA"
        else
            temp => self%cabeza
            tipo_imagen=self%cabeza%tipo_imagen
            self%cabeza => self%cabeza%siguiente
            deallocate(temp)
        end if
    end subroutine pop_img_pequena

    subroutine print_img_pequena(self)
        class(cola_impresora_pequena), intent(in) :: self
        type(nodo_impresora_pequena), pointer :: actual
        print*, "COLA IMPRESORA PEQUENA"
        actual => self%cabeza
        do while (associated(actual))
            print *, "Imagen: ",actual%tipo_imagen
            actual => actual%siguiente
        end do
    end subroutine print_img_pequena
    
end module modulo_cola_impresora_pequena

!COLA DE IMAGENES GRANDES--------------------------------------------------
module modulo_cola_impresora_grande
    use modulo_pila_imagenes
    implicit none
    type :: cola_impresora_grande
    type(nodo_impresora_grande), pointer :: cabeza => null()
    contains
        procedure :: push_img_grande
        procedure :: print_img_grande
    end type cola_impresora_grande
    type :: nodo_impresora_grande
        character(len=:), allocatable :: tipo_imagen
        type(nodo_impresora_grande), pointer :: siguiente
    end type nodo_impresora_grande

    contains
    subroutine push_img_grande(self, pila)
        class(cola_impresora_grande), intent(inout) :: self
        type(pila_imagenes), intent(inout) :: pila
        type(nodo_impresora_grande), pointer :: actual, nuevo_nodo
        character(len=:), allocatable :: tipo_imagen
        do while (associated(pila%cabeza))
            tipo_imagen = pila%cabeza%tipo_imagen
            if (tipo_imagen == "Grande") then
                allocate(nuevo_nodo)
                nuevo_nodo%tipo_imagen = tipo_imagen
                nuevo_nodo%siguiente => null()
                if (.not. associated(self%cabeza)) then
                    self%cabeza => nuevo_nodo
                else
                    actual => self%cabeza
                    do while (associated(actual%siguiente))
                        actual => actual%siguiente
                    end do
                    actual%siguiente => nuevo_nodo
                end if
            end if
            pila%cabeza => pila%cabeza%siguiente
        end do
    end subroutine push_img_grande

    subroutine print_img_grande(self)
        class(cola_impresora_grande), intent(in) :: self
        type(nodo_impresora_grande), pointer :: actual
        actual => self%cabeza
        print*, "COLA IMPRESORA GRANDE"
        do while (associated(actual))
            print *, "Imagen: ",actual%tipo_imagen
            actual => actual%siguiente
        end do
    end subroutine print_img_grande
    
end module modulo_cola_impresora_grande

!LISTA CLIENTES ESPERA-----------------------------------------------------------------------------
module modulo_lista_cliente_espera
    implicit none
    type :: nodo_cliente_espera
    integer :: numero_ventanilla, cantidad_paso
    character(len=:), allocatable :: id_cliente
    character(len=:), allocatable :: nombre
    character(len=:), allocatable :: img_grande
    character(len=:), allocatable :: img_pequena
        type(nodo_cliente_espera), pointer :: anterior, siguiente
    end type nodo_cliente_espera

    type :: lista_cliente_espera
        type(nodo_cliente_espera), pointer :: cabeza => null()
    contains
        procedure :: append_cliente
        procedure :: delete_cliente
        procedure :: print_lista
    end type lista_cliente_espera

    contains
    subroutine append_cliente(self, id_cliente, nombre, img_pequena, img_grande, numero_ventanilla, cantidad_paso)
        class(lista_cliente_espera), intent(inout) :: self
        character(len=*), intent(in) :: id_cliente, nombre, img_pequena, img_grande
        integer, intent(in) :: numero_ventanilla, cantidad_paso
        type(nodo_cliente_espera), pointer :: nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%id_cliente = id_cliente
        nuevo_nodo%nombre = nombre
        nuevo_nodo%img_pequena = img_pequena
        nuevo_nodo%img_grande = img_grande
        nuevo_nodo%numero_ventanilla = numero_ventanilla
        nuevo_nodo%cantidad_paso = cantidad_paso
        if (.not. associated(self%cabeza)) then
            self%cabeza => nuevo_nodo
            nuevo_nodo%anterior => nuevo_nodo
            nuevo_nodo%siguiente => nuevo_nodo
        else
            nuevo_nodo%anterior => self%cabeza%anterior
            nuevo_nodo%siguiente => self%cabeza
            self%cabeza%anterior%siguiente => nuevo_nodo
            self%cabeza%anterior => nuevo_nodo
        end if
    end subroutine append_cliente

    subroutine delete_cliente(self, id_cliente)
        class(lista_cliente_espera), intent(inout) :: self
        character(len=*), intent(in) :: id_cliente
        type(nodo_cliente_espera), pointer :: actual
        actual => self%cabeza
        do while (associated(actual) .and. actual%id_cliente /= id_cliente)
            actual => actual%siguiente
        end do
        if (associated(actual)) then
            actual%anterior%siguiente => actual%siguiente
            actual%siguiente%anterior => actual%anterior
            if (associated(self%cabeza, actual)) then
                if (associated(actual, actual%siguiente)) then
                    self%cabeza => null()
                else
                    self%cabeza => actual%siguiente
                end if
            end if
            deallocate(actual)
        end if
    end subroutine delete_cliente

    subroutine print_lista(self)
        class(lista_cliente_espera), intent(in) :: self
        type(nodo_cliente_espera), pointer :: actual
        if (.not. associated(self%cabeza)) then
            print *, "LISTA CLIENTE VACIA."
        else
            actual => self%cabeza
            do
                print *, "ID Cliente: ", actual%id_cliente
                print *, "Nombre: ", actual%nombre
                print *, "Imagen Pequena: ", actual%img_pequena
                print *, "Imagen Grande: ", actual%img_grande
                print *, "Ventanilla: ", actual%numero_ventanilla
                print *, "Cantidad Paso: ", actual%cantidad_paso
                print *, "------------------------"
                actual => actual%siguiente
                if (associated(actual, self%cabeza)) exit
            end do
        end if
    end subroutine print_lista
end module modulo_lista_cliente_espera

!LISTA VENTANILA-----------------------------------------------------------------------------
module modulo_lista_ventanilla
    use modulo_pila_imagenes
    use modulo_cola_impresora_pequena
    use modulo_cola_impresora_grande
    use modulo_lista_cliente_espera
    implicit none
    type :: lista_ventanilla
        type(nodo_lista_cliente), pointer :: cabeza => null()
        type(cola_impresora_pequena) :: cola_imagen_pequena
        type(cola_impresora_grande) :: cola_imagen_grande
        type(lista_cliente_espera) :: lista_clientes_esperando
    contains
        procedure :: agregar_ventanilla
        procedure :: print_ventanilla
        procedure :: asignar_ventanilla
        procedure :: ventanilla_disponible
        procedure :: atender_cliente_ventanilla
    end type lista_ventanilla

    type :: nodo_lista_cliente
        integer :: numero_ventanilla, pequena, grande
        character(len=:), allocatable :: id_cliente
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: img_grande
        character(len=:), allocatable :: img_pequena
        logical :: ocupada = .false.
        type(pila_imagenes) :: pila
        type(nodo_lista_cliente), pointer :: siguiente
    end type nodo_lista_cliente
    integer :: cantidad_paso=1

    contains
    subroutine agregar_ventanilla(self, numero_ventanilla, id_cliente, nombre, img_pequena, img_grande)
        class(lista_ventanilla), intent(inout) :: self
        integer, intent(in) :: numero_ventanilla
        character(len=*), intent(in) :: id_cliente, nombre, img_grande, img_pequena
        type(nodo_lista_cliente), pointer :: actual, nuevo_nodo
        allocate(nuevo_nodo)
        nuevo_nodo%numero_ventanilla = numero_ventanilla
        nuevo_nodo%id_cliente = id_cliente
        nuevo_nodo%nombre = nombre
        nuevo_nodo%img_pequena = img_pequena
        nuevo_nodo%img_grande = img_grande
        nuevo_nodo%pequena = 0
        nuevo_nodo%grande = 0
        nuevo_nodo%ocupada = .false.
        nuevo_nodo%siguiente => null()
        if (.not. associated(self%cabeza)) then
            self%cabeza => nuevo_nodo
        else
            actual => self%cabeza
            do while (associated(actual%siguiente))
                actual => actual%siguiente
            end do
            actual%siguiente => nuevo_nodo
        end if
    end subroutine agregar_ventanilla  

    subroutine print_ventanilla(self)
        class(lista_ventanilla), intent(in) :: self
        type(nodo_lista_cliente), pointer :: actual
        actual => self%cabeza
        do while (associated(actual))
            print *, "------------------------"
            print *, "Ventanilla: ", actual%numero_ventanilla
            print *, "ID: ", actual%id_cliente
            print *, "Nombre: ", actual%nombre
            print *, "Img Pequena: ", actual%img_pequena
            print *, "Img Grande: ", actual%img_grande
            print *, "Pequena: ", actual%pequena
            print *, "Grande: ", actual%grande
            print *, "Ocupada: ", actual%ocupada
            actual => actual%siguiente
        end do
    end subroutine print_ventanilla

    logical function ventanilla_disponible(self) result (hay_ventanilla_disponible)
        class(lista_ventanilla), intent(inout) :: self
        type(nodo_lista_cliente), pointer :: actual
        hay_ventanilla_disponible = .false.
        actual => self%cabeza
        do while (associated(actual))
            if (.not. actual%ocupada) then
                hay_ventanilla_disponible = .true.
                exit
            end if
            actual => actual%siguiente
        end do
    end function ventanilla_disponible

    subroutine asignar_ventanilla(self, id_cliente, nombre, img_pequena, img_grande)
        class(lista_ventanilla), intent(inout) :: self
        integer :: pequena, grande
        character(len=*), intent(in) :: id_cliente, nombre, img_pequena, img_grande
        type(nodo_lista_cliente), pointer :: actual
        READ(img_pequena, *) pequena
        READ(img_grande, *) grande
        actual => self%cabeza
        do while (associated(actual))
            if (.not. actual%ocupada) then
                actual%id_cliente = id_cliente
                actual%nombre = nombre
                actual%img_pequena = img_pequena
                actual%img_grande = img_grande
                actual%pequena = pequena
                actual%grande = grande
                actual%ocupada = .true.
                exit
            end if
            actual => actual%siguiente
        end do
    end subroutine asignar_ventanilla

    subroutine atender_cliente_ventanilla(self) 
        class(lista_ventanilla), intent(inout) :: self
        type(nodo_lista_cliente), pointer :: actual
        integer :: num_img_grandes, num_img_pequenas
        actual => self%cabeza
        do while (associated(actual))
            num_img_pequenas = actual%pequena
            num_img_grandes = actual%grande
            if (num_img_pequenas>0) then
                call actual%pila%push_imagen("Pequena")
                num_img_pequenas=num_img_pequenas-1
                actual%pequena = num_img_pequenas
            else if (num_img_grandes>0) then
                call actual%pila%push_imagen("Grande")
                num_img_grandes=num_img_grandes-1
                actual%grande = num_img_grandes
            else if (actual%grande==0 .and. actual%pequena==0) then
                call actual%pila%print_imagen()
                call self%cola_imagen_pequena%push_img_pequena(actual%pila)
                call self%cola_imagen_grande%push_img_grande(actual%pila)
                if (actual%id_cliente=="NULL")then
                    exit
                else
                    call self%lista_clientes_esperando%append_cliente(actual%id_cliente, &
                                                actual%nombre, &
                                                actual%img_pequena, &
                                                actual%img_grande, &
                                                actual%numero_ventanilla, &
                                                cantidad_paso)

                end if
                actual%id_cliente = "NULL"
                actual%nombre = "NULL"
                actual%img_pequena = "0"
                actual%img_grande = "0"
                actual%pequena = 0
                actual%grande = 0
                actual%ocupada = .false.
                call actual%pila%clean_imagen()
            end if 
            actual => actual%siguiente
        end do
        cantidad_paso=cantidad_paso+1
    end subroutine atender_cliente_ventanilla
    
end module modulo_lista_ventanilla