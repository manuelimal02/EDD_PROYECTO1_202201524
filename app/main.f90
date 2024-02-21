program PROYECTO_FASE_1
    use :: json_module
    use cola_module
    implicit none
    !CLIENTE
    type :: Cliente
        character(len=:), allocatable :: id
        character(len=:), allocatable :: nombre
        character(len=:), allocatable :: img_p
        character(len=:), allocatable :: img_g
    end type Cliente
    type(Cliente) :: mi_cliente
    !LECTURA JSON
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, animalPointer, attributePointer
    logical :: found
    integer :: size, i
    real(8) :: random_value
    integer :: it, n, random_integer
    character(:), allocatable :: id, nombre, img_peque, img_grande

    !VARIABLES PARA EL USO DEL PROGRAMA
    integer :: opcion_menu, opcion_menu_parametros

    !COLA DE CLIENTES
    type(cola) :: cola_recepcion

    do
        call Mostrar_Menu()
        read(*,*) opcion_menu
        select case(opcion_menu)
            case(1)
                call OPCION_1()
            case(2)
                call OPCION_2()
            case(3)
                call OPCION_3()
            case(4)
                call OPCION_4()
            case(5)
                call OPCION_5()
            case(6)
                print *, "---------------------------------------"
                print *, "Gracias Por Utilizar El Programa"
                exit
            case default
                print *, "Opcion No Valida. Intentelo De Nuevo."
        end select
    end do

    contains
    subroutine Mostrar_Menu()
        print *, "---------------------------------------"
        print *, "Menu Principal"
        print *, "1. Parametros Iniciales"
        print *, "2. Ejecutar Paso"
        print *, "3. Estado En Memoria De Las Estructuras"
        print *, "4. Reportes"
        print *, "5. Acerca De"
        print *, "6. Salir"
        print *, "---------------------------------------"
        print *, "Seleccione El Numero De Opcion:"
        print *, "---------------------------------------"
    end subroutine

    subroutine OPCION_1()
        print *, "---------------------------------------"
        print *, "PARAMETROS INICIALES"
        print *, "1. Carga Masiva De Clientes"
        print *, "2. Cantidad De Ventanillas "
        read(*,*) opcion_menu_parametros
        select case(opcion_menu_parametros)
            case(1)
                call Carga_Masiva_Clientes()
            case(2)
                call Cantidad_Ventanillas()

        end select
    end subroutine

    subroutine OPCION_2()
        print *, "---------------------------------------"
        print *, "-- Ejecutar Paso --"
    end subroutine

    subroutine OPCION_3()
        print *, "---------------------------------------"
        print *, "-- Estado En Memoria De Las Estructuras --"
    end subroutine

    subroutine OPCION_4()
      print *, "---------------------------------------"
      print *, "-- Reportes --"
    end subroutine

    subroutine OPCION_5()
        print *, "---------------------------------------"
        print *, "Acerca De:"
        Print *, "Carlos Manuel Lima y Lima"
        Print *, "202201524"
        Print *, "Estructura De Datos"
        Print *, "Primer Semestre 2024"
    end subroutine

    subroutine Carga_Masiva_Clientes()
        print *, "---------------------------------------"
        print *, "-- Carga Masiva Cliente --"
        call json%initialize()
        call json%load(filename='config.json')
        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('', listPointer, found)
    
        do i = 1, size
            call jsonc%get_child(listPointer, i, animalPointer, found)

            call jsonc%get_child(animalPointer, 'id', attributePointer, found)
            call jsonc%get(attributePointer, id)
    
            call jsonc%get_child(animalPointer, 'nombre', attributePointer, found)
            call jsonc%get(attributePointer, nombre)
    
            call jsonc%get_child(animalPointer, 'img_p', attributePointer, found) 
            call jsonc%get(attributePointer, img_peque)

            call jsonc%get_child(animalPointer, 'img_g', attributePointer, found) 
            call jsonc%get(attributePointer, img_grande)

            mi_cliente%id = id
            mi_cliente%nombre = nombre
            mi_cliente%img_p = img_peque
            mi_cliente%img_g = img_grande

            print *, "----"
            print *, 'ID: ', mi_cliente%id
            print *, 'Nombre: ', mi_cliente%nombre
            print *, 'img_p: ', mi_cliente%img_p
            print *, 'img_g: ', mi_cliente%img_g

            call cola_recepcion%append(1)

        end do
        call json%destroy()
        call cola_recepcion%print()
    end subroutine

    subroutine Cantidad_Ventanillas()
        print *, "---------------------------------------"
        print *, "-- Cantidad Ventanillas --"
        call random_seed()


    ! Generar y mostrar los números aleatorios enteros
    do i = 1, 1
        call random_number(random_value)
        random_integer = mod(int(random_value * 5.0 + 0.5), 5)  ! Escalar y redondear
        print *, 'Numero aleatorio ', i, ': ', random_integer
    end do
    end subroutine

end program PROYECTO_FASE_1