program PROYECTO_FASE_1
    use :: json_module

    implicit none
    integer :: opcion_menu, opcion_menu_parametros
    integer :: i, size, id, img_p, img_g 
    logical :: found
    type(json_file) :: json   ! Se declara una variable del tipo json_file
    type(json_value), pointer :: listPointer, personPointer, attributePointer  ! Se declaran punteros a variables del tipo json_value
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombre  ! Se declara una cadena de caracteres que se asignará dinámicamente

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
    end subroutine

    subroutine Cantidad_Ventanillas()
        print *, "---------------------------------------"
        print *, "-- Cantidad Ventanillas --"
    end subroutine

end program PROYECTO_FASE_1