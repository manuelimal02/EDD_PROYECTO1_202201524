program PROYECTO_FASE_1
    use :: json_module
    use modulo_cola_cliente
    use modulo_lista_ventanilla
    implicit none
    !LECTURA JSON
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: lista_puntero, puntero, atributo_puntero
    logical :: encontrado
    logical :: hay_ventanilla_disponible
    !VARIABLES LECTURA DESCOLAR
    integer, parameter :: numero_info = 4
    character(len=20) :: info_cliente(numero_info)
    !VARIABLES - USO DEL PROGRAMA
    character(:), allocatable :: id, nombre, img_pequena, img_grande
    character(:), allocatable :: ruta
    integer :: cantidad_cliente_json, contado_1, contado_2, contado_3, contado_4
    integer :: opcion_menu, opcion_menu_parametros, cantidad_ventanilla, contador_ventanilla, pequena, grande
    !COLA DE CLIENTES
    type(cola_cliente) :: cola_cliente_recepcion
    !LISTA DE VENTANILLAS 
    type(lista_ventanilla) :: lista_ventanilla_repecion

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
        print *, "Menu Principal - Pixel Print Studio"
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
        print *, "EJECUTAR PASO"   
        print *, "---------------------------------------"
        call lista_ventanilla_repecion%atender_cliente_ventanilla()
        call lista_ventanilla_repecion%imprimir_imagenes_cliente()
        hay_ventanilla_disponible = lista_ventanilla_repecion%ventanilla_disponible()
        if (hay_ventanilla_disponible) then
            call cola_cliente_recepcion%pop_cliente(info_cliente)
            call lista_ventanilla_repecion%asignar_ventanilla(info_cliente(1), info_cliente(2), info_cliente(3), info_cliente(4))
        end if
        !print *, "****************************************"
        !call cola_cliente_recepcion%print_cliente()
        !print *, "****************************************"
        call lista_ventanilla_repecion%print_ventanilla()
        !print *, "****************************************"
        !call lista_ventanilla_repecion%cola_imagen_pequena%print_img_pequena()
        !print *, "****************************************"
        !call lista_ventanilla_repecion%cola_imagen_grande%print_img_grande()
        !print *, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*"
        print *, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
        call lista_ventanilla_repecion%cola_imagen_pequena%print_img_pequena()
        print *, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
        call lista_ventanilla_repecion%cola_imagen_grande%print_img_grande()
        print *, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
        call lista_ventanilla_repecion%lista_clientes_esperando%print_lista()
        print *, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
        call lista_ventanilla_repecion%lista_clientes_atendido%print_cliente_atendido()
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
        print *, "Carga Masiva Cliente"
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename='config.json')
        call json%info('',n_children=cantidad_cliente_json)
        call json%get_core(jsonc)
        call json%get('', lista_puntero, encontrado)
        do contado_2 = 1, cantidad_cliente_json
            call jsonc%get_child(lista_puntero, contado_2, puntero, encontrado)
            call jsonc%get_child(puntero, 'id', atributo_puntero, encontrado)
            call jsonc%get(atributo_puntero, id)
            call jsonc%get_child(puntero, 'nombre', atributo_puntero, encontrado)
            call jsonc%get(atributo_puntero, nombre)
            call jsonc%get_child(puntero, 'img_p', atributo_puntero, encontrado) 
            call jsonc%get(atributo_puntero, img_pequena)
            call jsonc%get_child(puntero, 'img_g', atributo_puntero, encontrado) 
            call jsonc%get(atributo_puntero, img_grande)
            call cola_cliente_recepcion%push_cliente(trim(id), trim(nombre), trim(img_grande), trim(img_pequena))
        end do
        call json%destroy()
        print *, "Clientes En Cola."
        call cola_cliente_recepcion%print_cliente()
    end subroutine

    subroutine Cantidad_Ventanillas()
        print *, "---------------------------------------"
        print *, "Cantidad De Ventanillas"
        print *, "---------------------------------------"
        print *, "Seleccione El Numero De Ventanilla:"
        read(*,*) cantidad_ventanilla
        print *, "---------------------------------------"
        contador_ventanilla = 1
        do contado_3 = 1, cantidad_ventanilla
            call  lista_ventanilla_repecion%agregar_ventanilla(contador_ventanilla, "NULL", "NULL", "0", "0")
            contador_ventanilla=contador_ventanilla+1
        end do
        call  lista_ventanilla_repecion%print_ventanilla()
    end subroutine

end program PROYECTO_FASE_1