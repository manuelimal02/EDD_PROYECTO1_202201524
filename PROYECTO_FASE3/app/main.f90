program main
    use json_module
    use modulo_arbol_avl
    use modulo_tabla_hash
    !LECTURA JSON
    type(json_file) :: json
    type(json_core) :: jsonc

    !LECTURA SUCURSAL
    type(json_value), pointer :: lista_puntero_s, puntero_s, atributo_puntero_s
    logical :: sucursal_encontrada
    integer :: size_sucursal, contador_surcusal
    character(:), allocatable :: id_s, direccion_s, departamento_s, password_s

    !LECTURA TECNICO
    type(json_value), pointer :: lista_puntero_t, puntero_t, atributo_puntero_t
    logical :: tecnico_encontrado
    integer :: size_tecnico, contador_tecnico
    character(:), allocatable :: dpi_t, nombre_t, apellido_t, genero_t, direccion_t, telefono_t
    
    !LECTURA RUTAS
    type(json_value), pointer :: lista_puntero_r, puntero_r, atributo_puntero_r
    type(json_value), pointer :: puntero_aux, atributo_puntero_aux
    logical :: grafo_encontrado
    integer :: size_grafo, contador_grafo
    integer :: size_ruta, contador_ruta
    character(:), allocatable :: s1, s2, distancia, imp_mantenimiento

    !ESTRUCTURAS
    type(arbol_avl) :: arbol_avl_sucursal  

    !VARIABLES GLOBALES
    integer :: opcion_principal, id_s_int
    character(len=100) :: usuario
    character(len=100) :: contrasena
    character(len=100) :: documento_sucursal, documento_grafo, documento_tecnico

    do
        call mostrar_menu()
        read(*,*) opcion_principal
        select case(opcion_principal)
            case(1)
                call iniciar_sesion()
            case(2)
                exit
            case default
                print *, "OPCION INVALIDA"
        end select
    end do

    contains
    subroutine mostrar_menu()
        print *, "---------------------------------------"
        print *, "Menu Principal - Pixel Print Studio"
        print *, "1. Iniciar Sesion"
        print *, "2. Salir"
        print *, "---------------------------------------"
        print *, "Seleccione El Numero De Opcion:"
        print *, "---------------------------------------"
    end subroutine mostrar_menu

    subroutine iniciar_sesion()
        print *, "---------------------------------------"
        print *, "INICIAR SESION"   
        print *, "---------------------------------------"
        print *, "Ingrese su nombre de usuario:"
        read(*,*) usuario
        print *, "Ingrese su contrasenia:"
        read(*,*) contrasena
        if (usuario == "EDD1S2024" .and. contrasena == "ProyectoFase3") then
            print *, "---------------------------------------"
            print*,"BIENVENIDO ADMINISTRADOR"
            call menu_administrador()
        else if(usuario == "1" .and. contrasena == "1")then
            print *, "---------------------------------------"
            print*,"BIENVENIDO ADMINISTRADOR"
            call menu_administrador()
        else
            print *, "CREDENCIALES INCORRECTAS"
        end if
    end subroutine iniciar_sesion

    subroutine menu_administrador()
        integer :: opcion_admin
        do
            print *, "---------------------------------------"
            print *, "Menu Admin - Pixel Print Studio"
            print *, "1. Carga Masiva Archivos"
            print *, "2. Manejo Sucursales"
            print *, "3. Reportes Graficos"
            print *, "4. Reportes Impresos"
            print *, "5. Cerrar Sesion"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_admin
            select case(opcion_admin)
                case(1)
                    call carga_masiva()
                case(2)
                    call manejo_sucursal()
                case(3)
                    call reportes_graficos()
                case(4)
                    print *, ""
                case(5)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine menu_administrador

    subroutine carga_masiva()
        integer :: opcion_carga
        do
            print *, "---------------------------------------"
            print *, "Menu de Carga Masiva - Pixel Print Studio"
            print *, "1. Sucursales"
            print *, "2. Rutas"
            print *, "3. Regresar Al Menu Principal"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call carga_masiva_sucursal()
                    print*,"Carga De Sucursales Correctamente."
                case(2)
                    print*,"Carga De Rutas Correctamente."
                case(3)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine carga_masiva

    subroutine manejo_sucursal()
        integer :: opcion_carga
        do
            print *, "---------------------------------------"
            print *, "Menu de Manejo Sucursal - Pixel Print Studio"
            print *, "1. Carga De Tecnicos"
            print *, "2. Rutas"
            print *, "3. Regresar Al Menu Principal"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call carga_masiva_tecnico()
                    print*,"Carga De Tecnicos Correctamente."
                case(2)
                    print*,"Carga De Rutas Correctamente."
                case(3)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine manejo_sucursal

    subroutine reportes_graficos()
        integer :: opcion_carga
        do
            print *, "---------------------------------------"
            print *, "Menu de Reportes Graficos - Pixel Print Studio"
            print *, "1. Grafo De Sucursales"
            print *, "2. Rutas"
            print *, "3. Regresar Al Menu Principal"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call arbol_avl_sucursal%graficar_arbol("Arbol_Sucursales")
                case(2)
                    print*,""
                case(3)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine reportes_graficos
!------------------------------------------------------------------------
!CARGA MASIVA SUCURSALES
!------------------------------------------------------------------------
    subroutine carga_masiva_sucursal()
        print *, "---------------------------------------"
        print *, "CARGA MASIVA SUCURSALES"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento de sucursal:"
        print *, "---------------------------------------"
        read(*,*) documento_sucursal
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_sucursal)
        call json%info('',n_children=size_sucursal)
        call json%get_core(jsonc)
        call json%get('', lista_puntero_s, sucursal_encontrada)
        do contador_surcusal = 1, size_sucursal
            call jsonc%get_child(lista_puntero_s, contador_surcusal, puntero_s, sucursal_encontrada)
            call jsonc%get_child(puntero_s, 'id', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, id_s)
            call jsonc%get_child(puntero_s, 'departamento', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, departamento_s)
            call jsonc%get_child(puntero_s, 'direccion', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, direccion_s)
            call jsonc%get_child(puntero_s, 'password', atributo_puntero_s, sucursal_encontrada)
            call jsonc%get(atributo_puntero_s, password_s)
            read(id_s, *) id_s_int
            print *, "Procesando Sucursal: ", id_s
            call arbol_avl_sucursal%insertar_nodo(id_s_int, departamento_s, direccion_s, password_s)
        end do
        call json%destroy()
    end subroutine carga_masiva_sucursal

!------------------------------------------------------------------------
!CARGA MASIVA TECNICOS
!------------------------------------------------------------------------
    subroutine carga_masiva_tecnico()
        print *, "---------------------------------------"
        print *, "CARGA MASIVA TECNICOS"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento de tecnico:"
        print *, "---------------------------------------"
        read(*,*) documento_grafo
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_grafo)
        call json%info('',n_children=size_tecnico)
        call json%get_core(jsonc)
        call json%get('', lista_puntero_t, tecnico_encontrado)
        do contador_tecnico = 1, size_tecnico
            call jsonc%get_child(lista_puntero_t, contador_tecnico, puntero_t, tecnico_encontrado)
            call jsonc%get_child(puntero_t, 'dpi', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, dpi_t)
            call jsonc%get_child(puntero_t, 'nombre', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, nombre_t)
            call jsonc%get_child(puntero_t, 'apellido', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, apellido_t)
            call jsonc%get_child(puntero_t, 'genero', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, genero_t)
            call jsonc%get_child(puntero_t, 'direccion', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, direccion_t)
            call jsonc%get_child(puntero_t, 'telefono', atributo_puntero_t, tecnico_encontrado)
            call jsonc%get(atributo_puntero_t, telefono_t)
            print *, "----------"
            print *, dpi_t
            print *, nombre_t
            print *, apellido_t
            print *, genero_t
            print *, direccion_t
            print *, telefono_t
        end do
        call json%destroy()
end subroutine carga_masiva_tecnico



end program main
