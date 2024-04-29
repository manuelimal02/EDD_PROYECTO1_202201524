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
    integer(8) ::  dpi_t_int, telefono_t_int
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
                    call carga_masiva_ruta()
                    print*,"Carga De Rutas Correctamente."
                case(3)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine carga_masiva

    subroutine manejo_sucursal()
        integer :: opcion_carga, id_sucursal
        character(len=100) :: contrasena
        logical :: existe_matriz
        print *, "---------------------------------------"
        print *, "CREDENCIALES SUCURSALES"
        print *, "---------------------------------------"
        print *, "Escribe el ID de la sucursal:"
        read(*,*) id_sucursal
        print *, "Escribe la contrasena de la sucursal:"
        read(*,*) contrasena
        existe_matriz = arbol_avl_sucursal%valor_existe(id_sucursal, contrasena)
        if(existe_matriz)then
            do
                print *, "---------------------------------------"
                print *, "Bienvenido Sucursal: ", id_sucursal
                print *, "---------------------------------------"
                print *, "Menu de Manejo Sucursal - Pixel Print Studio"
                print *, "1. Carga De Tecnicos"
                print *, "2. Generar Recorrido Mas Optimo"
                print *, "3. Informacion Tecnico En Especifico"
                print *, "4. Listar Tecnicos"
                print *, "5. Regresar Al Menu Principal"
                print *, "---------------------------------------"
                print *, "Seleccione El Numero De Opcion:"
                print *, "---------------------------------------"
                read(*,*) opcion_carga
                select case(opcion_carga)
                    case(1)
                        call carga_masiva_tecnico(id_sucursal, contrasena)
                    case(2)
                        print*,"Recorrido Más Optimo"
                    case(3)
                        call informacion_tecnico_especifico(id_sucursal, contrasena)
                    case(4)
                        call listar_informacion_tecnico(id_sucursal, contrasena)
                    case(5)
                        exit
                    case default
                        print *, "OPCION INVALIDA"
                end select
            end do
        else 
            print*, "Credenciales De Sucursal Incorrectas."
        end if
    end subroutine manejo_sucursal

    subroutine reportes_graficos()
        integer :: opcion_carga, id_sucursal
        character(len=100) :: contrasena
        logical :: existe_matriz
        do
            print *, "---------------------------------------"
            print *, "Menu de Reportes Graficos - Pixel Print Studio"
            print *, "1. Arbol De Sucursales"
            print *, "2. Tabla Hash Tecnico"
            print *, "3. Regresar Al Menu Principal"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call arbol_avl_sucursal%graficar_arbol("Arbol_Sucursales")
                case(2)
                    print *, "---------------------------------------"
                    print *, "CREDENCIALES SUCURSALES"
                    print *, "---------------------------------------"
                    print *, "Escribe el ID de la sucursal:"
                    read(*,*) id_sucursal
                    print *, "Escribe la contrasena de la sucursal:"
                    read(*,*) contrasena
                    existe_matriz = arbol_avl_sucursal%valor_existe(id_sucursal, contrasena)
                    if(existe_matriz)then
                        call grafica_tabla_hash(id_sucursal, contrasena)
                    else
                        print*, "Credenciales De Sucursal Incorrectas."
                    end if
                case(3)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine reportes_graficos

    subroutine informacion_tecnico_especifico(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer(8) :: dpi_int
        integer, intent(in) :: id_sucursal
        character(len=*), intent(in) :: contrasena
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
        print *, "---------------------------------------"
        print *, "INFORMACION TECINCO ESPECIFICO"
        print *, "---------------------------------------"
        print *, "Ingrese el DPI del tecnico:"
        print *, "---------------------------------------"
        read(*,*) dpi_int
        call sucursal_actual%tabla%imprimir(dpi_int)
    end subroutine informacion_tecnico_especifico

    subroutine listar_informacion_tecnico(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer, intent(in) :: id_sucursal
        character(len=*), intent(in) :: contrasena
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
        print *, "---------------------------------------"
        print *, "LISTAR INFORMACION TECNICOS"
        print *, "---------------------------------------"
        call sucursal_actual%tabla%listar_tecnico()
    end subroutine listar_informacion_tecnico

    subroutine grafica_tabla_hash(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer, intent(in) :: id_sucursal
        character(len=*), intent(in) :: contrasena
        character(len=32) :: id_sucursal_str
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
        write(id_sucursal_str, '(I0)') id_sucursal
        print *, "---------------------------------------"
        print *, "GRAFICA TABLA HASH TECNICO"
        print *, "---------------------------------------"
        call sucursal_actual%tabla%grafica_tabla("Sucursal_"//id_sucursal_str)
    end subroutine grafica_tabla_hash
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
    subroutine carga_masiva_tecnico(id_sucursal, contrasena)
        type(nodo_avl), pointer :: sucursal_actual
        integer, intent(in) :: id_sucursal
        integer :: i
        character(len=*), intent(in) :: contrasena
        sucursal_actual => arbol_avl_sucursal%obtener_nodo(id_sucursal, contrasena)
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
            print *, "Procesando Tecnico: ", dpi_t
            read(dpi_t, *) dpi_t_int
            read(telefono_t, *) telefono_t_int
            call sucursal_actual%tabla%insertar(dpi_t_int, nombre_t, apellido_t, direccion_t, telefono_t_int, genero_t)
        end do
        print*,"Tecnicos Cargados Correctamente. Sucursal: ", id_sucursal
        call json%destroy()
    end subroutine carga_masiva_tecnico
!------------------------------------------------------------------------
!CARGA MASIVA RUTAS
!------------------------------------------------------------------------
    subroutine carga_masiva_ruta()
        print *, "---------------------------------------"
        print *, "CARGA MASIVA RUTAS"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento de ruta:"
        print *, "---------------------------------------"
        read(*,*) documento_grafo
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_grafo)
        call json%info('',n_children=size_grafo)
        call json%get_core(jsonc)
        call json%get('', lista_puntero_r, grafo_encontrado)
        do contador_grafo = 1, size_grafo
            call jsonc%get_child(lista_puntero_r, contador_grafo, puntero_r, grafo_encontrado)
            call jsonc%get_child(puntero_r, 'grafo', atributo_puntero_r, grafo_encontrado)
            call jsonc%info(atributo_puntero_r, n_children=size_ruta)
            do contador_ruta = 1, size_ruta
                call jsonc%get_child(atributo_puntero_r, contador_ruta, puntero_aux, grafo_encontrado)
                call jsonc%get_child(puntero_aux, 's1', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, s1)
                call jsonc%get_child(puntero_aux, 's2', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, s2)
                call jsonc%get_child(puntero_aux, 'distancia', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, distancia)
                call jsonc%get_child(puntero_aux, 'imp_mantenimiento', atributo_puntero_aux, grafo_encontrado)
                call jsonc%get(atributo_puntero_aux, imp_mantenimiento)
                print *, "--------------"
                print *, "s1: ", s1
                print *, "s1: ", s2
                print *, "distancia: ", distancia
                print *, "imp_mantenimiento: ", imp_mantenimiento
            end do
        end do
        call json%destroy()
    end subroutine carga_masiva_ruta

end program main