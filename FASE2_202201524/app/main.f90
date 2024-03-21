program main
    use json_module
    use modulo_pixel
    use modulo_arbol_abb
    use modulo_arbol_avl
    implicit none
    !LECTURA JSON
    type(json_file) :: json
    type(json_core) :: jsonc
    !JSON CAPA
    type(json_value), pointer :: listaPunteroCapa, punteroCapa, atributoPunteroCapa, punteroPixel, atributoPixel
    logical :: capa_encontrada
    integer :: size_capa, contador_capa, size_pixel, contador_pixel
    integer ::  fila_int, columna_int, id_capa_int
    character(:), allocatable :: id_capa, fila, columna, color
    !JSON IMAGEN
    type(json_value), pointer :: listaPunteroImagen, punteroImagen, atributoPunteroImagen
    integer :: size_imagen, contador_imagen
    integer :: id_imagen, id_capas
    logical :: imagen_encontrada
    !ESTRUCTURAS
    type(arbol_abb) :: arbol_abb_capa
    type(Tree_t) :: myTree
    !PROGRAMA
    integer :: opcion_principal
    character(len=100) :: usuario
    character(len=100) :: contrasena
    do
        call mostrar_menu()
        read(*,*) opcion_principal
        select case(opcion_principal)
            case(1)
                call iniciar_sesion()
            case(2)
                call registrarse()
            case(3)
                exit
            case default
                print *, "OPCION INVALIDA "
        end select
    end do

contains
    subroutine mostrar_menu()
        print *, "---------------------------------------"
        print *, "Menu Principal - Pixel Print Studio"
        print *, "1. Iniciar Sesion"
        print *, "2. Registrarse"
        print *, "3. Salir"
        print *, "---------------------------------------"
        print *, "Seleccione El Numero De Opcion:"
        print *, "---------------------------------------"
    end subroutine mostrar_menu

    subroutine iniciar_sesion()
        print *, "---------------------------------------"
        print *, "INICIAR SESION"   
        print *, "---------------------------------------"
        print *, "Ingrese nombre de usuario:"
        read(*,*) usuario
        print *, "Ingrese su contrasenia:"
        read(*,*) contrasena
        if (usuario == "admin" .and. contrasena == "EDD2024") then
            print *, "ADMINISTRADOR"
        else if(usuario == "carlos" .and. contrasena == "1234")then
            call menu_cliente()
        else
            print *, "CREDENCIALES INCORRECTAS"
        end if
    end subroutine iniciar_sesion

    subroutine registrarse()
        print *, "REGISTRARSE"
    end subroutine registrarse

    subroutine menu_cliente()
        integer :: opcion_cliente
        do
            print *, "---------------------------------------"
            print *, "Menu Cliente - Pixel Print Studio"
            print *, "1. Visualizar Estructuras"
            print *, "2. Navegacion Y Gestion Imagenes"
            print *, "3. Carga Masiva Informacion"
            print *, "4. Regresar Al Login"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_cliente
            select case(opcion_cliente)
                case(1)
                    call visualizar_estructura()
                case(2)
                    call generador_imagen()
                case(3)
                    call carga_masiva()
                case(4)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine menu_cliente

    subroutine visualizar_estructura()
        integer :: opcion_estructura
        do
            print *, "---------------------------------------"
            print *, "Menu Visualizar Estructuras - Pixel Print Studio"
            print *, "1. Ver Arbol De Imagenes"
            print *, "2. Ver Arbol De Capas"
            print *, "3. Ver Listado De Albumes"
            print *, "4. Ver Capa"
            print *, "5. Regresar Al Menu Cliente"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_estructura
            select case(opcion_estructura)
                case(1)
                    print*,""
                case(2)
                    print*,""
                case(3)
                    print*,""
                case(4)
                    print*,""
                case(5)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine visualizar_estructura

    subroutine generador_imagen()
        integer :: opcion_imagen, numero_nodo, tipo_recorrido
        character(len=:), allocatable :: recorrido1, recorrido2, recorrido3
        do
            print *, "---------------------------------------"
            print *, "Menu Generador Imagenes - Pixel Print Studio"
            print *, "1. Por Recorrido Limitado"
            print *, "2. Por Arbol de Imagenes"
            print *, "3. Por capa"
            print *, "4. Regresar Al Menu Cliente"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_imagen
            select case(opcion_imagen)
                case(1)
                    print *, "---------------------------------------"
                    print *, "RECORRIDO LIMITADO"
                    print *, "---------------------------------------"
                    print *, "Escribe el numero de nodos:"
                    read(*,*) numero_nodo
                    print *, "Escribe el tipo de recorrido:"
                    print *,"1. Preorden"
                    print *,"2. Inorden" 
                    print *,"3. Postorden"
                    !read(*,*) tipo_recorrido
                    !if (tipo_recorrido==1) then
                        call arbol_abb_capa%preorder(numero_nodo, recorrido1)
                        print*, "Preorder: ", recorrido1
                    !else if (tipo_recorrido==2) then
                        call arbol_abb_capa%inorder(numero_nodo, recorrido2)
                        print*, "Inorder: ", recorrido2
                    !else if (tipo_recorrido==3) then
                        call arbol_abb_capa%posorder(numero_nodo, recorrido3)
                        print*, "Posorder: ", recorrido3
                    !end if
                    print *, "---------------------------------------"
                case(2)
                    print*,""
                case(3)
                    print*,""
                case(4)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine generador_imagen

    subroutine carga_masiva()
        integer :: opcion_carga
        do
            print *, "---------------------------------------"
            print *, "Menu de Carga Masiva - Pixel Print Studio"
            print *, "1. Capas"
            print *, "2. Imagenes"
            print *, "3. Albumes"
            print *, "4. Regresar Al Menu Cliente"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_carga
            select case(opcion_carga)
                case(1)
                    call carga_masiva_capa()
                    call arbol_abb_capa%graph("Pruba_Capa")
                    !call arbol_abb_capa%capas_cliente%imprimir()
                case(2)
                    call carga_masiva_imagen()
                case(3)
                    print*,""
                case(4)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine carga_masiva
    !------------------------------------------------------------------------
    !CARGA MASIVA CAPAS
    !------------------------------------------------------------------------
    subroutine carga_masiva_capa()
        type(pixeles), pointer :: pixel_capa
        call json%initialize()
        call json%load(filename='2CAPAS.json')
        call json%info('',n_children=size_capa)
        call json%get_core(jsonc)
        call json%get('', listaPunteroCapa, capa_encontrada)
        do contador_capa = 1, size_capa
            allocate(pixel_capa)
            call jsonc%get_child(listaPunteroCapa, contador_capa, punteroCapa, capa_encontrada)
            call jsonc%get_child(punteroCapa, 'id_capa', atributoPunteroCapa, capa_encontrada)
            call jsonc%get(atributoPunteroCapa, id_capa)
            call jsonc%get_child(punteroCapa, 'pixeles', atributoPunteroCapa, capa_encontrada)
            call jsonc%info(atributoPunteroCapa,n_children=size_pixel)
            read(id_capa, *) id_capa_int
            do contador_pixel = 1, size_pixel
                call jsonc%get_child(atributoPunteroCapa, contador_pixel, punteroPixel, capa_encontrada)
                call jsonc%get_child(punteroPixel, 'fila', atributoPixel, capa_encontrada)
                call jsonc%get(atributoPixel, fila)
                call jsonc%get_child(punteroPixel, 'columna', atributoPixel, capa_encontrada)
                call jsonc%get(atributoPixel, columna)
                call jsonc%get_child(punteroPixel, 'color', atributoPixel, capa_encontrada)
                call jsonc%get(atributoPixel, color)
                read(fila, *) fila_int
                read(columna, *) columna_int
                call pixel_capa%insertar(columna_int, fila_int, color)
            end do
            call arbol_abb_capa%insertar(id_capa_int)
            call arbol_abb_capa%capas_cliente%insertar(id_capa_int, pixel_capa)
        end do
        call json%destroy()
    end subroutine carga_masiva_capa
    !------------------------------------------------------------------------
    !CARGA MASIVA IMAGENES
    !------------------------------------------------------------------------
    subroutine carga_masiva_imagen()
        integer, dimension(:), allocatable :: arreglo_capa
        call json%initialize()
        call json%load(filename='3IMAGEN.json')
        call json%info('',n_children=size_imagen)
        call json%get_core(jsonc)
        call json%get('', listaPunteroImagen, imagen_encontrada)
        do contador_imagen = 1, size_imagen
            call jsonc%get_child(listaPunteroImagen, contador_imagen, punteroImagen, imagen_encontrada)
            call jsonc%get_child(punteroImagen, 'id', atributoPunteroImagen, imagen_encontrada)
            call jsonc%get(atributoPunteroImagen, id_imagen)
            call jsonc%get_child(punteroImagen, 'id_capa', atributoPunteroImagen, imagen_encontrada)
            call jsonc%info(atributoPunteroImagen,n_children=size_capa)
            allocate(arreglo_capa(size_capa))
            do contador_capa = 1, size_capa
                call jsonc%get_child(atributoPunteroImagen, contador_capa, punteroCapa, imagen_encontrada)
                call jsonc%get(punteroCapa, id_capas)
                arreglo_capa(contador_capa) = id_capas
            end do
            call myTree%insert(id_imagen, arreglo_capa)
            deallocate(arreglo_capa)
        end do
        call json%destroy()
    end subroutine carga_masiva_imagen

end program main