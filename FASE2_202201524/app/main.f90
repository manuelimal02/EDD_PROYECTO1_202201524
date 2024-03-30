program main
    use json_module
    use modulo_split
    use global_variable
    use modulo_lista_cliente
    use modulo_lista_imagen
    use md
    use modulo_arbol_abb_c
    use modulo_arbol_abb_s
    use modulo_arbol_avl_c
    use modulo_matrix_dispersa
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
    !JSON ALBUMES
    type(json_value), pointer :: listaPunteroAlbum, punteroAlbum, atributoPunteroAlbum
    character(:), allocatable :: nombre_album,imgs
    integer :: size_album, contador_album
    integer :: imgs_size, contador_a
    logical :: album_encontrado
    !JSON CLIENTE
    type(json_value), pointer :: listaPunteroCliente, punteroCiente, atributoPunteroCliente
    integer :: size_cliente, contador_cliente
    character(:), allocatable :: dpi_cliente, nombre_cliente, contrasena_cliente
    logical :: cliente_encontrado
    !ESTRUCTURAS
    type(lista_cliente) :: lista_simple_cliente  
    !PROGRAMA
    integer :: opcion_principal
    character(len=100) :: usuario
    character(len=100) :: contrasena
    character(len=100) :: dpi_cliente1, nombre_cliente1, contrasena_cliente1
    character(len=100) :: documento_capa, documento_imagen, documento_album, documento_cliente
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
            print *, "---------------------------------------"
            print*,"BIENVENIDO ADMINISTRADOR"
            call menu_administrador()
        else if(lista_simple_cliente%iniciar_sesion_c(usuario, contrasena))then
            dpi_global = usuario
            print *, "---------------------------------------"
            print*,"BIENVENIDO CLIENTE: ", dpi_global
            call menu_cliente()
        else
            print *, "CREDENCIALES INCORRECTAS"
        end if
    end subroutine iniciar_sesion

    subroutine registrarse()
        character(len=100) :: linea
        print *, "---------------------------------------"
        print *, "REGISTRARSE"  
        print *, "---------------------------------------"
        print*,"Ingrese el numero de DPI: "
        read(*,'(a)') linea
        dpi_cliente1 = trim(linea)
        print*,"Ingrese su nombre y apellido: "
        read(*,'(a)') linea
        nombre_cliente1 = trim(linea)
        print*,"Ingrese su contrasena: "
        read(*,'(a)') linea
        contrasena_cliente1 = trim(linea)
        call lista_simple_cliente%insertar_cliente(dpi_cliente1, nombre_cliente1, contrasena_cliente1)
    end subroutine registrarse

    subroutine menu_administrador()
        integer :: opcion_admin
        do
            print *, "---------------------------------------"
            print *, "Menu Admin - Pixel Print Studio"
            print *, "1. Visualizar Arbol Cliente"
            print *, "2. Manejo De Clientes"
            print *, "3. Carga Masiva Cliente"
            print *, "4. Reportes"
            print *, "5. Regresar Al Login"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_admin
            select case(opcion_admin)
                case(1)
                    call lista_simple_cliente%grafica_cliente("Lista_Cliente")
                case(2)
                    call abc_cliente()
                case(3)
                    call carga_masiva_cliente()
                case(4)
                    call reportes_administrador()
                case(5)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine menu_administrador

    subroutine abc_cliente()
        integer :: opcion_cliente
        character(len=100) :: linea
        do
            print *, "---------------------------------------"
            print *, "Menu Manejo Cliente - Pixel Print Studio"
            print *, "1. Nuevo Cliente"
            print *, "2. Eliminar Cliente"
            print *, "3. Modificar Cliente"
            print *, "4. Regresar Al Menu Administrador"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_cliente
            select case(opcion_cliente)
                case(1)
                    print *, "---------------------------------------"
                    print *, "NUEVO CLIENTE"  
                    print *, "---------------------------------------"
                    print*,"Ingrese el numero de DPI: "
                    read(*,'(a)') linea
                    dpi_cliente1 = trim(linea)
                    print*,"Ingrese El Nombre y Apellido: "
                    read(*,'(a)') linea
                    nombre_cliente1 = trim(linea)
                    print*,"Ingrese La Contrasena: "
                    read(*,'(a)') linea
                    contrasena_cliente1 = trim(linea)
                    call lista_simple_cliente%insertar_cliente(dpi_cliente1, nombre_cliente1, contrasena_cliente1)
                case(2)
                    print *, "---------------------------------------"
                    print *, "ELIMINAR CLIENTE"  
                    print *, "---------------------------------------"
                    print*,"Ingrese el numero de DPI: "
                    read(*,*) dpi_cliente1
                    call lista_simple_cliente%eliminar_cliente(dpi_cliente1)
                case(3)
                    print *, "---------------------------------------"
                    print *, "MODIFICAR CLIENTE"  
                    print *, "---------------------------------------"
                    print*,"Ingrese El Numero De DPI: "
                    read(*,'(a)') linea
                    dpi_cliente1 = trim(linea)
                    print*,"Ingrese El Nuevo Nombre Y Apellido: "
                    read(*,'(a)') linea
                    nombre_cliente1 = trim(linea)
                    print*,"Ingrese La Nueva Contrasena: "
                    read(*,'(a)') linea
                    contrasena_cliente1 = trim(linea)
                    call lista_simple_cliente%modificar_cliente(dpi_cliente1, nombre_cliente1, contrasena_cliente1)
                case(4)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine abc_cliente

    subroutine reportes_administrador()
        integer :: opcion_reporte_a
        character(len=20) :: dpi_cliente_reporte
        do
            print *, "---------------------------------------"
            print *, "Menu De Reportes Admin - Pixel Print Studio"
            print *, "1. Mostrar Informacion: Nombre, DPI Y Password."
            print *, "2. Cantidad De Albumes Y Sus Imagenes."
            print *, "3. Cantidad De Imagenes Totales."
            print *, "4. Cantidad De Capas Totales"
            print *, "5. Listar Clientes"
            print *, "6. Regresar Al Menu Administrador"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_reporte_a
            select case(opcion_reporte_a)
                case(1)
                    print *, "---------------------------------------"
                    print *, "MOSTRAR INFORMACION CLIENTE"
                    print *, "---------------------------------------"
                    print *, "Ingrese el DPI del cliente:"
                    print *, "---------------------------------------"
                    read(*,*) dpi_cliente_reporte
                    call lista_simple_cliente%mostrar_cliente(dpi_cliente_reporte)
                case(2)
                    print *, "---------------------------------------"
                    print *, "CANTIDAD DE ALBUMES Y SUS IMAGENES"
                    print *, "---------------------------------------"
                    print *, "Ingrese el DPI del cliente:"
                    print *, "---------------------------------------"
                    read(*,*) dpi_cliente_reporte
                    call lista_simple_cliente%reporte_albumes_cliente(dpi_cliente_reporte)
                case(3)
                    print *, "---------------------------------------"
                    print *, "CANTIDAD DE IMAGENES TOTALES"
                    print *, "---------------------------------------"
                    print *, "Ingrese el DPI del cliente:"
                    print *, "---------------------------------------"
                    read(*,*) dpi_cliente_reporte
                    call lista_simple_cliente%reporte_imagenes_cliente(dpi_cliente_reporte)
                case(4)
                    print *, "---------------------------------------"
                    print *, "CANTIDAD DE CAPAS TOTALES"
                    print *, "---------------------------------------"
                    print *, "Ingrese el DPI del cliente:"
                    print *, "---------------------------------------"
                    read(*,*) dpi_cliente_reporte
                    call lista_simple_cliente%reporte_capas_cliente(dpi_cliente_reporte)
                case(5)
                    print *, "---------------------------------------"
                    print *, "LISTAR CLIENTES"
                    call lista_simple_cliente%reporte_listar_cliente()
                case(6)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine reportes_administrador

    subroutine menu_cliente()
        integer :: opcion_cliente, numero_imagen
        logical :: existe_imagen
        do
            print *, "---------------------------------------"
            print *, "Menu Cliente - Pixel Print Studio"
            print *, "1. Visualizar Estructuras"
            print *, "2. Navegacion Y Gestion Imagenes"
            print *, "3. Carga Masiva Informacion"
            print *, "4. Reportes"
            print *, "5. Manejo De Imagenes"
            print *, "6. Regresar Al Login"
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
                    call reportes_usuario()
                case(5)
                    call abc_imagen()
                case(6)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine menu_cliente

    subroutine visualizar_estructura()
        type(nodo_cliente), pointer :: cliente_actual
        integer :: opcion_estructura, numero_capa, numero_imagen
        logical :: existe_matriz, existe_imagen
        type(matriz), pointer :: matriz_auxiliar
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "---------------------------------------"
            print *, "Menu Visualizar Estructuras - Pixel Print Studio"
            print *, "1. Ver Arbol De Capas"
            print *, "2. Ver Arbol De Imagenes"
            print *, "3. Ver Listado De Albumes"
            print *, "4. Ver Capa"
            print *, "5. Ver Imagen y Arbol De Capas"
            print *, "6. Regresar Al Menu Cliente"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_estructura
            select case(opcion_estructura)
                case(1)
                    call cliente_actual%arbol_abb_capa%graficar_arbol("Arbol_Capa_Abb_"//dpi_global)
                case(2)
                    call cliente_actual%arbol_avl_imagen%graficar_arbol("Arbol_Imagen_Avl_"//dpi_global)
                case(3)
                    call cliente_actual%lista_doble_album%graficar_album("Lista_Albumes_"//dpi_global)
                case(4)
                    print *, "---------------------------------------"
                    print *, "Ver Capa"
                    print *, "---------------------------------------"
                    print *, "Escribe el ID de capa a vizualizar:"
                    read(*,*) numero_capa
                    existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(numero_capa)
                    if (existe_matriz) then
                        allocate(matriz_auxiliar)
                        matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(numero_capa)
                        call matriz_auxiliar%graficar_matriz("Ver_Capa_"//dpi_global)
                        deallocate(matriz_auxiliar)
                    else
                        print*, "Capa No Existe: ", int_to_str(numero_capa)
                    end if
                case(5)
                    print *, "---------------------------------------"
                    print *, "Ver Imagen y Arbol De Capas"
                    print *, "---------------------------------------"
                    print *, "Escribe el ID de imagen a vizualizar:"
                    read(*,*) numero_imagen
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(numero_imagen)
                    if(existe_imagen)then
                        call cliente_actual%arbol_avl_imagen%graficar_arbol_imagen("Arbol_Imagen_"//dpi_global,numero_imagen)
                    else
                        print*, "Imagen No Existe: ", int_to_str(numero_imagen)
                    end if
                case(6)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine visualizar_estructura

    subroutine generador_imagen()
        type(nodo_cliente), pointer :: cliente_actual
        type(matriz), pointer :: matriz_imagen, matriz_auxiliar
        type(arbol_abb_simple), pointer :: arbol_abb_simple
        integer :: opcion_imagen, numero_nodo, tipo_recorrido, contador, id_capa, id_imagen, cantidad_capa,numero_capa
        logical :: existe_imagen, existe_matriz
        character(len=:), allocatable :: recorrido
        character(len=20), dimension(:), allocatable :: nodo
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "---------------------------------------"
            print *, "Menu Generador Imagenes - Pixel Print Studio"
            print *, "1. Por Recorrido Limitado"
            print *, "2. Por Arbol de Imagenes"
            print *, "3. Por Capas"
            print *, "4. Regresar Al Menu Cliente"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_imagen
            select case(opcion_imagen)
                !---------------------------------------------------------------------------!
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
                    read(*,*) tipo_recorrido
                    !------------------------------------------!
                    if (tipo_recorrido==1) then
                        call cliente_actual%arbol_abb_capa%recorrido_preorden(numero_nodo, recorrido)
                        if(recorrido=="")then
                            print*, "No Existen Capas Cargadas."
                            exit
                        end if
                        allocate(matriz_imagen)
                        print*, "Preorder: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            allocate(matriz_auxiliar)
                            print *, trim(nodo(contador))
                            read(nodo(contador), *) id_capa
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                        end do
                        call matriz_imagen%graficar_matriz("I_Recorrido_Preorden_"//dpi_global)
                        deallocate(matriz_imagen)
                    !------------------------------------------!
                    else if (tipo_recorrido==2) then
                        call cliente_actual%arbol_abb_capa%recorrido_inorden(numero_nodo, recorrido)
                        if(recorrido=="")then
                            print*, "No Existen Capas Cargadas."
                            exit
                        end if
                        allocate(matriz_imagen)
                        print*, "Inorder: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            allocate(matriz_auxiliar)
                            print *, trim(nodo(contador))
                            read(nodo(contador), *) id_capa
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                        end do
                        call matriz_imagen%graficar_matriz("I_Recorrido_Inorden_"//dpi_global)
                        deallocate(matriz_imagen)
                    !------------------------------------------!
                    else if (tipo_recorrido==3) then
                        call cliente_actual%arbol_abb_capa%recorrido_postorden(numero_nodo, recorrido)
                        if(recorrido=="")then
                            print*, "No Existen Capas Cargadas."
                            exit
                        end if
                        allocate(matriz_imagen)
                        print*, "Postorden: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            allocate(matriz_auxiliar)
                            print *, trim(nodo(contador))
                            read(nodo(contador), *) id_capa
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                        end do
                        call matriz_imagen%graficar_matriz("I_Recorrido_Postorden_"//dpi_global)
                        deallocate(matriz_imagen)
                    end if
                    print *, "---------------------------------------"
                !---------------------------------------------------------------------------!
                case(2)
                    print *, "---------------------------------------"
                    print *, "ARBOL DE IMAGENES"
                    print *, "---------------------------------------"
                    print *, "Escribe el ID de la imagen:"
                    read(*,*) id_imagen
                    allocate(arbol_abb_simple)
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(id_imagen)
                    if (existe_imagen) then
                        arbol_abb_simple = cliente_actual%arbol_avl_imagen%buscar_valor(id_imagen)
                        call arbol_abb_simple%recorrido_amplitud(recorrido)
                        allocate(matriz_imagen)
                        print*, "Amplitud: "
                        call split(recorrido, '-', nodo)
                        do contador = 1, size(nodo)
                            print *, trim(nodo(contador))
                        end do
                        print*,"----"
                        do contador = 1, size(nodo)
                            read(nodo(contador), *) id_capa
                            existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(id_capa)
                            if (existe_matriz) then
                                allocate(matriz_auxiliar)
                                matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(id_capa)
                                call matriz_imagen%insertar_matriz(matriz_auxiliar)
                                deallocate(matriz_auxiliar)
                                print*, "Capa: ",  trim(nodo(contador)), " Apilada."
                            else
                                print*, "Capa: ",  trim(nodo(contador)), " No Existe."
                            end if
                        end do
                        print*,"----"
                        call matriz_imagen%graficar_matriz("I_Recorrido_Amplitud_"//dpi_global)
                        deallocate(matriz_imagen)
                    else
                        print *, "Imagen No Existe: ", int_to_str(id_imagen)
                    end if
                    deallocate(arbol_abb_simple)
                    
                !---------------------------------------------------------------------------!
                case(3)
                    print *, "---------------------------------------"
                    print *, "POR CAPAS"
                    print *, "---------------------------------------"
                    print *, "Escribe el numero de capas a utilizar:"
                    read(*,*) cantidad_capa
                    allocate(matriz_imagen)
                    do contador = 1, cantidad_capa
                        print*,"----"
                        print *, "Ingrese El ID Capa Numero ", int_to_str(contador)
                        read(*,*) numero_capa
                        existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(numero_capa)
                        if (existe_matriz) then
                            allocate(matriz_auxiliar)
                            matriz_auxiliar = cliente_actual%arbol_abb_capa%buscar_matriz(numero_capa)
                            call matriz_imagen%insertar_matriz(matriz_auxiliar)
                            deallocate(matriz_auxiliar)
                            print*, "Capa Apilada: ", int_to_str(numero_capa)
                        else
                            print*, "Capa No Existe: ", int_to_str(numero_capa)
                        end if
                    end do
                    print*,"--------"
                    call matriz_imagen%graficar_matriz("Imagen_Por_Capa_"//dpi_global)
                    deallocate(matriz_imagen)
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
                    print*,"Carga De Capas Correctamente."
                case(2)
                    call carga_masiva_imagen()
                    print*,"Carga De Imagenes Correctamente."
                case(3)
                    call carga_masiva_album()
                    print*,"Carga De Albumes Correctamente."
                case(4)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine carga_masiva

    subroutine reportes_usuario()
        type(nodo_cliente), pointer :: cliente_actual
        integer :: opcion_reporte, numero_nodo, contador
        character(len=:), allocatable :: recorrido
        character(len=20), dimension(:), allocatable :: nodo
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "---------------------------------------"
            print *, "Menu de Carga Masiva - Pixel Print Studio"
            print *, "1. Top 5 Imagenes Con Mas Numero De Capas"
            print *, "2. Todas Las Capas Que Son Hojas"
            print *, "3. Profundidad De Arbol De Capas"
            print *, "4. Listar Las Capas: Preorden, Inorden, Postorden"
            print *, "5. Regresar Al Menu Cliente"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_reporte
            select case(opcion_reporte)
                case(1)
                    print *, "---------------------------------------"
                    print *, "TOP5 IMAGENES CON MAYOR NO. DE CAPAS"
                    print *, "---------------------------------------"
                    call cliente_actual%arbol_avl_imagen%top_5_imagenes()
                case(2)
                    print *, "---------------------------------------"
                    print *, "CAPAS QUE SON HOJAS"
                    print *, "---------------------------------------"
                    call cliente_actual%arbol_abb_capa%imprimir_hoja()
                case(3)
                    print *, "---------------------------------------"
                    print *, "PROFUNDIDAD DEL ARBOL"
                    print *, "---------------------------------------"
                    call cliente_actual%arbol_abb_capa%profundidad_arbol()
                case(4)
                    print *, "---------------------------------------"
                    print *, "LISTAR LAS CAPAS"
                    print *, "---------------------------------------"
                    call cliente_actual%arbol_abb_capa%recorrido_preorden(numero_nodo, recorrido)
                    print*, "Recorrido Preorden:"
                    call split(recorrido, '-', nodo)
                    do contador = 1, size(nodo)
                        print *, trim(nodo(contador))
                    end do
                    print *, "---------------------------"
                    call cliente_actual%arbol_abb_capa%recorrido_inorden(numero_nodo, recorrido)
                    print*, "Recorrido Inorder:"
                    call split(recorrido, '-', nodo)
                    do contador = 1, size(nodo)
                        print *, trim(nodo(contador))
                    end do
                    print *, "---------------------------"
                    call cliente_actual%arbol_abb_capa%recorrido_postorden(numero_nodo, recorrido)
                    print*, "Recorrido Postorden:"
                    call split(recorrido, '-', nodo)
                    do contador = 1, size(nodo)
                        print *, trim(nodo(contador))
                    end do
                case(5)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine reportes_usuario

    subroutine abc_imagen()
        type(nodo_cliente), pointer :: cliente_actual
        type(arbol_abb_simple), pointer :: arbol_abb_capa_simple
        integer :: opcion_abc, numero_imagen, numero_capa, cantidad_capa, contador
        logical :: existe_matriz, existe_imagen
        character(len=20) :: imagen
        do
            cliente_actual => lista_simple_cliente%obtener_cliente(dpi_global)
            print *, "---------------------------------------"
            print *, "Menu ABC Imagen - Pixel Print Studio"
            print *, "1. Registrar Imagen"
            print *, "2. Eliminar Imagen"
            print *, "3. Regresar Al Menu Cliente"
            print *, "---------------------------------------"
            print *, "Seleccione El Numero De Opcion:"
            print *, "---------------------------------------"
            read(*,*) opcion_abc
            select case(opcion_abc)
                case(1)
                    print *, "---------------------------------------"
                    print *, "REGISTRAR UNA IMAGEN"
                    print *, "---------------------------------------"
                    print *, "Escribe el ID de imagen a registrar:"
                    read(*,*) numero_imagen
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(numero_imagen)
                    if(existe_imagen)then
                        print*, "Imagen Ya Existe En El Sistema: ", int_to_str(numero_imagen)
                        exit
                    else
                        allocate(arbol_abb_capa_simple)
                        print *, "---------------------------------------"
                        print *, "Escribe el numero de capas a Ingresar:"
                        read(*,*) cantidad_capa
                        do contador = 1, cantidad_capa
                            print*,"----"
                            print *, "Ingrese El ID Capa Numero ", int_to_str(contador)
                            read(*,*) numero_capa
                            existe_matriz = cliente_actual%arbol_abb_capa%valor_existe(numero_capa)
                            if (existe_matriz) then
                                call arbol_abb_capa_simple%insertar(numero_capa)
                                print*, "Capa Insertada: ", int_to_str(numero_capa)
                            else
                                print*, "Capa No Existe: ", int_to_str(numero_capa)
                            end if
                        end do
                        call cliente_actual%arbol_avl_imagen%insertar_nodo(numero_imagen, arbol_abb_capa_simple)
                        deallocate(arbol_abb_capa_simple)
                        print*,"Imagen: ",numero_imagen," Registrada Correctamente."
                    end if
                case(2)
                    print *, "---------------------------------------"
                    print *, "ELIMINAR UNA IMAGEN"
                    print *, "---------------------------------------"
                    print *, "Escribe el ID de imagen a eliminar:"
                    read(*,*) numero_imagen
                    existe_imagen = cliente_actual%arbol_avl_imagen%valor_existe(numero_imagen)
                    if(existe_imagen)then
                        write(imagen, '(I0)') numero_imagen
                        call cliente_actual%arbol_avl_imagen%eliminar_nodo(numero_imagen)
                        call cliente_actual%lista_doble_album%eliminar_imagen(imagen)
                    else
                        print*, "Imagen No Existe: ", int_to_str(numero_imagen)
                    end if
                case(3)
                    exit
                case default
                    print *, "OPCION INVALIDA"
            end select
        end do
    end subroutine abc_imagen
    !------------------------------------------------------------------------
    !CARGA MASIVA CAPAS
    !------------------------------------------------------------------------
    subroutine carga_masiva_capa()
        character(len=30) :: colorD
        type(matriz), pointer :: matriz_dispersa_capa
        type(nodo_cliente), pointer :: cliente_actual
        print *, "---------------------------------------"
        print *, "CARGA MASIVA CAPAS"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento de capa:"
        print *, "---------------------------------------"
        read(*,*) documento_capa
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_capa)
        call json%info('',n_children=size_capa)
        call json%get_core(jsonc)
        call json%get('', listaPunteroCapa, capa_encontrada)
        do contador_capa = 1, size_capa
            allocate(matriz_dispersa_capa)
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
                colorD=color
                call matriz_dispersa_capa%insertar_nodo(columna_int, fila_int, colorD)
            end do
            !----------
            cliente_actual => lista_simple_cliente%cabeza
            do while (associated(cliente_actual))
                if (trim(cliente_actual%dpi) == dpi_global) then
                    call cliente_actual%arbol_abb_capa%insertar_nodo(id_capa_int, matriz_dispersa_capa)
                    exit
                end if
                cliente_actual => cliente_actual%siguiente
            end do
            !----------
            !call arbol_abb_capa%insertar_nodo(id_capa_int, matriz_dispersa_capa)
            deallocate(matriz_dispersa_capa)
        end do
        call json%destroy()
    end subroutine carga_masiva_capa
    !------------------------------------------------------------------------
    !CARGA MASIVA IMAGENES
    !------------------------------------------------------------------------
    subroutine carga_masiva_imagen()
        type(arbol_abb_simple), pointer :: arbol_abb_capa_simple
        type(nodo_cliente), pointer :: cliente_actual
        print *, "---------------------------------------"
        print *, "CARGA MASIVA IMAGEN"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento de imagen:"
        print *, "---------------------------------------"
        read(*,*) documento_imagen
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_imagen)
        call json%info('',n_children=size_imagen)
        call json%get_core(jsonc)
        call json%get('', listaPunteroImagen, imagen_encontrada)
        do contador_imagen = 1, size_imagen
            call jsonc%get_child(listaPunteroImagen, contador_imagen, punteroImagen, imagen_encontrada)
            call jsonc%get_child(punteroImagen, 'id', atributoPunteroImagen, imagen_encontrada)
            call jsonc%get(atributoPunteroImagen, id_imagen)
            call jsonc%get_child(punteroImagen, 'capas', atributoPunteroImagen, imagen_encontrada)
            call jsonc%info(atributoPunteroImagen,n_children=size_capa)
            allocate(arbol_abb_capa_simple)
            do contador_capa = 1, size_capa
                call jsonc%get_child(atributoPunteroImagen, contador_capa, punteroCapa, imagen_encontrada)
                call jsonc%get(punteroCapa, id_capas)
                call arbol_abb_capa_simple%insertar(id_capas)
            end do
            !----------
            cliente_actual => lista_simple_cliente%cabeza
            do while (associated(cliente_actual))
                if (trim(cliente_actual%dpi) == dpi_global) then
                    call cliente_actual%arbol_avl_imagen%insertar_nodo(id_imagen, arbol_abb_capa_simple)
                    exit
                end if
                cliente_actual => cliente_actual%siguiente
            end do
            !----------
            !call arbol_avl_imagen%insertar_nodo(id_imagen, arbol_abb_capa_simple)
            deallocate(arbol_abb_capa_simple)
        end do
        call json%destroy()
    end subroutine carga_masiva_imagen
    !------------------------------------------------------------------------
    !CARGA MASIVA ALBUMES
    !------------------------------------------------------------------------
    subroutine carga_masiva_album()
        type(lista_imagen), pointer :: lista_imagen_album
        type(nodo_cliente), pointer :: cliente_actual
        print *, "---------------------------------------"
        print *, "CARGA MASIVA ALBUM"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento del album:"
        print *, "---------------------------------------"
        read(*,*) documento_album
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_album)
        call json%info('',n_children=size_album)
        call json%get_core(jsonc)
        call json%get('', listaPunteroAlbum, album_encontrado)
        do contador_album = 1, size_album
            call jsonc%get_child(listaPunteroAlbum, contador_Album, punteroAlbum, album_encontrado)
            call jsonc%get_child(punteroAlbum, 'nombre_album', atributoPunteroAlbum, album_encontrado)
            call jsonc%get(atributoPunteroAlbum, nombre_album)
            call jsonc%get_child(punteroAlbum, 'imgs', atributoPunteroAlbum, album_encontrado)
            call jsonc%info(atributoPunteroAlbum,n_children=imgs_size)
            allocate(lista_imagen_album)
            do contador_a = 1, imgs_size
                call jsonc%get_child(atributoPunteroAlbum, contador_a, punteroAlbum, album_encontrado)
                call jsonc%get(punteroAlbum, imgs)
                call lista_imagen_album%insertar_imagen(trim(imgs))
            end do
            !----------
            cliente_actual => lista_simple_cliente%cabeza
            do while (associated(cliente_actual))
                if (trim(cliente_actual%dpi) == dpi_global) then
                    call cliente_actual%lista_doble_album%insertar_album(nombre_album, lista_imagen_album)
                    exit
                end if
                cliente_actual => cliente_actual%siguiente
            end do
            !----------
            !call lista_doble_album%insertar_album(nombre_album, lista_imagen_album)
            deallocate(lista_imagen_album)
        end do
        call json%destroy()
    end subroutine carga_masiva_album
    !------------------------------------------------------------------------
    !CARGA MASIVA CLIENTE
    !------------------------------------------------------------------------
    subroutine carga_masiva_cliente()
        print *, "---------------------------------------"
        print *, "CARGA MASIVA CLIENTE"
        print *, "---------------------------------------"
        print *, "Ingrese el nombre del documento del cliente:"
        print *, "---------------------------------------"
        read(*,*) documento_cliente
        print *, "---------------------------------------"
        call json%initialize()
        call json%load(filename=documento_cliente)
        call json%info('',n_children=size_cliente)
        call json%get_core(jsonc)
        call json%get('', listaPunteroCliente, cliente_encontrado)
        do contador_cliente = 1, size_cliente
            call jsonc%get_child(listaPunteroCliente, contador_cliente, punteroCiente, cliente_encontrado)
            call jsonc%get_child(punteroCiente, 'dpi', atributoPunteroCliente, cliente_encontrado)
            call jsonc%get(atributoPunteroCliente, dpi_cliente)
            call jsonc%get_child(punteroCiente, 'nombre_cliente', atributoPunteroCliente, cliente_encontrado)
            call jsonc%get(atributoPunteroCliente, nombre_cliente)
            call jsonc%get_child(punteroCiente, 'password', atributoPunteroCliente, cliente_encontrado)
            call jsonc%get(atributoPunteroCliente, contrasena_cliente)
            call lista_simple_cliente%insertar_cliente(dpi_cliente, nombre_cliente, contrasena_cliente)
        end do
        call json%destroy()
    end subroutine carga_masiva_cliente
end program main