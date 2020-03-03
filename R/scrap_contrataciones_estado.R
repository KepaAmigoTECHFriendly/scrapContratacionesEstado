#' @title Scrapea la Plataforma de Contratación del Sector Público.
#'
#' @description Scrapea la Plataforma de Contratación del Sector Público y filtra el resultado en función de la palabra clave parámetro, devolviendo un json
#' enviándo este a la plataforma Smart City (TECH) del cliente correspondiente.
#'
#' @param palabra_clave
#'
#' @return json
#'
#' @examples scrap_contrataciones_estado("suministro")
#'
#' @import jsonlite
#' tidyr
#' lubridate
#' httr
#' stringr
#' anytime
#' rvest
#' xml2
#'
#' @export

scrap_contrataciones_estado <- function(palabra_clave){

  # Palabra clave por la que hacer el filtrado
  palabra_clave <- as.character(tolower(palabra_clave))

  #Evita que la palabra clave sean artículos o espacios en blanco
  if(nchar(palabra_clave) <= 3 & tolower(palabra_clave) != "cpi" ){
    return(1)  # Error por longitud de palabra clave
  }

  todos_registros <- NULL # Inicialización de 'todos_registros' para que luego el sistema no pete haciendo un rbind

  agente <- paste("google_chrome_v",sample(10000,1),Sys.time(),sep=".")
  web    <- html_session("https://contrataciondelestado.es/wps/portal/licRecientes",user_agent(agente))

  links  <- web %>% html_nodes(xpath="//td[contains(@headers,'idthExpediente')]/span/a") %>% html_attr("href")
  links  <- paste("https://contrataciondelestado.es",links,sep="")
  links  <- links %>% as.data.frame()

  exped  <- web %>% html_nodes(xpath="//td[contains(@headers,'idthExpediente')]/span/a") %>% html_text()
  fecha  <- web %>% html_nodes(xpath="//td[contains(@headers,'abiertas_idthFUActualizacion')]/span[2]") %>% html_text() %>% dmy_hm(tz="CET")
  estado <- web %>% html_nodes(xpath="//div[contains(@class,'estadoLicitacionAzul')]/span") %>% html_text()

  links$exped  <- exped
  links$fecha  <- fecha
  links$estado <- estado

  #lectura de fichas de expedientes

  for(i in 1:nrow(links))
  {
    agente <- paste("firefoxBrowserVersion",Sys.Date()-sample(365,1),sep="-")
    web <- html_session(as.character(links$.[i]),user_agent(agente))
    filas <- web %>% html_nodes(xpath="//fieldset/div[contains(@class,'capaAtributos')]/ul[contains(@class,'altoDetalle')]/li[contains(@id,'fila')]")

    columna1 <- filas[grep("columna1",filas)] %>% html_text(trim=TRUE) %>% as.data.frame()
    columna2 <- filas[grep("columna2",filas)] %>% html_text(trim=TRUE)
    columna2 <- gsub("[\t\r\n]","",columna2) %>% as.data.frame()

    datos <- 1:nrow(columna1)
    datos <- cbind(datos,columna1,columna2)
    colnames(datos) <- c("expediente","key","value")

    enlace_licit <- web %>% html_nodes(xpath="//a[contains(@id,'link_Enlace')]") %>% html_attr("href")
    enlace_licit <- cbind("enlace","Enlace licitación",enlace_licit)
    colnames(enlace_licit) <-c("expediente","key","value")

    enlace_docum <- web %>% html_nodes(xpath="//td[contains(@class,'documentosPub')]/div/a[1]") %>% html_attr("href")
    if(identical(enlace_docum,character(0)))
    {
      enlace_docum <- "(ver detalles en enlace de licitación)"
    }

    nombre_docum <- web %>% html_nodes(xpath="//td[contains(@class,'tipoDocumento')]") %>% html_text(trim=TRUE)
    if(identical(nombre_docum,character(0)))
    {
      nombre_docum <- web %>% html_nodes(xpath="//table[contains(@id,'TablaDetallePliegosPlatAgre')]/tbody/tr[contains(@class,'rowClass')]/td[contains(@class,'padding0punto2')]/div[contains(@class,'padding0punto1')]") %>% html_text(trim=TRUE)
    }
    if(identical(nombre_docum,character(0)))
    {
      nombre_docum <- "(ver detalles en enlace de licitación)"
    }

    fecha_docum  <- web %>% html_nodes(xpath="//td[contains(@class,'fechaPubLeft')]") %>% html_text(trim=TRUE)
    if(identical(fecha_docum,character(0)))
    {
      fecha_docum <- "(ver fecha en enlace de licitación)"
    }

    nombre_docum <- gsub("Ã³","ó",nombre_docum)
    nombre_docum <- paste(fecha_docum,"- Documento:",nombre_docum)
    documentos   <- cbind("docs",nombre_docum,enlace_docum)
    colnames(documentos) <-c("expediente","key","value")
    documentos   <- documentos %>% as.data.frame()
    documentos   <- documentos[order(documentos$key),]

    datos <- rbind(datos,enlace_licit,documentos)

    datos$value <- gsub("[\t\r\n]","",datos$value)
    datos$expediente       <- links$exped[i]
    datos$fecha_plataforma <- links$fecha[i]

    #if(identical(grep("Publicada",datos$value),integer(0))) {next()}
    todos_registros <- rbind(datos,todos_registros)
    todos_registros <- unique(todos_registros)

  }

  ##############################################
  # BÚSQEDA POR PALABRA CLAVE
  ##############################################

  #Generación Data Frame ordenado

  nombres <- c("Objeto del contrato", "Tipo de Contrato:", "Código CPV", "Presentación", "Órgano de Contratación",
               "Estado de la Licitación", "Resultado", "Presupuesto base de licitación sin impuestos", "Valor estimado del contrato:",
               "Lugar de Ejecución", "Procedimiento de contratación", "Adjudicatario", "Nº de Licitadores Presentados",
               "Importe de Adjudicación", "Enlace licitación", "Fecha fin de presentación de oferta", "Documento: Formalización",
               "Documento: Anuncio de Licitación", "Documento: Pliego", "Documento: Adjudicación")

  matriz_datos <- matrix(, nrow = 6, ncol = length(nombres))

  expedientes <- unique(todos_registros$expediente)

  for (j in 1:length(expedientes)){
    for(i in 1:length(nombres)){
      if(str_detect(nombres[i], "Documento")){
        if(any(todos_registros[gsub(".*[-] ","",todos_registros$key) == nombres[i], 1] == expedientes[j]) & !identical(todos_registros[gsub(".*[-] ","",todos_registros$key) == nombres[i], 1] == expedientes[j],logical(0))){
          pos_variable <- grep(nombres[i], as.character(todos_registros[todos_registros$expediente == expedientes[j], 2]))[1]
          matriz_datos[j, i] <- as.character(todos_registros[(min(grep(gsub("[ ].*","",expedientes[j]),todos_registros$expediente)) + pos_variable - 1), 3])
        }else{
          matriz_datos[j, i] <- "-"
        }

      }else{

        if(any(todos_registros[todos_registros$key == nombres[i], 1] == expedientes[j]) & !identical(todos_registros[todos_registros$key == nombres[i], 1] == expedientes[j],logical(0))){
          pos_variable <- grep(nombres[i], as.character(todos_registros[todos_registros$expediente == expedientes[j], 2]))
          matriz_datos[j, i] <- as.character(todos_registros[(min(grep(gsub("[ ].*","",expedientes[j]),todos_registros$expediente)) + pos_variable - 1), 3])
        }else{
          matriz_datos[j, i] <- "-"
        }
      }
    }
  }

  # Extracción fecha plataforma
  presentacion <- c()
  for(i in 1:length(expedientes)){
    presentacion <- c(presentacion, unique(todos_registros$fecha_plataforma[min(grep(gsub("[ ].*","",expedientes[i]),todos_registros$expediente))]))
  }
  presentacion <- as.POSIXct(presentacion, origin="1970-01-01")


  df <- data.frame("Expediente" = expedientes)
  df2 <- as.data.frame(matriz_datos)
  names(df2) <- nombres

  df_contrataciones_completo <- na.omit(cbind(df,df2))

  df_contrataciones_completo$Presentación <- presentacion


  # EXTRACCIÓN PALABRA CLAVE
  # Extracción objeto de contrato
  objeto_contrato <- df_contrataciones_completo$`Objeto del contrato`

  # Extracción expedientes que cumplen la condición de la palabra clave
  pos_palabra_clave <- grep(palabra_clave,tolower(objeto_contrato))

  #Comprobación existencia de palabra clave
  if(!any(grepl(palabra_clave,tolower(objeto_contrato)))){
    return(0)  # Error por falta de datos.
  }

  expediente_palabra_clave <- c()
  for(i in pos_palabra_clave){
    expediente_palabra_clave <- c(expediente_palabra_clave, as.character(df_contrataciones_completo$Expediente[i]))
  }

  # Subset DF todos_registros por expedientes palabra clave
  df_filtrado <- subset(df_contrataciones_completo, Expediente == expediente_palabra_clave)



  #===============================================================
  # CREACIÓN JSON Y ENVÍO A PLATAFORMA SMART CITY
  #===============================================================

  #Variables envío JSON a plataforma
  TB_token <- "Jz90c8d89Ub3fmlWFNsi"
  TB_url   <- paste("http://94.130.77.253:8080/api/v1/",TB_token,"/telemetry",sep="")

  json_contrataciones_return <- toJSON(df_filtrado,pretty=T)

  #Extracción timestamp en formato unix
  tsi <- format(as.numeric(anytime(Sys.Date()))*1000,scientific = F)
  #tsi <- sub("\\..*", "",tsi)
  for(i in 1:nrow(df_filtrado)){
    ts <- as.numeric(tsi) +i  #Añade i ms al timestamp para poder verse sin solapamiento en el widget de la plataforma smart city.

    #Creación de JSON noticias y eliminación de ][ para cumplir con el formato json con modificación de timestamp de thingsboard.
    json_contrataciones <- toJSON(df_filtrado[i,],pretty=T)
    json_contrataciones <- sub("[[]","",json_contrataciones)
    json_contrataciones <- sub("[]]","",json_contrataciones)

    #Formato json con modificación de timestamp de thingsboard.
    json_envio_plataforma <- paste('{"ts":',ts,', "values":',json_contrataciones,"}",sep="")

    #Envío JSON a plataforma
    POST(url=TB_url,body=json_envio_plataforma)
  }

  return(json_contrataciones_return)

}
