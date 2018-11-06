### Integrar una única capa con todas las de AT de entrada
### Buscar capas ATOM del municipio equivalente
### Exportar capa ATOM y capa AT a directorio de análisis por municipio
### Crear los directorios si no existen
### Exportar capa única de AT para fase de cruce con fincas



############################################################################################################################
########################################             LIBRARY LOAD                ###########################################
############################################################################################################################

library(readxl)
library(data.table)
library(stringr)
library(rgdal)

## Environment cleanning

rm(list = ls())

############################################################################################################################
########################################             FUNCTION DEFS               ###########################################
############################################################################################################################

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '^', na="")
  close(f)  
}

# Paste data into R

paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

cols.tabla.comillas <- function(tabla.datos){
  cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")
}

cols.tabla <- function(tabla.datos){
  cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")
}


lista.ficheros <- function(in.process.dir, extension.fichero){
  
  if (dir.exists(in.process.dir)){
    ficheros <-  list.files(in.process.dir, full.names = T)
    if (length(ficheros)>0){
      for (in.process.file in ficheros){
        nombre.fichero <- strsplit(in.process.file, "/")[[1]]
        nombre.fichero <- nombre.fichero[length(nombre.fichero)]
        
        ## Cargamos los ficheros eliminando las filas sin datos que pueda haber
        
        
        if (exists("ficheros.total")){
          ficheros.total <- rbind(ficheros.total, data.table( RutaCompleta = in.process.file,
                                                              NombreFichero = nombre.fichero,
                                                              FechaModificacion = file.mtime(in.process.file),
                                                              Size = file.size(in.process.file)))
        }
        
        
        if (!exists("ficheros.total")){
          ficheros.total <- data.table( RutaCompleta = in.process.file,
                                        NombreFichero = nombre.fichero,
                                        FechaModificacion = file.mtime(in.process.file),
                                        Size = file.size(in.process.file))
        }
      }
      
      
      ## Eliminamos los ficheros que no tengan la extension
      
      ficheros.total[, str_to_upper(str_sub(NombreFichero, start = str_length(NombreFichero)-(str_length(extension.fichero)-1))) == toupper(extension.fichero)]
      
      ficheros.total <- ficheros.total[str_to_upper(str_sub(NombreFichero, start = str_length(NombreFichero)-(str_length(extension.fichero)-1))) == toupper(extension.fichero),]
      return(ficheros.total)
    }
    
  }
}

capa.prepara <- function(ruta){
  
  directorio <- str_c(head(unlist(strsplit(ruta, split = "/")), length(unlist(strsplit(ruta, split = "/")))-1), collapse = "/") 
  capa <- unlist(strsplit(tail(unlist(strsplit(ruta, split = "/")), 1), split = "[.]"))[1]
  capa.temp <- readOGR(dsn = directorio, layer = capa, disambiguateFIDs = TRUE)
  capa.temp@data$bolsa.numero  <- 1:nrow(capa.temp)
  capa.temp@data$bolsa.ID  <- paste(capa.temp@data$bolsa.numero, capa.temp@data$INE, sep = '_')
  
  # Establecer proyeccion
  
  capa.temp <- spTransform(capa.temp, CRS("+init=epsg:32630"))
  proj4string(capa.temp) <- CRS("+init=epsg:32630")
  
  return(capa.temp)
}


EPSG <- make_EPSG()
EPSG[grepl("WGS 84", EPSG$note),]

############################################################################################################################
########################################            PATH & STATIC VARS               #######################################
############################################################################################################################

dir.salida <- './outdata/shapefiles'
nombre.capa.salida <- 'bolsas'
fich.munis.exclusion <- 'indata/municipios_exclusion.xlsx'
fecha <- format(Sys.time(), "%Y%m%d")


############################################################################################################################
########################################            IMPORT DATA                      #######################################
############################################################################################################################

munis.exclusion <- as.data.table(read_xlsx(fich.munis.exclusion, sheet = 1, col_names = T, col_types = 'text', trim_ws = T))


############################################################################################################################
########################################            MAIN CODE                        #######################################
############################################################################################################################

## Generar capa total de municipios AT

ficheros.total <- lista.ficheros('indata/AT/', 'shp')


if (nrow(ficheros.total)>0){
  list.fich <- as.list(ficheros.total$RutaCompleta)
  capa.integrada <-  do.call(rbind.SpatialPolygonsDataFrame, c(lapply(list.fich, capa.prepara), makeUniqueIDs = T))
}



############################################################################################################################
########################################            EXPORTS                          #######################################
############################################################################################################################

suppressWarnings(writeOGR(capa.integrada, dsn = dir.salida, layer = nombre.capa.salida, driver = "ESRI Shapefile", overwrite_layer = T, verbose = T, encoding = 'UTF-8' ))

############################################################################################################################
########################################            REPORTS                          #######################################
############################################################################################################################




