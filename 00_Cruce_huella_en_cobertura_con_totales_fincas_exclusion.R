## Analizar fincas en municipios de exclusion sobre huella actual
## Analizar fincas en municipios de exclusion en huella actual y no en
## total de fincas con xy


## Library load

library(readxl)
library(data.table)
library(stringr)

## Environment cleanning

rm(list = ls())

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


## Path and static variables definition

## Definicion de rutas y variables estáticas
in.files <- './00_in_data/'
out.files <- './01_out_data/'
FTTH.file <- str_c(out.files, '01_total_direcciones_FTTH.txt', sep = '', collapse = T)
munis.file <- '../../00_Analisis_municipios/data/clean/00_Analisis_municipios.xlsx'
evol.semanal.file <- './01_out_data/evolucion_semanal_municipio.csv'
fecha <- format(Sys.time(), "%Y%m%d")