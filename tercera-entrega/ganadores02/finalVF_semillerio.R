# para correr el Google Cloud
#   8 vCPU
#  16 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "8"

PARAM$input$dataset <- "./datasets/competencia_03_FE_Flores.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202012, 202101, 202102, 202103, 202104, 202105,202106,202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo
# Tomo parámetros obtenidos de BO de casi 5 días (1 mejor ganancia)
PARAM$finalmodel$semilla <- 880007

PARAM$finalmodel$num_iterations <- 332
PARAM$finalmodel$learning_rate <- 0.0353492857681127
PARAM$finalmodel$feature_fraction <- 0.497982733742354
PARAM$finalmodel$min_data_in_leaf <- 142
PARAM$finalmodel$num_leaves <- 1023

semillas <- c(500107, 500167, 579967, 835973, 718387, 
              111011, 111012, 111013, 111014, 111015,
             500109, 500169, 579969, 835077, 718389,
             111021, 111031, 111041, 111051, 111061)

PARAM$finalmodel$max_bin <- 31

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1/")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------
# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

ganancias <- tibble::tribble(~semilla,~ganancia,~envios)
lista_tb_entrega <- list()


for (i in 1:20) {
  PARAM$finalmodel$semilla <- semillas[i]
    # genero el modelo
  # estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
  modelo <- lgb.train(
    data = dtrain,
    param = list(
      objective = "binary",
      max_bin = PARAM$finalmodel$max_bin,
      learning_rate = PARAM$finalmodel$learning_rate,
      num_iterations = PARAM$finalmodel$num_iterations,
      num_leaves = PARAM$finalmodel$num_leaves,
      min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
      feature_fraction = PARAM$finalmodel$feature_fraction,
      seed = PARAM$finalmodel$semilla
    )
  )

#--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  archivo_importancia <-  paste0("semilla_",i,"_impo.txt")
  
  fwrite(tb_importancia,
         file = archivo_importancia,
         sep = "\t"
  )
  
  #--------------------------------------
  
  
  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]
  
  # grabo las probabilidad del modelo
  fwrite(tb_entrega,
         file = paste0("prediccion_",i,"_.txt"),
         sep = "\t"
  )
  
  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)
  lista_tb_entrega[[i]] <- tb_entrega


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

  cortes <- seq(8000, 15000, by = 500)
  for (envios in cortes) {
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]
    
    fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
           file = paste0(PARAM$experimento, "_", i, "_", envios, ".csv"),
           sep = ","
    )

    tb_ganancias <- tb_entrega[truth, on = c("numero_de_cliente"), nomatch = 0]
    tb_ganancias <- tb_ganancias[Predicted == 1,]
    tb_ganancias[,gan := fifelse(clase_ternaria == "BAJA+2",273000,-7000)]
    
    ganancia <- tibble::tribble(~semilla,~ganancia,~envios,
                                semillas[i], sum(tb_ganancias$gan),envios)
    
    ganancias <- rbind(ganancias,ganancia)
  
  }

    print(paste0("Iteracion ",i, " finalizada"))
}

write.csv(ganancias,
       file = paste0(PARAM$experimento, "_ganancias_semillerio.csv"),
       sep = ",")

tb_entrega_promedio <- rbindlist(lista_tb_entrega)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
setorder(tb_entrega_promedio, -prob)

for (envios in cortes) {
    tb_entrega_promedio[, Predicted := 0L]
    tb_entrega_promedio[1:envios, Predicted := 1L]
  
    fwrite(tb_entrega_promedio[, list(numero_de_cliente, Predicted)],
      file = paste0(PARAM$experimento,"semillerio_20_promedio","_", envios, ".csv"),
      sep = ","
    )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
