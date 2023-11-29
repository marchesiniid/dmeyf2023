#Ensamble de modelos 3: AutoGluon_test1, 824_nobaja4_semillerio20_201909-202107, y 824_nobaja4_drifting_201901-202105_iteracion67semillerio20, 824_nobaja4_drifting_lrAlto-iteracion72_202105, 824_VF_202012,  824_baja4_semillerio20_iteracion85_201909-201912_202001_202011-202107

require("data.table")

#autogluon
autogluon_predicciones <- read.csv("/home/marchesini_id/buckets/b1/exp/AutoGluon_test/buckets/b1/exp/AutoGluon_test/predicciones_autogluon.csv")
autogluon_predicciones <- as.data.table(autogluon_predicciones)
autogluon_predicciones <- autogluon_predicciones[, 1:3, with=FALSE]
setnames(autogluon_predicciones, old = "prob_1", new = "prob")
#modelo 1- 824nobaja4202109-202107


lista_predicciones_1 <- list()

path_1 <- "/home/marchesini_id/buckets/b1/exp/824_baja4_undersampling05_2019-2021/"
for (i in 1:20){
  predicciones <- fread(paste0(path_1, "prediccion_", i,"_.txt"))
  lista_predicciones_1[[i]] <- predicciones 
}

tb_entrega_promedio_1 <- rbindlist(lista_predicciones_1)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
setorder(tb_entrega_promedio_1, -prob)


#modelo 2  nobaja4-drifting-201901-202105
lista_predicciones_2 <- list()
path_2 <- "/home/marchesini_id/buckets/b1/exp/824_nobaja4_drifting_201901-202105_iteracion67/"
for ( i in 1:20){
  predicciones <- fread(paste0(path_2, "prediccion_", i,"_.txt"))
  lista_predicciones_2[[i]] <- predicciones
}

tb_entrega_promedio_2 <- rbindlist(lista_predicciones_2)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
setorder(tb_entrega_promedio_2, -prob)


#modelo 3  VF
lista_predicciones_3 <- list()
path_3 <- "/home/marchesini_id/buckets/b1/exp/824_VF_202012/"
for ( i in 1:20){
  predicciones <- fread(paste0(path_3, "prediccion_", i,"_.txt"))
  lista_predicciones_3[[i]] <- predicciones
}


tb_entrega_promedio_3 <- rbindlist(lista_predicciones_3)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
setorder(tb_entrega_promedio_3, -prob)


#modelo 4  lrAlto
lista_predicciones_4 <- list()
path_4 <- "/home/marchesini_id/buckets/b1/exp/824_nobaja4_drifting_lrAlto-iteracion72_202105/"
for ( i in 1:20){
  predicciones <- fread(paste0(path_3, "prediccion_", i,"_.txt"))
  lista_predicciones_4[[i]] <- predicciones
}


tb_entrega_promedio_4 <- rbindlist(lista_predicciones_4)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
setorder(tb_entrega_promedio_4, -prob)

#modelo 5 -  824_baja4_iteracion85

lista_predicciones_5 <- list()
path_5 <- "/home/marchesini_id/buckets/b1/exp/824_baja4_semillerio20_iteracion85_201909-201912_202001_202011-202107/"
for ( i in 1:20){
  predicciones <- fread(paste0(path_5, "prediccion_", i,"_.txt"))
  lista_predicciones_5[[i]] <- predicciones
}


tb_entrega_promedio_5 <- rbindlist(lista_predicciones_5)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
setorder(tb_entrega_promedio_5, -prob)

#Ensamble de los 5

lista_final <- list()
lista_final[[1]] <- autogluon_predicciones
lista_final[[2]] <- tb_entrega_promedio_1
lista_final[[3]] <- tb_entrega_promedio_2
lista_final[[4]] <- tb_entrega_promedio_3
lista_final[[5]] <- tb_entrega_promedio_4
lista_final[[6]] <- tb_entrega_promedio_5

tb_promedio_ensamble <- rbindlist(lista_final)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
setorder(tb_promedio_ensamble, -prob)

path_final <- "/home/marchesini_id/buckets/b1/exp/ensamble_modelos_3/cortes250/"
dir.create(path_final, showWarnings = FALSE)

cortes <- seq(7500, 17000, by = 250)
for (envios in cortes) {
  tb_promedio_ensamble[, Predicted := 0L]
  tb_promedio_ensamble[1:envios, Predicted := 1L]
  
  fwrite(tb_promedio_ensamble[, list(numero_de_cliente, Predicted)],
         file = paste0(path_final, "ensamble_modelos_3","_", envios, ".csv"),
         sep = ","
  )
}