require("data.table")
require("tidyverse")

# Aqui se debe poner la carpeta de la materia de SU computadora local

# Calculo clase ternaria
bd <- fread("buckets/b1/datasets/competencia_03_crudo.csv.gz")

mes_clientes <- data.table(bd %>% distinct(foto_mes)) %>% 
  cross_join(bd %>% distinct(numero_de_cliente))

mes_clientes_bd <- bd[,c("numero_de_cliente","foto_mes")]
mes_clientes_bd[,si := 1]

mes_clientes_presencia <- merge(mes_clientes, mes_clientes_bd, all.x = TRUE)
mes_clientes_presencia[,si:= fifelse(is.na(si),0,si)]
setorder(mes_clientes_presencia,cols = foto_mes)
mes_clientes_presencia[,c("lag_1", "lag_2") := .(shift(si, n = 1, type = "lead"), shift(si, n = 2, type = "lead")), by = numero_de_cliente]
mes_clientes_presencia[, clase_ternaria := fifelse(lag_2 == 1, "CONTINUA", fifelse(lag_1 == 1, "BAJA+2", "BAJA+1"))]

mes_clientes_presencia <- mes_clientes_presencia[,c("numero_de_cliente","foto_mes","clase_ternaria")]
bd_final <- merge(bd,mes_clientes_presencia, all.x = T)

fwrite(bd_final,
       file = "buckets/b1/datasets/competencia_03.csv.gz",
       sep = ",")
