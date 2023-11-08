require(data.table)

dataset <- fread("buckets/b1/datasets/competencia_03.csv.gz")

## Lags de todas las variables

setorder(dataset,cols = foto_mes)

lagcols <- setdiff(names(dataset),c("numero_de_cliente","foto_mes","clase_ternaria"))

lagcols1_names <- paste0("lag1_",lagcols)
lagcols3_names <- paste0("lag3_",lagcols)
lagcols6_names <- paste0("lag6_", lagcols)


dataset[, (lagcols1_names) :=  shift(.SD, 1), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols3_names) :=  shift(.SD, 3), .SDcols = lagcols, by=numero_de_cliente]
dataset[, (lagcols6_names) :=  shift(.SD, 6), .SDcols = lagcols, by=numero_de_cliente]

meses <- c(202012,seq(202101,202107,1))

dataset <- dataset[foto_mes %in% meses,]


fwrite(dataset,
       file = "buckets/b1/datasets/dataset_baseline_exp_colab.csv.gz",
       sep = ",")


