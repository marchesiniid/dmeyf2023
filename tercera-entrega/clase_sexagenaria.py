import duckdb

# Connect to DuckDB (creates an in-memory database)
con = duckdb.connect()

# Load data from CSV file into a DuckDB table
con.execute("CREATE TABLE competencia_02 AS SELECT * FROM read_csv_auto('~/buckets/b1/datasets/competencia_02_crudo.csv.gz');")

result=con.execute(f"""create or replace table targets as
with periodos as (
    select distinct foto_mes from competencia_02
), clientes as (
    select distinct numero_de_cliente from competencia_02
), todo as (
    select numero_de_cliente, foto_mes from clientes cross join periodos
), clase_ternaria as (
    select
        t.numero_de_cliente
        , t.foto_mes
        , if(c.numero_de_cliente is null, 0, 1) as mes_0
        , lead(mes_0, 1) over (partition by t.numero_de_cliente order by foto_mes) as mes_1
        , lead(mes_0, 2) over (partition by t.numero_de_cliente order by foto_mes) as mes_2
        , lead(mes_0, 3) over (partition by t.numero_de_cliente order by foto_mes) as mes_3
        , lead(mes_0, 4) over (partition by t.numero_de_cliente order by foto_mes) as mes_4
        , lead(mes_0, 5) over (partition by t.numero_de_cliente order by foto_mes) as mes_5
        , lead(mes_0, 6) over (partition by t.numero_de_cliente order by foto_mes) as mes_6
        , 
        case
        WHEN ((mes_0 == 0) ) THEN NULL
        WHEN (mes_1 == 0) THEN 'BAJA+1'
        WHEN (mes_2 == 0) THEN 'BAJA+2'
        WHEN (mes_3 == 0) THEN 'BAJA+3'
        WHEN (mes_4 == 0) THEN 'BAJA+4'
        WHEN (mes_5 == 0) THEN 'BAJA+5'
        WHEN (mes_6 == 0) THEN 'BAJA+6'
        ELSE 'CONTINUA'
        END as clase_ternaria
    from todo t
    left join competencia_02 c using (numero_de_cliente, foto_mes)
) select
  foto_mes
  , numero_de_cliente
  , clase_ternaria
from clase_ternaria 
where mes_0 = 1""")

res2=con.execute(f"""alter table competencia_02 add column clase_ternaria VARCHAR(10)
""")

res3=con.execute(f"""update competencia_02
set clase_ternaria = targets.clase_ternaria
from targets
where competencia_02.numero_de_cliente = targets.numero_de_cliente and competencia_02.foto_mes = targets.foto_mes;
""")

print("ACA FETCHEA")
result_df = res3.fetchdf()
print("ARRANCA A ESCRIBIR")
res3=con.execute(f"""
COPY competencia_02 TO '~/buckets/b1/datasets/colaborativos_withTargets.csv' (FORMAT CSV, COMPRESSION GZIP, HEADER)
""")
# Specify the path for the new CSV file
#output_csv = 'features.csv'

# Save the DataFrame to a new CSV file
#result_df.to_csv(output_csv, index=False)

con.close()
quit()
