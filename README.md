# Índice de Aire y Salud en la Ciudad de México 2022

Este repositorio contiene los datos de emisiones de ozono y partículas menores a 10 y 2.5 um de la Ciudad de México del año 2022.

Con los cuales se determina el número de días por mes con categorías del Índice de Calidad del Aire y Riesgos a la Salud de acuerdo a lo establecido por la Norma Oficial Mexicana **NOM-172-SEMARNAT-2019** (https://www.dof.gob.mx/nota_detalle.php?codigo=5579387&fecha=20/11/2019#gsc.tab=0).

Este proyecyo contiene tres carpetas
- `bin`: donde se encuentra el código para categorizar los días de aceurdo con los intervalos de los valores normados para cada contaminante y generar las gráficas.
- `data`: se encuentran las tablas con la información horaria de los contaminantes utilizados y las tablas obtenidas del conteo de días categorizados por mes.
- `figs`: Contiene los gráficos de mosaico y gráficos de barras de cada contaminante: ozono, partículas menores a 10 y 2.5 um, para el año 2022.

-------------

## Calidad el aire en la CDMX

La contaminación atmosférica tiene diferentes efectos en la salud humana a corto y largo plazo. La mayoría de los contaminantes irritan las vías respiratorias, pero también pueden agravar enfermedades cardiovasculares y pulmonares o respiratorias.

En la República Mexicana se cuenta con un índice para el monitoreo de la calidad del aire desde 1977, sin embargo, fue hasta 1982 que se comenzó a difundir esta información a la población mediante los medios de comunicación de la época, sin embargo, no se contaba con un documento oficial que indicara su importancia y evaluación, por lo que en 2006, se generó la norma ambiental NADF-009-AIRE-2006 (http://siga.jalisco.gob.mx/assets/documentos/normatividad/nadf-009-aire-2006.pdf) para el índice conocido como Índice Metropolitcano de la Calidadl del Aire (IMECA), que posteriormente, en 2017 cambió su nombre a Índice de la Calidad del Aire de la Ciudad de México (ICA) con las modificaciones realizadas en la NADF-009-AIRE-2017 (http://www.aire.cdmx.gob.mx/descargas/monitoreo/normatividad/NADF-009-AIRE-2017.pdf) debido a diferentes actualizaciones de normas oficiales en materia de salud ambiental. Sin embargo, estos índices era utilizados únicamente para los niveles de contaminación atmosférica de la Ciudad de México. Por lo que, en 2019, se crea el primer índice a nivel nacional, publicado en la NORMA Oficial Mexicana NOM-172-SEMARNAT-2019, Lineamientos para la obtención y comunicación del **Índice de Calidad del Aire y Riesgos a la Salud (IAS)** (https://www.dof.gob.mx/nota_detalle.php?codigo=5579387&fecha=20/11/2019#gsc.tab=0). Que tiene como objetivo informar de manera clara, oportuna y continua el estado de la calidad del aire, los probables daños a la salud que ocasiona y las medidas que se pueden tomar para reducir los daños a la salud po exposición a los contaminantes.

Este índice usa cinco colores para designar las categorías que se asignan a cada intervalo de concentración, que es diferente para cada contaminante evaluado. A cada categría correspode una recomendación de actividades para la población en general y para los grupos sensibles (niños, personas con enfermedades cardiovasculares y/o respiratorias, adultos mayores de 65 años, mujeres embarazadas y personas que requieren atención especial debido al tipo de actividades que realizan).
El límite superior de la categoría *Aceptable* concuerda con los valores límite permisibles para la concentración ambiental del contaminante de que se trate y que están señalados en las normas oficales mexicanas en materia de salud ambiental.
Las entidades federativas que cuenten con sistema de monitoreo de calidad del aire, deben difundir el índice de forma continua y horaria, obligatoriamente a través de una plataforma electrónica oficial y preferentemente en tantos medios como sea posible.

La Ciudad de México cuenta con 34 estaciones de monitoreo continuo de las cuales se eliminan las estaciones consideradas de transporte de contaminantes (ACO, MON, MPA, AJU, INN) y se utilizan 29 para el cálculo del IAS.
Los datos horarios de los contaminantes fueron descargados de la página oficial de la Dirección de Monitoreo Atmosférico de la CDMX (http://www.aire.cdmx.gob.mx/default.php), para ozono las concentraciones horarias y para partículas los datos de NowCast, ya que estos indicadores suelen tener las concentraciones más altas y se toman como criterio para declarar Contingencias Ambientales Atmosféricas (PCAA).

En el año 2022 se registraron seis días con calidad del aire Extremadamente Mala tomando en cuenta los tres contaminantes (Ozono y PMs), en los meses de enero, febrero, marzo, abril y diciembre, correspondientes a la temporada de secas. Aunque ninguna de las fechas coincide con la activación de contingencia ambiental atmosféricas (http://www.aire.cdmx.gob.mx/descargas/ultima-hora/calidad-aire/pcaa/pcaa-historico-contingencias.pdf)

![conteo-ozono](https://github.com/bcsimat/conteos-IAS/blob/main/figs/todos_conteo_v3.png)
