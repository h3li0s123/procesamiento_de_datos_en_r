#### Código elaborado por Helios García
#### Ejercicios para la vacante de Enlance Supervisor
#### 30 de enero del 2023


##### Paquetes ----

#Cargamos los paquetes que necesitaremos para el ejercicio.
library(pacman)
p_load(tidyverse, janitor, ggplot2, readr, readxl)


#### 3. Personal de las agencias y/o fiscalías del Ministerio Público de la Procuraduría General de Justicia o Fiscalía General, por entidad federativa y nivel de escolaridad según cargo y sexo. ----

###Obtenemos la base de datos y especificamos el tipo de variable

bd_grado_cargo_sexo <- read_csv("01_datos/03_ejercicio/bd_ejercicio_3.csv", 
         col_types = cols(cafuaftt = col_double(), 
                          cafuaf1 = col_double(), cafuaf2 = col_double(),
                          cafuaf13 = col_double(),
                          cafuaf14 = col_double()))

##Agregamos los nombres de las entidades federativas y cambiamos los nombres de las variables


bd_grado_cargo_sexo <- bd_grado_cargo_sexo %>% 
  mutate(entidad_a = if_else(entidad_a == 1, "Aguascalientes",
                             if_else(entidad_a == 2, "Baja California",
                                     if_else(entidad_a == 3, "Baja California Sur",
                                             if_else(entidad_a==4, "Campeche", 
                                                     if_else(entidad_a==5, "Coahuila",
                                                             if_else(entidad_a==6, "Colima",
                                                                     if_else(entidad_a==7, "Chiapas",
                                                                             if_else(entidad_a==8, "Chihuahua",
                                                                                     if_else(entidad_a==9, "Ciudad de México",
                                                                                             if_else(entidad_a==10, "Durango",
                                                                                                     if_else(entidad_a==11, "Guanajuato",
                                                                                                             if_else(entidad_a==12, "Guerrero",
                                                                                                                     if_else(entidad_a==13, "Hidalgo",
                                                                                                                             if_else(entidad_a==14, "Jalisco",
                                                                                                                                     if_else(entidad_a==15, "Estado de México",
                                                                                                                                             if_else(entidad_a==16, "Michoacán",
                                                                                                                                                     if_else(entidad_a==17, "Morelos",
                                                                                                                                                             if_else(entidad_a==18, "Nayarit",
                                                                                                                                                                     if_else(entidad_a==19, "Nuevo León",
                                                                                                                                                                             if_else(entidad_a==20, "Oaxaca",
                                                                                                                                                                                     if_else(entidad_a==21, "Puebla",
                                                                                                                                                                                             if_else(entidad_a==22, "Querétaro",
                                                                                                                                                                                                     if_else(entidad_a==23, "Quintana Roo",
                                                                                                                                                                                                             if_else(entidad_a==24, "San Luis Potosí",
                                                                                                                                                                                                                     if_else(entidad_a==25, "Sinaloa",
                                                                                                                                                                                                                             if_else(entidad_a==26, "Sonora",
                                                                                                                                                                                                                                     if_else(entidad_a==27, "Tabasco",
                                                                                                                                                                                                                                             if_else(entidad_a==28, "Tamaulipas",
                                                                                                                                                                                                                                                     if_else(entidad_a==29, "Tlaxcala",
                                                                                                                                                                                                                                                             if_else(entidad_a==30, "Veracruz",
                                                                                                                                                                                                                                                                     if_else(entidad_a==31, "Yucatán", "Zacatecas")))))))))))))))))))))))))))))))) %>%
  rename(entidad = "entidad_a", 
         nivel_de_escolaridad = "gdoestud_c",
         total_personal = "cafuaftt",
         fiscales_de_ministerio_publico_hombres = "cafuaf3",
         fiscales_de_ministerio_publico_mujeres = "cafuaf4",
         agentes_de_ministerio_publico_hombres = "cafuaf5",
         agentes_de_ministerio_publico_mujeres = "cafuaf6",
         secretarios_del_ministerio_publico_hombres = "cafuaf7",
         secretarios_del_ministerio_publico_mujeres = "cafuaf8",
         actuarios_del_ministerio_publico_hombres = "cafuaf9",
         actuarios_del_ministerio_publico_mujeres = "cafuaf10",
         peritos_hombres = "cafuaf11",
         peritos_mujeres = "cafuaf12",
         policias_ministeriales_o_investigadores_o_judiciales_hombres = "cafuaf13",
         policias_ministeriales_o_investigadores_o_judiciales_mujeres = "cafuaf14",
         personal_administrativo_y_de_apoyo_hombres = "cafuaf15",
         personal_administrativo_y_de_apoyo_mujeres = "cafuaf16",
         otros_hombres = "cafuaf17",
         otros_mujeres = "cafuaf18")%>% 
  select(-cafuaf1, -cafuaf2) %>% 
  filter(nivel_de_escolaridad != 9)

#### Agregamos los nombres de la escolaridad

bd_grado_cargo_sexo <- bd_grado_cargo_sexo %>% 
  mutate(nivel_de_escolaridad = if_else(nivel_de_escolaridad == 1, "Ninguno",
                                        if_else(nivel_de_escolaridad == 2, "Preescolar o primaria",
                                                if_else(nivel_de_escolaridad == 3, "Secundaria",
                                                        if_else(nivel_de_escolaridad == 4, "Preparatoria",
                                                                if_else(nivel_de_escolaridad == 5, "Carrera técnica o carrera comercial",
                                                                        if_else(nivel_de_escolaridad == 6, "Licenciatura",
                                                                                if_else(nivel_de_escolaridad == 7, "Maestría",
                                                                                        "Doctorado"))))))))







#Obtenemos los datos generales de la federación

datos_federacion_general <- bd_grado_cargo_sexo %>% 
  group_by(nivel_de_escolaridad) %>% 
  summarise(total_personal = sum(total_personal, na.rm = T),
            fiscales_de_ministerio_publico_hombres = sum(fiscales_de_ministerio_publico_hombres, na.rm = T),
            fiscales_de_ministerio_publico_mujeres = sum(fiscales_de_ministerio_publico_mujeres, na.rm = T),
            agentes_de_ministerio_publico_hombres = sum(agentes_de_ministerio_publico_hombres, na.rm = T),
            agentes_de_ministerio_publico_mujeres = sum(agentes_de_ministerio_publico_mujeres, na.rm = T),
            secretarios_del_ministerio_publico_hombres = sum(secretarios_del_ministerio_publico_hombres, na.rm = T),
            secretarios_del_ministerio_publico_mujeres = sum(secretarios_del_ministerio_publico_mujeres, na.rm = T),
            actuarios_del_ministerio_publico_hombres = sum(actuarios_del_ministerio_publico_hombres, na.rm = T),
            actuarios_del_ministerio_publico_mujeres =sum(actuarios_del_ministerio_publico_mujeres, na.rm = T), 
            peritos_hombres = sum(peritos_hombres, na.rm = T),
            peritos_mujeres = sum(peritos_mujeres, na.rm = T), 
            policias_ministeriales_o_investigadores_o_judiciales_hombres = sum(policias_ministeriales_o_investigadores_o_judiciales_hombres, na.rm = T),
            policias_ministeriales_o_investigadores_o_judiciales_mujeres = sum(policias_ministeriales_o_investigadores_o_judiciales_mujeres, na.rm = T),
            personal_administrativo_y_de_apoyo_hombres = sum(personal_administrativo_y_de_apoyo_hombres, na.rm = T),
            personal_administrativo_y_de_apoyo_mujeres = sum(personal_administrativo_y_de_apoyo_mujeres, na.rm = T),
            otros_hombres = sum(otros_hombres, na.rm = T),
            otros_mujeres = sum(otros_mujeres, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(entidad = c("Federación")) %>% 
  select(entidad, 
         nivel_de_escolaridad, 
         total_personal, 
         fiscales_de_ministerio_publico_hombres, 
         fiscales_de_ministerio_publico_mujeres, 
         agentes_de_ministerio_publico_hombres, 
         agentes_de_ministerio_publico_mujeres,
         secretarios_del_ministerio_publico_hombres, 
         secretarios_del_ministerio_publico_mujeres,
         actuarios_del_ministerio_publico_hombres,
         actuarios_del_ministerio_publico_mujeres,
         peritos_hombres, 
         peritos_mujeres, 
         policias_ministeriales_o_investigadores_o_judiciales_hombres, 
         policias_ministeriales_o_investigadores_o_judiciales_mujeres, 
         personal_administrativo_y_de_apoyo_hombres, 
         personal_administrativo_y_de_apoyo_mujeres, 
         otros_hombres, 
         otros_mujeres)

#Obtenemos los datos totales de la federación

datos_federacion_totales <- datos_federacion_general %>%
  group_by(entidad) %>% 
  summarise(total_personal = sum(total_personal, na.rm = T),
            fiscales_de_ministerio_publico_hombres = sum(fiscales_de_ministerio_publico_hombres, na.rm = T),
            fiscales_de_ministerio_publico_mujeres = sum(fiscales_de_ministerio_publico_mujeres, na.rm = T),
            agentes_de_ministerio_publico_hombres = sum(agentes_de_ministerio_publico_hombres, na.rm = T),
            agentes_de_ministerio_publico_mujeres = sum(agentes_de_ministerio_publico_mujeres, na.rm = T),
            secretarios_del_ministerio_publico_hombres = sum(secretarios_del_ministerio_publico_hombres, na.rm = T),
            secretarios_del_ministerio_publico_mujeres = sum(secretarios_del_ministerio_publico_mujeres, na.rm = T),
            actuarios_del_ministerio_publico_hombres = sum(actuarios_del_ministerio_publico_hombres, na.rm = T),
            actuarios_del_ministerio_publico_mujeres =sum(actuarios_del_ministerio_publico_mujeres, na.rm = T), 
            peritos_hombres = sum(peritos_hombres, na.rm = T),
            peritos_mujeres = sum(peritos_mujeres, na.rm = T), 
            policias_ministeriales_o_investigadores_o_judiciales_hombres = sum(policias_ministeriales_o_investigadores_o_judiciales_hombres, na.rm = T),
            policias_ministeriales_o_investigadores_o_judiciales_mujeres = sum(policias_ministeriales_o_investigadores_o_judiciales_mujeres, na.rm = T),
            personal_administrativo_y_de_apoyo_hombres = sum(personal_administrativo_y_de_apoyo_hombres, na.rm = T),
            personal_administrativo_y_de_apoyo_mujeres = sum(personal_administrativo_y_de_apoyo_mujeres, na.rm = T),
            otros_hombres = sum(otros_hombres, na.rm = T),
            otros_mujeres = sum(otros_mujeres, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(nivel_de_escolaridad = c("Total")) %>% 
  select(entidad, 
         nivel_de_escolaridad, 
         total_personal, 
         fiscales_de_ministerio_publico_hombres, 
         fiscales_de_ministerio_publico_mujeres, 
         agentes_de_ministerio_publico_hombres, 
         agentes_de_ministerio_publico_mujeres,
         secretarios_del_ministerio_publico_hombres, 
         secretarios_del_ministerio_publico_mujeres,
         actuarios_del_ministerio_publico_hombres,
         actuarios_del_ministerio_publico_mujeres,
         peritos_hombres, 
         peritos_mujeres, 
         policias_ministeriales_o_investigadores_o_judiciales_hombres, 
         policias_ministeriales_o_investigadores_o_judiciales_mujeres, 
         personal_administrativo_y_de_apoyo_hombres, 
         personal_administrativo_y_de_apoyo_mujeres, 
         otros_hombres, 
         otros_mujeres)
  

#### Unimos los datos de la federación


datos_federacion <- rbind(datos_federacion_totales, datos_federacion_general)
  

### Obtenemos las estadisticas generales de cada entidad


datos_entidades_generales <- bd_grado_cargo_sexo %>% 
  group_by(entidad, nivel_de_escolaridad) %>% 
  summarise(total_personal = sum(total_personal, na.rm = T),
            fiscales_de_ministerio_publico_hombres = sum(fiscales_de_ministerio_publico_hombres, na.rm = T),
            fiscales_de_ministerio_publico_mujeres = sum(fiscales_de_ministerio_publico_mujeres, na.rm = T),
            agentes_de_ministerio_publico_hombres = sum(agentes_de_ministerio_publico_hombres, na.rm = T),
            agentes_de_ministerio_publico_mujeres = sum(agentes_de_ministerio_publico_mujeres, na.rm = T),
            secretarios_del_ministerio_publico_hombres = sum(secretarios_del_ministerio_publico_hombres, na.rm = T),
            secretarios_del_ministerio_publico_mujeres = sum(secretarios_del_ministerio_publico_mujeres, na.rm = T),
            actuarios_del_ministerio_publico_hombres = sum(actuarios_del_ministerio_publico_hombres, na.rm = T),
            actuarios_del_ministerio_publico_mujeres =sum(actuarios_del_ministerio_publico_mujeres, na.rm = T), 
            peritos_hombres = sum(peritos_hombres, na.rm = T),
            peritos_mujeres = sum(peritos_mujeres, na.rm = T), 
            policias_ministeriales_o_investigadores_o_judiciales_hombres = sum(policias_ministeriales_o_investigadores_o_judiciales_hombres, na.rm = T),
            policias_ministeriales_o_investigadores_o_judiciales_mujeres = sum(policias_ministeriales_o_investigadores_o_judiciales_mujeres, na.rm = T),
            personal_administrativo_y_de_apoyo_hombres = sum(personal_administrativo_y_de_apoyo_hombres, na.rm = T),
            personal_administrativo_y_de_apoyo_mujeres = sum(personal_administrativo_y_de_apoyo_mujeres, na.rm = T),
            otros_hombres = sum(otros_hombres, na.rm = T),
            otros_mujeres = sum(otros_mujeres, na.rm = T)) %>% 
  ungroup()


#### Obtenemos las estadisticas totales de cada entidad

datos_entidades_totales <- datos_entidades_generales %>%
  group_by(entidad) %>% 
  summarise(total_personal = sum(total_personal, na.rm = T),
            fiscales_de_ministerio_publico_hombres = sum(fiscales_de_ministerio_publico_hombres, na.rm = T),
            fiscales_de_ministerio_publico_mujeres = sum(fiscales_de_ministerio_publico_mujeres, na.rm = T),
            agentes_de_ministerio_publico_hombres = sum(agentes_de_ministerio_publico_hombres, na.rm = T),
            agentes_de_ministerio_publico_mujeres = sum(agentes_de_ministerio_publico_mujeres, na.rm = T),
            secretarios_del_ministerio_publico_hombres = sum(secretarios_del_ministerio_publico_hombres, na.rm = T),
            secretarios_del_ministerio_publico_mujeres = sum(secretarios_del_ministerio_publico_mujeres, na.rm = T),
            actuarios_del_ministerio_publico_hombres = sum(actuarios_del_ministerio_publico_hombres, na.rm = T),
            actuarios_del_ministerio_publico_mujeres =sum(actuarios_del_ministerio_publico_mujeres, na.rm = T), 
            peritos_hombres = sum(peritos_hombres, na.rm = T),
            peritos_mujeres = sum(peritos_mujeres, na.rm = T), 
            policias_ministeriales_o_investigadores_o_judiciales_hombres = sum(policias_ministeriales_o_investigadores_o_judiciales_hombres, na.rm = T),
            policias_ministeriales_o_investigadores_o_judiciales_mujeres = sum(policias_ministeriales_o_investigadores_o_judiciales_mujeres, na.rm = T),
            personal_administrativo_y_de_apoyo_hombres = sum(personal_administrativo_y_de_apoyo_hombres, na.rm = T),
            personal_administrativo_y_de_apoyo_mujeres = sum(personal_administrativo_y_de_apoyo_mujeres, na.rm = T),
            otros_hombres = sum(otros_hombres, na.rm = T),
            otros_mujeres = sum(otros_mujeres, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(nivel_de_escolaridad = c("Total")) %>% 
  select(entidad, 
         nivel_de_escolaridad, 
         total_personal, 
         fiscales_de_ministerio_publico_hombres, 
         fiscales_de_ministerio_publico_mujeres, 
         agentes_de_ministerio_publico_hombres, 
         agentes_de_ministerio_publico_mujeres,
         secretarios_del_ministerio_publico_hombres, 
         secretarios_del_ministerio_publico_mujeres,
         actuarios_del_ministerio_publico_hombres,
         actuarios_del_ministerio_publico_mujeres,
         peritos_hombres, 
         peritos_mujeres, 
         policias_ministeriales_o_investigadores_o_judiciales_hombres, 
         policias_ministeriales_o_investigadores_o_judiciales_mujeres, 
         personal_administrativo_y_de_apoyo_hombres, 
         personal_administrativo_y_de_apoyo_mujeres, 
         otros_hombres, 
         otros_mujeres)
  
### Unimos los datos de cada entidad y los ordenamos 


datos_entidades <- rbind(datos_entidades_totales, datos_entidades_generales) %>% 
  group_by(entidad, nivel_de_escolaridad) %>% 
  arrange(entidad) %>% 
  ungroup()

### Unimos todos los datos en un solo tibble

num_personal_ministerios_por_entidad_nivel_de_escolaridad_cargo_y_sexo <- 
  rbind(datos_federacion, datos_entidades)

#### Exportamos la base de datos 

write.csv(num_personal_ministerios_por_entidad_nivel_de_escolaridad_cargo_y_sexo,
  file = "01_datos/03_ejercicio/num_personal_ministerios_por_entidad_nivel_de_escolaridad_cargo_y_sexo.csv")






#### 4. Delitos cometidos por las personas privadas de la libertad en los centros penitenciarios por entidad federativa y tipo de delito según fuero ----

### Obtenemos los datos del fuero comun y federal y  nos quedamos con las variables que nos interesa trabajar

bd_fuero_comun <- read_csv("01_datos/04_ejercicio/bd_fuero_comun.csv", 
                           col_types = cols(tipdelit_c = col_double(), 
                                            ingrestt = col_number(), ingres1 = col_number(), 
                                            ingres2 = col_number(), ingress1 = col_number(), 
                                            ingres3 = col_number(), ingres4 = col_number(), 
                                            ingress2 = col_number(), ingres6 = col_number(), 
                                            ingres7 = col_number())) %>% 
  select(entidad_a, tipdelit_c, ingrestt)


bd_fuero_federal <- read_csv("01_datos/04_ejercicio/bd_fuero_federal.csv", 
                             col_types = cols(ingrestt = col_number()))%>%
  select(entidad_a, tipdelit_c, ingrestt)

### Agregamos los nombres de las entidades y seleccionamos las variables que nos interesan


bd_fuero_comun <- bd_fuero_comun %>%
  mutate(entidad_a = if_else(entidad_a == 1, "Aguascalientes",
                             if_else(entidad_a == 2, "Baja California",
                                     if_else(entidad_a == 3, "Baja California Sur",
                                             if_else(entidad_a==4, "Campeche", 
                                                     if_else(entidad_a==5, "Coahuila",
                                                             if_else(entidad_a==6, "Colima",
                                                                     if_else(entidad_a==7, "Chiapas",
                                                                             if_else(entidad_a==8, "Chihuahua",
                                                                                     if_else(entidad_a==9, "Ciudad de México",
                                                                                             if_else(entidad_a==10, "Durango",
                                                                                                     if_else(entidad_a==11, "Guanajuato",
                                                                                                             if_else(entidad_a==12, "Guerrero",
                                                                                                                     if_else(entidad_a==13, "Hidalgo",
                                                                                                                             if_else(entidad_a==14, "Jalisco",
                                                                                                                                     if_else(entidad_a==15, "Estado de México",
                                                                                                                                             if_else(entidad_a==16, "Michoacán",
                                                                                                                                                     if_else(entidad_a==17, "Morelos",
                                                                                                                                                             if_else(entidad_a==18, "Nayarit",
                                                                                                                                                                     if_else(entidad_a==19, "Nuevo León",
                                                                                                                                                                             if_else(entidad_a==20, "Oaxaca",
                                                                                                                                                                                     if_else(entidad_a==21, "Puebla",
                                                                                                                                                                                             if_else(entidad_a==22, "Querétaro",
                                                                                                                                                                                                     if_else(entidad_a==23, "Quintana Roo",
                                                                                                                                                                                                             if_else(entidad_a==24, "San Luis Potosí",
                                                                                                                                                                                                                     if_else(entidad_a==25, "Sinaloa",
                                                                                                                                                                                                                             if_else(entidad_a==26, "Sonora",
                                                                                                                                                                                                                                     if_else(entidad_a==27, "Tabasco",
                                                                                                                                                                                                                                             if_else(entidad_a==28, "Tamaulipas",
                                                                                                                                                                                                                                                     if_else(entidad_a==29, "Tlaxcala",
                                                                                                                                                                                                                                                             if_else(entidad_a==30, "Veracruz",
                                                                                                                                                                                                                                                                     if_else(entidad_a==31, "Yucatán", "Zacatecas")))))))))))))))))))))))))))))))) %>% 
  rename(entidad = "entidad_a",
         total_delito = "ingrestt",
         tipo_delito = "tipdelit_c")

  

bd_fuero_federal <- bd_fuero_federal %>%
  mutate(entidad_a = if_else(entidad_a == 1, "Aguascalientes",
                             if_else(entidad_a == 2, "Baja California",
                                     if_else(entidad_a == 3, "Baja California Sur",
                                             if_else(entidad_a==4, "Campeche", 
                                                     if_else(entidad_a==5, "Coahuila",
                                                             if_else(entidad_a==6, "Colima",
                                                                     if_else(entidad_a==7, "Chiapas",
                                                                             if_else(entidad_a==8, "Chihuahua",
                                                                                     if_else(entidad_a==9, "Ciudad de México",
                                                                                             if_else(entidad_a==10, "Durango",
                                                                                                     if_else(entidad_a==11, "Guanajuato",
                                                                                                             if_else(entidad_a==12, "Guerrero",
                                                                                                                     if_else(entidad_a==13, "Hidalgo",
                                                                                                                             if_else(entidad_a==14, "Jalisco",
                                                                                                                                     if_else(entidad_a==15, "Estado de México",
                                                                                                                                             if_else(entidad_a==16, "Michoacán",
                                                                                                                                                     if_else(entidad_a==17, "Morelos",
                                                                                                                                                             if_else(entidad_a==18, "Nayarit",
                                                                                                                                                                     if_else(entidad_a==19, "Nuevo León",
                                                                                                                                                                             if_else(entidad_a==20, "Oaxaca",
                                                                                                                                                                                     if_else(entidad_a==21, "Puebla",
                                                                                                                                                                                             if_else(entidad_a==22, "Querétaro",
                                                                                                                                                                                                     if_else(entidad_a==23, "Quintana Roo",
                                                                                                                                                                                                             if_else(entidad_a==24, "San Luis Potosí",
                                                                                                                                                                                                                     if_else(entidad_a==25, "Sinaloa",
                                                                                                                                                                                                                             if_else(entidad_a==26, "Sonora",
                                                                                                                                                                                                                                     if_else(entidad_a==27, "Tabasco",
                                                                                                                                                                                                                                             if_else(entidad_a==28, "Tamaulipas",
                                                                                                                                                                                                                                                     if_else(entidad_a==29, "Tlaxcala",
                                                                                                                                                                                                                                                             if_else(entidad_a==30, "Veracruz",
                                                                                                                                                                                                                                                                     if_else(entidad_a==31, "Yucatán", "Zacatecas")))))))))))))))))))))))))))))))) %>% 
  rename(entidad = "entidad_a",
         total_delito = "ingrestt",
         tipo_delito = "tipdelit_c")

#### FIltramos el código de tipo de delitos para quedarnos con el agregado y no repetir número

cod_delitos <- c(seq(10100, 10500, 100),
seq(20100, 20700, 100),
seq(30100, 30700, 100),
seq(40100, 40800, 100),
seq(50100, 50300, 100),
seq(60100, 60600, 100),
seq(70100, 70900, 100),
seq(80100, 80500, 100),
seq(90100, 91800, 100),
100100, 999999)

bd_fuero_comun <- bd_fuero_comun %>% 
  filter(tipo_delito %in% cod_delitos)

bd_fuero_federal <- bd_fuero_federal %>% 
  filter(tipo_delito %in% cod_delitos)


###Unimos los datos de fuero común y fuero federal
#Nota: Es importante considerar que el siguiente código funciona por la 
#estructura de ambas bases de datos ya que están ordenadas de igual forma y 
#cuentan con el mismo número de observaciones. Por cuestiones de tiempo es la 
#forma que decidí unirlas.


bd_fuero_comun_y_federal <- bd_fuero_comun %>% 
  transmute(entidad,
            tipo_delito,
            total_delito_fuero_comun = total_delito,
            total_delito_fuero_federal = bd_fuero_federal$total_delito)

###Obtenemos el total de delitos por entidad y tipo de delito

bd_fuero_comun_y_federal <- bd_fuero_comun_y_federal %>% 
  transmute(entidad, tipo_delito, 
         total_delitos = total_delito_fuero_comun+total_delito_fuero_federal,
         total_delito_fuero_comun, total_delito_fuero_federal)

### Obtenemos los datos nacionales generales

datos_nacional_general <-  bd_fuero_comun_y_federal %>% 
  group_by(tipo_delito) %>% 
  summarise(total_delitos = sum(total_delitos),
            total_delito_fuero_comun = sum(total_delito_fuero_comun),
            total_delito_fuero_federal = sum(total_delito_fuero_federal)) %>% 
  ungroup() %>% 
  transmute(entidad = c("Nacional"),
            tipo_delito, 
            total_delitos, 
            total_delito_fuero_comun,
            total_delito_fuero_federal)


###Obtenemos los datos nacionales totales

datos_nacional_total <- datos_nacional_general %>% 
  group_by(entidad) %>% 
  summarise(total_delitos = sum(total_delitos),
            total_delito_fuero_comun = sum(total_delito_fuero_comun),
            total_delito_fuero_federal = sum(total_delito_fuero_federal)) %>% 
  mutate(tipo_delito = c("Total")) %>% 
  select(entidad,
         tipo_delito,
         total_delitos, 
         total_delito_fuero_comun,
         total_delito_fuero_federal)

###Unimos los datos nacionales


datos_nacional <- rbind(datos_nacional_total, datos_nacional_general) %>% 
group_by(entidad, tipo_delito) %>% 
  arrange(entidad) %>% 
  ungroup()


###Obtenemos datos totales por entidad


datos_estatal_total <- bd_fuero_comun_y_federal %>% 
  group_by(entidad) %>% 
  summarise(total_delitos = sum(total_delitos),
            total_delito_fuero_comun = sum(total_delito_fuero_comun),
            total_delito_fuero_federal = sum(total_delito_fuero_federal)) %>% 
  mutate(tipo_delito  = c("Total")) %>% 
  select(entidad,
         tipo_delito,
         total_delitos, 
         total_delito_fuero_comun,
         total_delito_fuero_federal)
  
### Unimos los datos estatales
  
datos_estatal <- rbind(datos_estatal_total,bd_fuero_comun_y_federal) %>% 
  group_by(entidad, tipo_delito) %>% 
  arrange(entidad) %>% 
  ungroup()
  

#### Unimos las 2 bases de datos y exportamos 


num_delitos_por_entidad_y_tipo_de_delito_segun_fuero <- rbind(datos_nacional, datos_estatal)

write.csv(num_delitos_por_entidad_y_tipo_de_delito_segun_fuero, file = "01_datos/04_ejercicio/num_delitos_por_entidad_y_tipo_de_delito_segun_fuero.csv")


## Paso extra para agregar los nombres de los delitos 
#Nota: Este paso no está terminado pero tiene la intención de obtener el nombre de asociado a cada código
# para después cambiar en la base de datos final y tener nombres en lugar del código

#Obtenemos el nombre de cada tipo de delito por su codigo
codigos_tipo_delito <- read_excel("01_datos/04_ejercicio/codigos_tipo_delito.xlsx")

#Agregamos los nombres faltantes

a <- c(20600, 
  30400,
  40100,
  40200,
  40500,
  60100,
  60200,
  70100,
  70200,
  70600,
  80100,
  80200,
  90500,
  90700,
  91600)

b <- c("Secuestro",
  "Violación",
  "Robo",
  "Delitos en materia de hidrocarburos y sus derivados",
  "Extorsión",
  "Delitos contra el libre desarrollo de la personalidad",
  "Trata de personas",
  "Delitos contra la salud relacionados con narcóticos en su modalidad de narcomenudeo",
  "Delitos federales contra la salud relacionados con narcóticos",
  "Delitos en materia de armas, explosivos y otros materiales destructivos",
  "Delitos por hechos de corrupción",
  "Delitos contra la administración de justicia",
  "Delitos contra el medio ambiente, el equilibrio ecológico y la gestión ambiental",
  "Delitos en materia de migración",
  "Delitos contra la seguridad de los datos y/o sistemas o equipos informáticos")  

codigos_tipo_delito <- rbind(codigos_tipo_delito, tibble(cod_delito = a, delito = b)) 

codigos_tipo_delito_2 <- codigos_tipo_delito %>% 
  filter(cod_delito %in% cod_delitos) %>%
  arrange(cod_delito) %>% 
  mutate(cod_delito = as.character(cod_delito))

codigos_tipo_delito_2$cod_delito

datos_nacional$tipo_delito[-1]



