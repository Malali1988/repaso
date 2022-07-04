
# cargar paquetes ----------------------------------------------------------------



library(readxl)
library(tidyverse)
library(janitor)
library(readr)


# importar ----------------------------------------------------------------

NECROMASA <- read_excel(path = "~/R/AGLOMERADOS_COTOPAXI/base-datos-AC_20-6-22.xlsx",
                        sheet = "NECROMASA") %>% 
  
                        
  
clean_names()

sapply (Necromas_pinus, class)

#cabiar caracter a numerico dentro de un dataframe eligiendo la columna que deseao cambiar -------------------------------------------------------------

Necromas_pinus <- transform(Necromas_pinus, necromas_f_b_18 = as.numeric(necromas_f_b_18), 
                            necromas_f_c_24 = as.numeric(necromas_f_c_24))


#clean Data -------------------------------------------------------------
Necromas_pinus <- NECROMASA %>% 
  filter(genero == "Pinus") %>% 
  select(rodal, plot, genero, contains("necromas")) %>% 
  mutate(necromas_f_b_18 = as.numeric(necromas_f_b_18)) %>% 
  mutate(necromas_f_c_24 = as.numeric(necromas_f_c_24)) %>% 
  pivot_longer(cols = -(rodal:genero), # las variables horizontales las coloca como una variable vertical, variables que son las mismas. tidy
               names_to = "necromasa_punto_muestreo", # para cambiar el nombre de la variable names que aparece por default
               values_to = "peso") %>%  # para cambiar el nombre de la variable values que aparece por default
  mutate(peso = replace_na(peso, 0)) %>% # reeplaza los NA por 0
  mutate(necromasa_punto_muestreo = str_remove(necromasa_punto_muestreo, "necromas_f_"))#permite editar la columna de una variable, pero solo una palabra que se repita en todas
  
  # otro camino para trasformasr de carcter a numérico

Necromas_pinus <- transform(Necromas_pinus, necromas_f_b_18 = as.numeric(necromas_f_b_18), # transfoma las variables de caracter a numerico, primero se coloca el dataframe que va a ser intervenido y luego las variables a intervenir
                            necromas_f_c_24 = as.numeric(necromas_f_c_24)) %>%
pivot_longer(cols = -(rodal:genero), # las variables horizontales las coloca como una variable vertical, variables que son las mismas. tidy
             names_to = "necromasa_punto_muestreo", # para cambiar el nombre de la variable names que aparece por default
             values_to = "peso") %>%  # para cambiar el nombre de la variable values que aparece por default
  mutate(peso = replace_na(peso, 0)) %>% # reeplaza los NA por 0
  mutate(necromasa_punto_muestreo = str_remove(necromasa_punto_muestreo, "necromas_f_"))


Necromas_pinus %>% 
  summarize(total=sum(peso))



# busca NA  ---------------------------------------------------------------


sum(is.na (Necromas_pinus))# para saber si existen NA en mis variables

na_count <-sapply(Necromas_pinus, function(y) sum(length(which(is.na(y)))))# crea una variable con los NA
na_count <- data.frame(na_count)# crea un data frame con las la variable creada anteriormente

#group by() and mutate() ------------------------------------------------


Aglomerados_cotopaxi %>% 
  select (site, plot, nro_plot, scientific_name, dbm) %>% 
  group_by(site) %>% 
  mutate(porcentaje = dbm / sum(dbm)) %>% # calcula la proporcion de una variable numérica, porcentaje es una variable creada por mi
  ungroup() %>%# desagrupa los datos agrupados por group_by  
  slice_max(porcentaje, n = 1)# permite ver el maximo de la variable que elijo



  
  

# buscar Na de agomerados_cotopaxi y tranformar en 0 -----------------------------------------

sum(is.na (Aglomerados_cotopaxi))
na_count <-sapply(Aglomerados_cotopaxi, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
  


# importar datos -----------------------------------------------------


patula <- read_delim("~/R/AGLOMERADOS_COTOPAXI/bases_union/patula_data-2022-06-30.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names() 

radiata <- read_delim("~/R/AGLOMERADOS_COTOPAXI/bases_union/radiata_data-2022-06-30.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names() 
globulus <- read_delim("~/R/AGLOMERADOS_COTOPAXI/bases_union/globulus_data-2022-06-30.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names() 

nitens <- read_delim("~/R/AGLOMERADOS_COTOPAXI/bases_union/nitens_data-2022-06-30.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  clean_names() 

base_general <- read_excel(path = "~/R/AGLOMERADOS_COTOPAXI/bases_union/23-06-22_Coordenadas_R_corregido2 (2).xlsx") %>% 
  clean_names()





# unir bases una sobre otra --------------------------------------------------------------

generos_unido <- bind_rows(patula, 
                           radiata,
                           globulus,
                           nitens)


# unir base general con las bases unidas una sobre otra, de derecha a izquierda--------



base_final <- base_general %>% 
  left_join(generos_unido,
            by = "codigo") %>% 
  select(-x1, -ht, -(hlocal:placa.y))


sum(is.na (base_final))
na_count <-sapply(base_final, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

sum(table(base_final$codigo)-1)#cuenta valores duplicados en una variable
 






# importar base -----------------------------------------------------------


datos2017 <- read_excel("~/R/AGLOMERADOS_COTOPAXI/datos2017.xlsx") %>% 

clean_names() 


datos2022 <- read_excel("~/R/AGLOMERADOS_COTOPAXI/datos2022.xlsx") %>% 

clean_names() 


sum(is.na (datos2022))
na_count <-sapply(datos2022, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

sum(table(datos2022$codigo)-1)#cuenta valores duplicados en una variable


#full join, unir bases sin perdere datos---------------------------------------------------------------


datos_2017_2022 <- datos2022 %>%# base que sera la base a la cual se va añadir otra base
  full_join(datos2017,
            by = "codigo" )  
 # rename(dap2022 = dap_cm) %>% # para reeplazar se coloca primero la variable vieja y luego la variable nueva
  #rename(dap2017 = dap)

# export data -------------------------------------------------------------

  write.csv(datos_2017_2022, "~/R/AGLOMERADOS_COTOPAXI/datos_2017_2022.csv")

  
2+10




