# Load libraries
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)
library(dplyr)

#observamos nuestro directorio de trabajo
getwd()
# Establecemos carpeta de trabajo
setwd("C:\\Users\\VICTOR\\Documents\\Curso de Rstudio\\Curso de Rstudio Básico\\BASES DE DATOS")

base<- read_delim("german_credit_data.txt",
                  col_names= TRUE,
                  delim= ',',
                  col_types= cols(Sexo= col_factor(levels= NULL),
                                  Resultado= col_factor(levels= NULL)),
                  na= c("NA", "#N/A", ".", "NaN"))

base %>% 
  transmute(Sexo_cat= if_else(Sexo=="0","Hombre","Mujer")) 


base %>% 
  transmute(P_Credito= Monto_Credito/sum(Monto_Credito)) %>% 
  View()   # % De deuda que representa cada cliente para la institución


base %>% 
  mutate(Cliente= if_else(Cuenta_Ahorro %in% c("intermedio","bajo","muy_alto","alto"), 
                          "cliente", "externo")) %>% 
  view()

base %>% 
  summarise(cuenta= n())   # Tibble con nÃºmero de filas de la base

# Histograma por sexo y monto a financiar 
ggplot(base)+
  geom_histogram(mapping = aes(x=Monto_Credito, fill= Sexo),
                 alpha= 0.4)

#Boxplot
ggplot(base) +
  geom_boxplot(mapping = aes(x=Resultado, y=Monto_Credito, fill=Sexo)) + 
  scale_fill_manual(values=c("#B0A8B9","#4E8397"))

#densidad
ggplot(base) + geom_density(mapping= aes(x=Edad), fill="#F9F871")

# Validación de NAs
for (i in colnames(base)) {
  nulos<- sum(is.na(base[,i]))
  print( paste0("La columna ", i, " tiene ", nulos, " nulos") )
}

## Segmentamos las variables en dos grupos, categóricas y numéricas
#CATEGORICA
i = 0
var_cat<- c() # Vector vacío
for(i in colnames(base)){
  if( class(data.frame(base)[,i]) %in% c("factor", "character")  ){
    var_cat<- c(var_cat, i)
  }
}

var_cat

#NUMERICA


var_num<- c() # Vector vacío
i = 0
for(i in colnames(base)){
  if( (class(data.frame(base)[,i]) %in% c("numeric", "integer"))  ){
    var_num<- c(var_num, i)
  }
}

var_num

#El análisis bivariado analiza dos conjuntos de 
#datos emparejados, estudiando si existe una relación entre ellos

library(GGally)

#grafica de correlaciones de las variables numericas

base %>% 
  select(var_num) %>% 
  ggpairs(lower = list(continuous= "points", combo= "facehist"),
          diag= list(continuous= "bar", comobo= "facehist") )

base %>% select(c(var_num, "Sexo")) %>%
  ggpairs(lower=list(continuous="points", combo="facethist"),
          diag = list(continuous="bar", combo="facethist"))


# Normalizacion
MontoNormalizado <-base %>% 
  mutate(edad_media= mean(Edad), edad_desv= sd(Edad), edad= (Edad-edad_media)/edad_desv) %>% 
  mutate(Monto_Credito_media= mean(Monto_Credito), Monto_Credito_desv= sd(Monto_Credito), Monto_Credito= (Monto_Credito-Monto_Credito_media)/Monto_Credito_desv) 

MontoNormalizado <- as.data.frame(scale(base$Monto_Credito))

# Datos originales
p1 <- ggplot(base, aes(x=Monto_Credito, y=Edad)) +
  geom_point() +
  labs(title="Data originial") +
  theme_bw()

# Datos normalizados
p2 <- ggplot(MontoNormalizado, aes(x=Monto_Credito, y=Edad)) +
  geom_point() +
  labs(title="Data normalizada") +
  theme_bw()


# Ejecucion de k-means con k=2
set.seed(1234)
Base2 <- kmeans(MontoNormalizado, centers=2)

# Cluster que se le asigna a cada punto
Base2$cluster

# Centros de clusters
Base2$centers

# Tamaño de cluster
Base2$size

# Suma de cuadrados entre grupos
Base2$betweenss

# Suma de cuadrdados entre grupos
Base2$withinss

# Suma total de cuadrados dentro del conglomerado 
Base2$tot.withinss

# Suma total de cuadrados
Base2$totss

# ¿Cuantos Clusters?

bss <- numeric()
wss <- numeric()

# Ejecutar el algoritmo para diferente valores de k
set.seed(1234)

for(i in 1:10){
  
  # Para cada k calcule entre tot.withins
  bss[i] <- kmeans(MontoNormalizado, centers=i)$betweenss
  wss[i] <- kmeans(MontoNormalizado, centers=i)$tot.withinss
  
}

# Suma de cuadrados entre grupos vs Eleccion de K
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Suma de cuadrados total dentro del conglomerado vs Eleccion de K
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# grafica
grid.arrange(p3, p4, ncol=2)

# Resultados con k=2
set.seed(1234)

Base3 <- kmeans(MontoNormalizado, centers=2)

#Valores medios de cada conglomerado
aggregate(base, by=list(Base3$cluster), mean)

# Clustering 
ggpairs(cbind(base, Cluster=as.factor(Base3$cluster)),
        columns=1:10, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()