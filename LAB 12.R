####################
#                  #
# Copiar todo esto #
#                  #
####################
# Hecho con gusto por Carla Carolina Pérez Hernández (UAEH)

# video 05 - DENSIDAD DEL RELACIONAMIENTO: CASO HIPOTÉTICO 

# Objetivo: Estimar la densidad del relacionamiento (rd)
# La densidad varia entre 0 y 1, los valores más grandes indican que la región ha avanzado a
# una VCR en muchos bienes proximos al bien i y tiene mayor probabilidad de exportarlo en el futuro
# mayor densidad, mayor cercanía. Menor densidad, mayor lejanía.
# --------------------------------------------------------------------------------------------------------------------
# En este laboratorio vamos a:
# 1. Cargar una matriz hipotética de datos
# 2. Estimar la VCR de la matriz hipotética
# 3. Calcular las co-ocurrencias y el relacionamiento (espacio-producto)
# 4. Estimar la densidad del relacionamiento (rd)
#Buscamos saber si las industrias que estan relacionadas estan presentes en las regiones o no. Nivel de cercanía
# 5. Predecir la entrada de una nueva industria
#Econometría usar count(densidad del relacionamiento) como regresora (predictora) de la entrada de una nueva industria

####################################################
# LABORATORIO 5: densidad del relacionamiento (rd) #
####################################################

library (EconGeo)

# 1.Cómo se mide la densidad del relacionamiento (rd)?
# Paquete: Balland, P.A. (2017) Economic Geography in R: Introduction to the EconGeo Package, Papers in Evolutionary Economic Geography, 17 (09): 1-75 
# Para instalar: https://www.paballand.com/install-r

?RCA
??RCA
#Usa la documentación para generar "mat" y crear una matriz con RCA
#copia los datos de las utilidades a la consola ##generate

## generate a region - industry matrix
set.seed(31)
mat <- matrix(sample(0:100, 20, replace = TRUE), ncol = 4)
rownames(mat) <- c("R1", "R2", "R3", "R4", "R5")
colnames(mat) <- c("I1", "I2", "I3", "I4")

mat

dim(mat)


install.packages("RCA")

mat=rca(mat,binary=T)
mat
# ve la nueva matriz binaria de RCA
#correr el RCA de la matriz original (rca básicamente es un indicador de especialización,
#produce mas de lo esperado basado en el tamaño de la región y de la industria)






# calcula la matriz de co-ocurrencia de la mat transpuesta
c=co_occurrence(t(mat))
c

# calcula el relacionamiento con base en las co-ocurrencias
r=relatedness(c)
r



# Densidad del relacionamiento
r[r<1]=0
r[r>1]=1

r
#el relacionamiento es el espacio producto

# Asegurate que hayas computado la matriz binaria de RCA antes de computar el Relacionamiento-densidad
# Se usa la matriz de VCR Y el espacio producto (r)

mat





# Rango 0-100

rd=relatedness_density(mat,r)
rd


# la industria 1 (I1) esta relacionada con la industria 2 y 3 (I2, I3) (I1)R = 2 (I2;I3)
# Y la región 1 (R1) ya tiene la industria 2 (I2), pero no tiene la industria 3 (I3)
# HAY 2 INDUSTRIAS RELACIONADAS CON LA INDUSTRIA 1, POR ESO 2
# PERO LA REGIÓN 1 SÓLO TIENE 1 DE ESAS INDUSTRIAS RELACIONADAS
# ENTONCES 1/2=0.5




#para econometría

rd=get_list(rd)
rd


# 6. Predecir entrada
??entry.list 


#Mira la sección de ejemplos. Copia-pega los datos a la consola
# matrices en 2 momentos en el tiempo

## generate a first region - industry matrix in which cells represent the presence/absence
## of a RCA (period 1)
set.seed(31)
mat1 <- matrix(sample(0:1, 20, replace = TRUE), ncol = 4)
rownames(mat1) <- c("R1", "R2", "R3", "R4", "R5")
colnames(mat1) <- c("I1", "I2", "I3", "I4")

mat1

## generate a second region - industry matrix in which cells represent the presence/absence
## of a RCA (period 2)
mat2 <- mat1
mat2[3, 1] <- 1

mat2

## generate a first region - industry matrix in which cells represent the presence/absence
## of a RCA (period 1)


## generate a second region - industry matrix in which cells represent the presence/absence
## of a RCA (period 2)




#ver que las posibilidades de entrada es cuando tienes un 0 de VCR
#cuando tienes 1 puedes: permanecer o salir, pero no entrar
# NA, no puede entrar
# 1 tiene potencial de entrada y entró
# 0 puede tener potencial de entrada, pero no ha entrado

d=entry_list(mat1,mat2)

d

colnames(d)=c("Region","Industry","Entry", "Period")
d=merge(d, rd, by = c("Region","Industry"))
d


#el 0 significa que en t1 y t2 hay cero
NA quiere decir que en t1 hay 1 al igual que en t2
#la variable entrada


#abre d para identificar que se busca explicar que tanto la "densidad del relacionamiento" (x) impacta la entrada (y)

summary(lm(d$Entry~d$Count))
#abre rd que es la "densidad del relacionamiento" como regresora (variable explicativa-x-) de la "entrada" (variable explicada-y-)





# lm(dependiente~independiente1+independiente2, data=datos)
# que tanto la "densidad del relacionamiento" impacta la entrada




#exportar resultados para cytoscape o gephi
write.csv(d, file="Resultados")
