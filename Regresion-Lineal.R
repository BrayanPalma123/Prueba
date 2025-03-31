library(readxl)
library(ggplot2)

#Importacion de los archivos
df1 <- read_excel("Punto 2.xlsx")
df2 <- read_excel("punto 3-a.xlsx")
df3 <- read_excel("punto 3-b.xlsx")

#Modelos
modelo_punto2 <- lm(df1$`Y(Nosotros)` ~ df1$`X_1(Padre)` + df1$`X_2(Madre)`)  
modelo_punto3_a <- lm(df2$`Y(Nosotros)` ~ df2$`X_1(M-P, F-M)`)  
modelo_punto3_b <- lm(df3$`Y(Nosotros)` ~ df3$`X_1(M-P, F-M)` + df3$`X_2(M-M, F-P)`)

summary(modelo_punto2)
summary(modelo_punto3_a)
summary(modelo_punto3_b)

#Graficas del modelo 1
grafica1 <- ggplot(df1, aes(x = `X_1(Padre)`, y = `Y(Nosotros)`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regresión Lineal: Altura del Padre vs Nosotros")
print(grafica1)

grafica1.2 <- ggplot(df1, aes(x = `X_2(Madre)`, y = `Y(Nosotros)`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión Lineal: Altura de la Madre vs Nosotros")
print(grafica1.2)

# Grafica del modelo 2
grafica2 <- ggplot(df2, aes(x = `X_1(M-P, F-M)`, y = `Y(Nosotros)`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "green") +
  labs(title = "Regresión Lineal: X1 vs Nosotros")
print(grafica2)

#Graficas del modelo 3
grafica3 <- ggplot(df3, aes(x = `X_1(M-P, F-M)`, y = `Y(Nosotros)`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "purple") +
  labs(title = "Regresión Lineal: X1 vs Nosotros")
print(grafica3)

grafica4 <- ggplot(df3, aes(x = `X_2(M-M, F-P)`, y = `Y(Nosotros)`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +
  labs(title = "Regresión Lineal: X2 vs Nosotros")
print(grafica4)




