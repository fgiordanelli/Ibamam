library(dplyr)


load("C:/Users/Fabrício Giordanelli/Desktop/ALFAPROMOTORA/df_arrec.rda")

# Se quiser simular dados para teste

set.seed(100)
moeda <- sample(c("Real", "Cruzado", "UFIR"), replace = TRUE, size = 100)
valorAuto <- as.integer(sample(c(100:1000), replace = TRUE, size = 100))
df_teste <- cbind.data.frame(moeda, valorAuto)

# função

converter <- function(moeda, valor, x = 1000, y= 2.75, z=1.0641){
  mutate(new_valor = ifelse(moeda == "UFIR",
                            valor * z,
                            ifelse(moeda == "OTN",
                                   (valor * 6.17 * 126.8621)/597.06 * z,
                            ifelse(moeda == "BTN",
                                   valor * 126.8621/597.06 * z,
                            iflese(moeda == "MVR",
                                   valor * 63.49,
                            iflese(moeda == "Cruzeiro (70 a 86)",
                                   valor / y * x^4,
                            iflese(moeda == "Cruzado",
                                   valor / y * x^3,
                            iflese(moeda == "Cruzado Novo",
                                   valor / y * x^2,
                            iflese(moeda == "Cruzeiro (90 a 93)",
                                   valor / y * x^2,
                            iflese(moeda == "Cruzeiro Real",
                                   valor / y * x,
                                   valor
                            )
                            )
                            )
                            )
                            )
                            )
                            )
                            )
                            )
  )
  df <- tibble(valor, moeda, new_valor)
}

# atribuindo objeto para conferir resultado
teste <- converter(moeda = df_arrec$moeda, valor = df_arrec$valorAuto)

# conferindo resultado da funcao
teste %>% View()


# atribuindo objeto para conferir resultado
teste <- converter(moeda = df_teste$moeda, valor = df_teste$valorAuto)
