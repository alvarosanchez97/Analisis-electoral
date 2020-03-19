
########################
###2ª BARÓMETRO CEO ###
######################

setwd("~/Desktop/Trabajo Lluis/2ª 2019 CEO")
ceo2 <- read_sav("Microdades anonimitzades -942.sav")

########################################### 

# -> LIMPIEZA DE VARIABLES

# 1. IDEOLOGÍA DE LOS PARTIDOS (ideol_[parti])

  # a. Ciudadanos
ceo2$ideol_cs <- ceo2$P30_Cs
ceo2$ideol_cs[ceo2$ideol_cs>10] <- NA
summary(ceo2$ideol_cs)

  # b. Junts per Cat
ceo2$ideol_jxc <- ceo2$P30_JXCAT
ceo2$ideol_jxc[ceo2$ideol_jxc>10] <- NA
summary(ceo2$ideol_jxc)

  # c. Esquerra
ceo2$ideol_erc <- ceo2$P30_ERC
ceo2$ideol_erc[ceo2$ideol_erc>10] <- NA
summary(ceo2$ideol_erc)

  # d. Socialistas
ceo2$ideol_psc <- ceo2$P30_PSC
ceo2$ideol_psc[ceo2$ideol_psc>10] <- NA
summary(ceo2$ideol_psc)

  # e. Cataluña en Comú Podem
ceo2$ideol_podem <- ceo2$P30_PODEMOS
ceo2$ideol_podem[ceo2$ideol_podem>10] <- NA
summary(ceo2$ideol_podem)

  # f. Candidaturas de la Unidad Popular
ceo2$ideol_cup <- ceo2$P30_CUP
ceo2$ideol_cup[ceo2$ideol_cup>10] <- NA
summary(ceo2$ideol_cup)

  # g. Partido Popular
ceo2$ideol_pp <- ceo2$P30_PPC
ceo2$ideol_pp[ceo2$ideol_pp>10] <- NA
summary(ceo2$ideol_pp)

  # h. VOX
ceo2$ideol_vox <- ceo2$P30_VOX
ceo2$ideol_vox[ceo2$ideol_vox>10] <- NA
summary(ceo2$ideol_vox)

# 2. ESCALA IDEOLÓGICA (e_ideol)
ceo2$e_ideol <- ceo2$P29
ceo2$e_ideol[ceo2$e_ideol>10]<-NA
summary(ceo2$e_ideol)


# 3. RECUERDO DE VOTO (rec_voto)
ceo2$rec_voto <- ceo2$P39R
ceo2$rec_voto[ceo2$rec_voto>22]<-NA
ceo2$rec_voto <- recode(ceo2$rec_voto, "c(21)=2; c(22)=5; c(10)=7")
ceo2$rec_voto <- factor(ceo2$rec_voto,
                        levels = c(1,2,3,4,5,6,7),
                        labels = c("PP", "JxC", "ERC","PSC", "CeCP", "Cs", "CUP"))
table(ceo2$rec_voto)


# 4. CATALANISMO POR PARTIDO (cata_[partido])

  # a. Ciudadanos
ceo2$cata_cs <- ceo2$P32_Cs
ceo2$cata_cs[ceo2$cata_cs>10] <- NA
summary(ceo2$cata_cs) # <- MEAN: 1.82

  # b. JxC
ceo2$cata_jxc <- ceo2$P32_JXCAT
ceo2$cata_jxc[ceo2$cata_jxc>10] <- NA
summary(ceo2$cata_jxc) # <- MEAN: 7.83

  # c. ERC
ceo2$cata_erc <- ceo2$P32_ERC
ceo2$cata_erc[ceo2$cata_erc>10] <- NA
summary(ceo2$cata_erc) # <- MEAN: 8.38

  # d. PSC
ceo2$cata_psc <- ceo2$P32_PSC
ceo2$cata_psc[ceo2$cata_psc>10] <- NA
summary(ceo2$cata_psc) # <- MEAN: 4.13

  # e. CeCP
ceo2$cata_podem <- ceo2$P32_PODEMOS
ceo2$cata_podem[ceo2$cata_podem>10] <- NA
summary(ceo2$cata_podem) # <- MEAN: 4.65

  # f. Candidaturas de la Unidad Popular
ceo2$cata_cup <- ceo2$P32_CUP
ceo2$cata_cup[ceo2$cata_cup>10] <- NA
summary(ceo2$cata_cup) # <- MEAN: 8.55

  # g. Partido Popular
ceo2$cata_pp <- ceo2$P32_PPC
ceo2$cata_pp[ceo2$cata_pp>10] <- NA
summary(ceo2$cata_pp) # <- MEAN: 1.39

  # h. VOX
ceo2$cata_vox <- ceo2$P32_VOX
ceo2$cata_vox[ceo2$cata_vox>10] <- NA
summary(ceo2$cata_vox) # <- MEAN: 0.44

# 6. ESCALA CATALANISMO (e_cata) <- MEAN: 6.11
ceo2$e_cata <- ceo2$P31
ceo2$e_cata[ceo2$e_cata>10]<- NA
summary(ceo2$e_cata)

########################################### 

# -> GRÁFICOS

  # DISTRIBUCIÓN IDEOLÍGICA DE LOS PARTIDOS
boxplot(ceo2$ideol_cs, ceo2$ideol_jxc, ceo2$ideol_erc, ceo2$ideol_psc, ceo2$ideol_podem, ceo2$ideol_cup,
        ceo2$ideol_pp, ceo2$ideol_vox, ceo2$e_ideol,
        names = c("Cs", "JxC", "ERC", "PSC", "CatECP", "CUP", "PP", "VOX", "MEDIA"), 
        col = c("orange", "pink", "yellow", "red", "purple", "black", "blue", "green", "white"), 
        border = c("black"),
        main = "Percepción del posicionamiento ideológico por partido",
        xlab = "Partidos",
        ylab = "Ideología")

  # DISTRIBUCIÓN IDEOLÍGICA ELECTORES
table(ceo2$e_ideol)
summary(ceo2$e_ideol)
hist(ceo2$e_ideol, 
     main = "Distribución ideológica por valores",
     sub = "Media = 3.76 / Mediana = 4 / Moda = 5",
     cex.sub = 1,
     xlab = "Ideología",
     ylab = "Frecuencia",
     col = c("grey", "grey", "grey", "grey", "orange", "grey"),
     ylim = c(0,500),
     xlim = c(0,5))

barplot(table(ceo2$e_ideol),main = "Distribución ideológica por valores",
        sub = "Media = 3.76 / Mediana = 4 / Moda = 5",
        cex.sub = 1,
        xlab = "Ideología",
        ylab = "Frecuencia",
        col = c("grey", "grey", "grey", "grey", "grey", "orange"),
        ylim = c(0,500),
        xlim = c(0,12))

# DISTRIBUCIÓN IDEOLÓGICA VOTANTES POR VALORES

ideol <- table(ceo2$rec_voto, ceo2$e_ideol)
ideol

ideol.2 <- prop.table(ideol, 1)*100
ideol.2

barplot(ideol.2,
        main = "Distribución ideológica y recuerdo de voto (2017)",
        col = c("blue", "pink", "yellow", "red", "purple", "orange", "black"),
        sub = "% de votantes del partido por valor ideológico",
        xlab = "Escala ideológica",
        ylab = "Porcentaje",
        ylim = c(0,200))

legend("topright", inset=.02,
       c("PP", "JxC", "ERC", "PSC", "CeCP", "Cs", "CUP"), fill= c("blue", "pink", "yellow", "red",
      "purple", "orange", "black"), horiz=FALSE, cex=0.6)

# EJE IDEOLÓGICO CATALANISMO

partidos <- c("Cs", "JxC", "ERC","PSC", "CeCP", "CUP", "PP", "VOX")
ideologia <- c(7.91, 4.86, 2.89, 5.07, 3.43, 1.8, 8.37, 9.37)
catalanismo <- c(1.82, 7.83, 8.38, 4.13, 4.65, 8.55, 1.39, 0.44)
eje <- data.frame(partidos, catalanismo, ideologia)

ggplot(eje, aes(x = catalanismo, y = ideologia)) +
  geom_point(pch = c(15,16,16,15,15,16,15,15), size = 6, color = c("orange", "pink", "yellow", "red", "purple", "black", "blue", "darkgreen")) +
  labs(title = "Eje ideológico - catalanista (nacionalista)", x = "Catalanismo (nacionalismo)", y = "Ideología", caption = "Fuente: CEO (Segunda oleada 2019)") +
  geom_vline(xintercept = 6.11, color = "red") + geom_hline(yintercept = 3.76, color = "red") + xlim(0,10) + ylim(0,10)

ggplot(eje, aes(x = ideologia, y = catalanismo)) +
  geom_point(pch = c(15,16,16,15,15,16,15,15), size = 8, color = c("orange", "pink", "yellow", "red", "purple", "black", "blue", "darkgreen")) +
  labs(title = "Eje ideológico - catalanista (nacionalista)", subtitle = "En rojo; media ideológica: 3.76 / media catalanista: 6.11", x = "Ideología", y = "Catalanismo (nacionalismo)", caption = "Fuente: CEO (Segunda oleada 2019)") +
  geom_vline(xintercept = 3.76, color = "red") + geom_hline(yintercept = 6.11, color = "red") + xlim(0,10) + ylim(0,10)

