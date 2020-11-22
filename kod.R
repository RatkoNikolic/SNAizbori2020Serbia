#############################################################################
### 1. UCITAVANJE PAKETA I FUNKCIJA ######################################### 
#############################################################################

library(twitteR)
library(rtweet)
library(tidyverse)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(igraph)
library(visNetwork)
library(httpuv)
library(dplyr)
library(p2distance)
library(corrplot)

setwd("ADRESA DIREKTORIJUMA")

source("funkcije.R")

#############################################################################
### 2. POVEZIVANJE SA TW ####################################################
#############################################################################

api_key <- "Niz od 25 karaktera"
api_secret <- "Niz od 50 karaktera"
access_token <- "Niz od 45 karaktera"
access_token_secret <- "Niz od 50 karaktera"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#############################################################################
### 3. SKIDANJE PODATAKA SA TW ##############################################
#############################################################################


### 3.1 EKSTRAKCIJA TVITOVA OSNOVNOG UZORKA ###


#Ucitavanje osnovnog uzorka profila iz osnovni_uzorak.csv
osnovni_uzorak <- read.csv("osnovni_uzorak.csv",
                           header = TRUE,
                           encoding = "UTF-8",
                           stringsAsFactors = F)

osnovni_uzorak <- osnovni_uzorak %>% rename("Ime" = "X.U.FEFF.Ime")

#Ekstrakcija tvitova osnovnog uzorka za period izmedu 21.5.2020. i 21.6.2020.
tvitovi_ou <- ekstrakcija_er(osnovni_uzorak,
                             pocetak = "2020-05-21 00:00:00 CET",
                             kraj = "2020-06-21 23:59:59 CET")

#Proveravanje koji nalozi iz osnovnog uzorka nisu usli u tvitovi_ou
setdiff(osnovni_uzorak$Nalog, unique(tvitovi_ou$screenName)) #svi su tu


#Cuvanje skinutih tvitova u .csv fajlu
write.csv(tvitovi_ou, "tvitovi_ou.csv", row.names = F)

#Ucitavanje tvitova iz .csv fajla
tvitovi_ou <- read.csv("tvitovi_ou.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = F)


### 3.2 EKSTRAKCIJA TVITOVA PROSIRENOG UZORKA ### 


#Pravljenje spiska imena naloga koji su nadprosecno cesto komunicirali sa nalozima iz osnovnog uzorka
prosireni_uzorak <- snowball(tvitovi_ou)


#Ekstrakcija tvitova prosirenog uzorka za period izmedu 21.5.2020. i 21.6.2020.
tvitovi_pu <- ekstrakcija_er(prosireni_uzorak,
                             pocetak = "2020-05-21 00:00:00 CET",
                             kraj = "2020-06-21 23:59:59 CET")

#Proveravanje koji nalozi iz prosirenog uzorka nisu usli u tvitovi_pu
setdiff(prosireni_uzorak, unique(tvitovi_pu$screenName)) #39 naloga iz prosireni_uzorak nije uslo u 
#tvitovi_pu


#Cuvanje tvitovi_pu.csv fajla
write.csv(tvitovi_pu, "tvitovi_pu.csv", row.names = F)

#Ucitavanje tvitovi_pu iz csv fajla
tvitovi_pu <- read.csv("tvitovi_pu.csv",
                       header = TRUE,
                       encoding = "UTF-8",
                       stringsAsFactors = F)


### 3.3 Skidanje dodatnih podataka o nalozima iz osnovnog uzorka (osnovne metrike) ###
metrika_ou <- naloziMeta(osnovni_uzorak$Nalog)


### 3.5 Skidanje podataka o tvitovima osnovnog uzorka u toku posmatranog perioda ###
metrika_tvitovi_ou <- retfav(tvitovi_ou)


### 3.6 Pravljenje tabela za izvestaj (Dodatak 1: Podaci o nalozima osnovnog uzorka 
### i Dodatak 2: Klasicne metrike osnovnog uzorka) ###
tab_ou <- cbind(osnovni_uzorak, metrika_ou["DatumOtvaranja"])
write.csv(tab_ou, "tabela_podaci_ou.csv", row.names = F)

tab_klas_met <- osnovni_uzorak[,c(1,2,4,5,6)]
tab_klas_met <- cbind(tab_klas_met, metrika_ou[,2:5])
tab_klas_met <- cbind(tab_klas_met, metrika_tvitovi_ou[,2:5])

write.csv(tab_klas_met, "tabela_klas_met.csv", row.names = F)

tab_klas_met <- read.csv("tabela_klas_met.csv",
                         header = TRUE,
                         encoding = "UTF-8",
                         stringsAsFactors = F)


### 3.7 Vizualizacija prosecnog retvita po listama
izb <- c(4, 11, 12)

pros_ret_lis <- tab_klas_met[,izb] %>% 
  group_by(Lista) %>% 
  summarize(Pros.Ret = sum(Retvitovan) / sum(Tvitovi.1))

pros_ret_lis <- arrange(pros_ret_lis, desc(pros_ret_lis$Pros.Ret))

ggplot(pros_ret_lis, aes(x = Pros.Ret, y = reorder(Lista, Pros.Ret), fill = Lista)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= round(Pros.Ret, digits = 2)), vjust=0) +
  labs(y= "Lista", x = "Prosecni retvit", title= "Prosecni retvit po listama") +
  theme_bw()


### 3.8 Vizualizacija prosecnog retvita po frakcijama
izb1 <- c(5, 11, 12)

pros_ret_fra <- tab_klas_met[,izb1] %>% 
  group_by(Frakcija) %>% 
  summarize(Pros.Ret = sum(Retvitovan) / sum(Tvitovi.1))

pros_ret_fra <- arrange(pros_ret_fra, desc(pros_ret_fra$Pros.Ret))

ggplot(pros_ret_fra, aes(x = Pros.Ret, y = reorder(Frakcija, Pros.Ret), fill = Frakcija)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= round(Pros.Ret, digits = 2)), vjust=0) +
  labs(y= "Frakcija", x = "Prosecni retvit", title= "Prosecni retvit po frakcijama") +
  theme_bw()


#############################################################################
### 4. PRAVLJENJE MATRICA POVEZANOSTI #######################################
#############################################################################


### Pravljenje matrice poveznosti za osnovni i prosireni uzorak ###

matpov_ou <- matrica_povezanosti(tvitovi_ou)
matpov_pu <- matrica_povezanosti(tvitovi_pu)


#############################################################################
### 5. KREIRANJE GRAFOVA ####################################################
#############################################################################


### 5.1 Sastavljanje igraph objekta od matrica povezanosti frakcija, osnovnog 
### i prosirenog uzorka ###

mreza_ou <- graph.adjacency(matpov_ou, mode="directed", weighted = TRUE)
mreza_pu <- graph.adjacency(matpov_pu, mode="directed", weighted = TRUE)


### 5.2 Dodeljivanje atributa noudovima osnovnog uzorka ###
V(mreza_ou)$ime <- osnovni_uzorak$Ime
V(mreza_ou)$prezime <- osnovni_uzorak$Prezime
V(mreza_ou)$stranka <- osnovni_uzorak$Stranka
V(mreza_ou)$lista <- osnovni_uzorak$Lista
V(mreza_ou)$frakcija <- osnovni_uzorak$Frakcija
V(mreza_ou)$color <- osnovni_uzorak$Boja


#############################################################################
### 6. VIZUALIZACIJA GRAFOVA ################################################
#############################################################################

### 6.1 Vizualizacija mreze osnovnog uzorka pomocu visNetwork

vis_ou <- toVisNetworkData(mreza_ou)
head(vis_ou$nodes)
head(vis_ou$edges)

vis_ou$nodes <- vis_ou$nodes %>% select(-label)
vis_ou$nodes <- vis_ou$nodes %>% rename(group = lista)
vis_ou$nodes <- vis_ou$nodes %>% rename(label = prezime)
vis_ou$nodes$value <- degree(mreza_ou, mode = "total")

vis_ou$edges$arrows <- rep("to", 341)

visNetwork(nodes = vis_ou$nodes, edges = vis_ou$edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(selectedBy = "frakcija") 


### 6.2 Vizualizacija mreze prosirenog uzorka pomocu visNetwork

vis_pu <- toVisNetworkData(mreza_pu)
head(vis_pu$nodes)
head(vis_pu$edges)

for (i in 1: length(vis_pu$nodes$id)) {
  if (!(vis_pu$nodes$id[i] %in% osnovni_uzorak$Nalog)) {
    vis_pu$nodes$frakcija[i] <- 'Ostali_tviterasi'
    vis_pu$nodes$color[i] <- 'gray'
    vis_pu$nodes$uzorak[i] <- 2
  }
  else {
    if (vis_pu$nodes$id[i] %in% osnovni_uzorak$Nalog[osnovni_uzorak$Frakcija == 'Opozicija_bojkot']) {
      vis_pu$nodes$frakcija[i] <- 'Opozicija_bojkot'
      vis_pu$nodes$color[i] <- 'lightblue'
      vis_pu$nodes$uzorak[i] <- 1
    }
    if (vis_pu$nodes$id[i] %in% osnovni_uzorak$Nalog[osnovni_uzorak$Frakcija == 'Opozicija_izbori']) {
      vis_pu$nodes$frakcija[i] <- 'Opozicija_izbori'
      vis_pu$nodes$color[i] <- 'khaki'
      vis_pu$nodes$uzorak[i] <- 1
    }
    if (vis_pu$nodes$id[i] %in% osnovni_uzorak$Nalog[osnovni_uzorak$Frakcija == 'Vlast']) {
      vis_pu$nodes$frakcija[i] <- 'Vlast'
      vis_pu$nodes$color[i] <- 'lightcoral'
      vis_pu$nodes$uzorak[i] <- 1
    }
  }
}  

vis_pu$nodes <- vis_pu$nodes %>% arrange(uzorak)

#Izbacivanje ivica koje ne ukljucuju cvorove osnovnog uzorka 
vis_pu_edges <- vis_pu$edges %>% filter(((vis_pu$edges$from %in% osnovni_uzorak$Nalog) | 
                                          (vis_pu$edges$to %in% osnovni_uzorak$Nalog)))

#Izbacivanje ivica izmedju cvorova osnovnog uzorka
vis_pu_edges <- vis_pu_edges %>% filter(!((vis_pu_edges$from %in% osnovni_uzorak$Nalog) & 
                                           (vis_pu_edges$to %in% osnovni_uzorak$Nalog)))

#Dodeljivanje ivica cvorovima osnovnog uzorka
for (i in 1:length(vis_pu_edges$from)) {
  if (!(vis_pu_edges$from[i] %in% osnovni_uzorak$Nalog)) {
    a <- vis_pu_edges$from[i]
    b <- vis_pu_edges$to[i]
    vis_pu_edges$from[i] <- b
    vis_pu_edges$to[i] <- a
  }
}
  
#Vizualizacija
visNetwork(nodes = vis_pu$nodes, edges = vis_pu_edges, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visOptions(selectedBy = "frakcija") 


#############################################################################
### 7. IZRACUNAVANJE MREZNIH METRIKA ########################################
#############################################################################


### 7.1 Globalne metrike mreze osnovnog i prosirenog uzorka ###

brisanje <- c("metla2020", "dsscentar") #ne pripadaju gigantskom klasteru pa 
#su neke od metrika centralnosti za njih besmislene
mreza_ou_con <- delete.vertices(mreza_ou, brisanje)

#Racunanje globalnih metrika mreza osnovnog uzorka, prosirenog uzorka i prosirenog
#uzorka pojedinacnih frakcija

glob_met_ou <- met_graf(mreza_ou_con)
glob_met_pu <- met_graf(mreza_pu)  

glob_met_ou
glob_met_pu


### 7.2 Metrike centralnosti noudova ###

#Osnovni uzorak

cent_met_ou_con <- met_cent(mreza_ou_con)
cent_met_ou_con
write.csv(cent_met_ou_con, "cent_met_ou_con.csv", row.names = T)


#Prosireni uzorak

cent_met_pu <- met_cent(mreza_pu)
cent_met_ou_in_pu <- cent_met_pu %>% filter(row.names(cent_met_pu) %in% osnovni_uzorak$Nalog)
write.csv(cent_met_ou_in_pu, "cent_met_ou_in_pu.csv", row.names = T)


#############################################################################
### 8. IZRACUNAVANJE KORELACIJE IZMEDJU SVIH METRIKA (MREZNIH I KLASICNIH) ##
#############################################################################

#Pravljenje dejtafrejma koji sadrzi: 1) metrike centralnosti noudova osnovnog
#uzorka u okviru PU; 2) klasicne metrike naloga i 3) metrike naloga za vreme 
#kampanje

cent_met <- cent_met_ou_in_pu
cent_met <- cent_met %>% arrange(rownames(cent_met))

klas_met <- tab_klas_met
klas_met <- klas_met[, 6:13]
rownames(klas_met) <- osnovni_uzorak$Nalog
klas_met <- klas_met %>% arrange(rownames(klas_met))

sve_metrike <- cbind(cent_met, klas_met)
sve_metrike <- sve_metrike %>%
  rename("SviTvitovi" = "Tvitovi",
         "TvitoviKamp" = "Tvitovi.1")

#Vizualizacija matrice korelacija svih pokazatelja
mat_cor_sve1 <- cor(sve_metrike, method = "spearman")
mat_cor_sve <- cor.mtest(sve_metrike, method = "spearman", conf.level = 0.95, exact= FALSE)$p
corrplot(mat_cor_sve1, p.mat= mat_cor_sve, type = "upper", method = "number", sig.level = 0.5)


# Izracunavanje i vizualizacija sveukupne centralnosti prema listama za naloge
# OU u okviru PU
ou_arranged <- osnovni_uzorak %>% arrange(Nalog)

g <- aggregate(
  x = cent_met$P2odstojanje,
  FUN = sum,
  by = list(
    ou_arranged$Lista)
)
g$Broj <- as.numeric(table(ou_con_arranged$Lista))
colnames(g) <- c("Lista", "Centralnost", "Broj")
g$CentStand <- g$Centralnost / g$Broj
g_ord <- g[order(- g$CentStand), ]

ggplot(g_ord, aes(x = CentStand, y = reorder(Lista, CentStand), fill = Lista)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= round(`CentStand`, digits = 2)), vjust=0) +
  ylab("Lista") +
  xlab("Prosecna sveukupna centralnost") +
  theme_bw()


# Izracunavanje i vizualizacija sveukupne centralnosti prema frakcijama za naloge
# OU u okviru PU
f <- aggregate(
  x = cent_met$P2odstojanje,
  FUN = sum,
  by = list(
    ou_arranged$Frakcija)
)
f$Broj <- as.numeric(table(ou_con_arranged$Frakcija))
colnames(f) <- c("Frakcija", "Centralnost", "Broj")
f$CentStand <- f$Centralnost / f$Broj
f_ord <- f[order(- f$CentStand), ]

ggplot(f_ord, aes(x = CentStand, y = reorder(Frakcija, CentStand), fill = Frakcija)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= round(`CentStand`, digits = 2)), vjust=0) +
  ylab("Frakcija") +
  xlab("Prosecna sveukupna centralnost") +
  theme_bw()


#############################################################################
### 9. UTVRDJIVANJE HOMOFILIJE ##############################################
#############################################################################

### Vizualizacija subgrafova OU prema listi i frakciji i racunanje homofilije
#i tranzitivnosti ###

#Sredjivanje igraph objekta ou_hom koji ce biti input
ou_hom <- mreza_ou

V(ou_hom)$group <- V(ou_hom)$lista
V(ou_hom)$shape <- rep(NA, length(V(ou_hom)$name))


#PROVERA
ou_hom_vis <- toVisNetworkData(ou_hom)
ou_hom_vis$nodes #sve je u redu

#Vizualizacija subgrafova po listama

for (i in 1:length(unique(V(ou_hom)$lista))) {
  podgraf <- podgraf_hom_lis(ou_hom, lis = i)
  print(podgraf)
}

#Vizualizacija subgrafova po frakcijama

for (i in 1:length(unique(V(ou_hom)$frakcija))) {
  podgraf <- podgraf_hom_fra(ou_hom, fra = i)
  print(podgraf)
}


#############################################################################
### 10. KLASTER ANALIZA #####################################################
#############################################################################

#Primena "cluster_optimal()" funcije na mrezu osnovnog uzorka

klaster_ou <- cluster_optimal(mreza_ou)
membership(klaster_ou)
modularity(klaster_ou)

#Printanje rezultata

for (i in 1:length(klaster_ou)) {
  print(klaster_ou[i])
}

#Sredjivnaje igraph objekta ou_klast i dodeljivanje clanstva u klasterima
#cvorovima
ou_klast <- mreza_ou

V(ou_klast)$group <- V(ou_klast)$lista

V(ou_klast)$klaster <- klaster_ou$membership

#PROVERA
ou_klast_vis <- toVisNetworkData(ou_klast)
ou_klast_vis$nodes #sve je u redu


#Vizualizacija podele po klasterima
ivice <- ou_klast_vis$edges
ivice$color <- rep("gainsboro", length(ivice$from))

visNetwork(nodes = ou_klast_vis$nodes, edges = ivice, width = "100%", height ="100vh") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(selectedBy = "klaster")


#############################################################################
### KRAJ ####################################################################
#############################################################################
