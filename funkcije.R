#1 ekstrakcija_er()

#Funkcija za prikupljanje tvitova sa vise naloga, tvitovanih u izabranom vremenskom periodu
#Funkcija ekstrakcija() kao ulaz prima spisak naloga, i dva vremenska perioda pocetak i kraj
#vremenskog intervala za koji zelimo tvitove (Galjak, 2017). Ako naidje na privatni profil, 
#ne prekida lup erorom, vec ga samo preskace (er - error resistant).
ekstrakcija_er <- function(uzorak, pocetak = NULL, kraj = NULL) 
{                                                                             
  ekstrakcija <- function(spisak, od = NULL, do = NULL) {                       
    tvitovi <- data.frame()
    for (i in 1:length(spisak)) {
      a <- userTimeline(spisak [i], n = 3200, includeRts = TRUE)
      a <- twListToDF(a)
      if (!is.null(od)) {
        a <- a[a$created > od,]
      }
      if (!is.null(do)) {
        a <- a[a$created < do,]
      }
      tvitovi <- rbind(tvitovi, a)
      if (as.numeric(getCurRateLimitInfo()[39, 3]) < 5) {
        Sys.sleep(60 * as.numeric(getCurRateLimitInfo()[39, 4] - Sys.time()) + 10)
      }
    }
    return(tvitovi)
  }                                                                             
  neuspeh <- c()                                                                
  tw <- data.frame()                                                                                                                      
  for (i in 1:length(uzorak))                                                                       
  {
    if 
    (class(try(b <- ekstrakcija(uzorak[i], od = pocetak, do = kraj), silent = T)) == "try-error")
    {neuspeh <- c(neuspeh, uzorak[i])}
    else
    {b <- ekstrakcija(uzorak[i], od = pocetak, do = kraj)
    tw <- rbind(tw, b)
    }
  }                                                                                                                                                                     
  return(tw)                                                                    
}    


#2 snowball()

#Funkcija snowball uzima podatke sa Tvitera i kao rezultat daje imena naloga orignalnog uzorka,
#i sve one naloge sa kojima su originalni nalozi nadprosecno cesto komunicirali (Galjak, 2017)
snowball <- function(x) {
  sb <- names(sort(table(x$replyToSN)[table(x$replyToSN) > mean(table(x$replyToSN))],
                   decreasing = T))
  sb <- sb[!(sb %in% unique(x$screenName))]
  return(c(unique(x$screenName), sb))
}


#3 naloziMeta()

#Funkcija naloziMeta skida meta podatke o svakom profilu iz definisanog uzorka (Galjak, 2017)
naloziMeta <- function(spisak) {
  poltab <- matrix(nrow = length(spisak), ncol = 6)
  for (i in 1:length(spisak)) {
    a <- getUser(spisak[i])
    poltab[i, 1] <- a$screenName
    poltab[i, 2] <- a$followersCount
    poltab[i, 3] <- a$friendsCount
    poltab[i, 4] <- a$statusesCount
    poltab[i, 5] <- a$favoritesCount
    poltab[i, 6] <- as.character(a$created)
  }
  poltab <- as.data.frame(poltab)
  colnames(poltab) <-
    c("Nalog",
      "Pratioci",
      "Prijatelji",
      "Tvitovi",
      "Favorisao",
      "DatumOtvaranja")
  poltab$Pratioci <- as.numeric(as.character(poltab$Pratioci))
  poltab$Prijatelji <- as.numeric(as.character(poltab$Prijatelji))
  poltab$Tvitovi <- as.numeric(as.character(poltab$Tvitovi))
  poltab$Favorisao <- as.numeric(as.character(poltab$Favorisao))
  poltab$DatumOtvaranja <- as.Date(poltab$DatumOtvaranja)
  return(poltab)
}


#4 retfav()

#Funkcija retfav uzima klasicne Tviter metrike favorisan, retvitovan i tvitovi za
#tvitove tvitovane u toku kampanje za svaki nalog iz definisanog uzorka. (Galjak, 2017)
retfav <- function(tvitovi) {
  sp <- unique(tvitovi$screenName)
  retfav <- matrix(ncol = 4, nrow = length(sp))
  for (i in 1:length(sp)) {
    l <- tvitovi[tvitovi$screenName == sp[i], ]
    retfav[i, 1] <- l$screenName[1]
    retfav[i, 2] <- sum(l[l$isRetweet == F, 3])
    retfav[i, 3] <- sum(l[l$isRetweet == F, 12])
    retfav[i, 4] <- length(l[l$isRetweet == F, 12])
  }
  retfav <- as.data.frame(retfav)
  colnames(retfav) <- c("Nalog", "Favorisan", "Retvitovan", "Tvitovi")
  retfav$Favorisan <- as.numeric(as.character(retfav$Favorisan))
  retfav$Retvitovan <- as.numeric(as.character(retfav$Retvitovan))
  retfav$Tvitovi <- as.numeric(as.character(retfav$Tvitovi))
  retfav$Pros.Ret <- retfav$Retvitovan / retfav$Tvitovi
  return(retfav)
}


#5 matrica_povezanosti()

#Funkcija matrica_povezanosti od liste tvitova pravi matricu povezanosti (Galjak, 2017)
matrica_povezanosti <- function(x) {
  sp <- unique(x$screenName)
  lsp <- length(sp)
  mp <-
    matrix(nrow = lsp, ncol = lsp)
  colnames(mp) <- sp
  row.names(mp) <- sp
  for (i in 1:lsp) {
    l <- x[x$screenName == sp[i], ]
    for (j in 1:lsp) {
      if (j == i) {
        j <- j + 1
      }
      if (j > lsp) {
        break
      }
      k <- x[x$screenName == sp[j], ]
      if (any(grepl(k$screenName[1], l$text))) {
        mp[i, j] <- table(grepl(k$screenName[1], l$text))["TRUE"]
      }
    }
  }
  mp[is.na(mp)] <- 0
  return(mp)
}


#6 met_graf()

# Funkcija met_graf racuna globalne metrike izabrane mreze (Galjak, 2017)
met_graf <- function(graf, atribut=NULL) {
  met_graf <- list(
    "Broj cvorova" = vcount(graf),
    "Broj ivica" = ecount(graf),
    "Gustina" = graph.density(graf),
    "Gustina neusmereni" = graph.density(as.undirected(graf, mode =
                                                         "collapse")),
    "Povezanost" = vertex.connectivity(graf),
    "Povezanost neusmereni" = vertex.connectivity(as.undirected(graf, mode =
                                                                  "collapse")),
    "Dijametar" = diameter(graf, weights = NA),
    "Najudaljeniji" = paste(
      farthest_vertices(graf, weights = NA)$vertices$name[1],
      farthest_vertices(graf, weights = NA)$vertices$name[2],
      sep = " -- "),
    "Prosecna duzina putanje" = average.path.length(graf),
    "Tranzitivnost" = transitivity(graf),
    "Asortativnost stepena" = assortativity.degree(graf),
    "Centralizacija intermedijanosti" = centralization.betweenness(graf,
                                                                   directed = TRUE, normalized = TRUE)$centralization,
    "Centralizacija stepena" = centralization.degree(graf, normalized = TRUE,
                                                     mode = "all")$centralization,
    "Centralizacija bliskosti" = centralization.closeness(graf, mode = "all")$centralization,
    "Centralizacija svojstvenog vektora" = centralization.evcent(graf,
                                                                 directed = TRUE, normalized = TRUE)$centralization
  )
  return(met_graf)
}


#7 met_cent()

#Funkcija met_cent racuna metrike centralnosti za svaki pojedinacni cvor izabrane mreze
#i sortira ih po p2 odstojanju (Galjak, 2017)
#IZMENA: 
require(p2distance)

met_cent <- function(graf) {
  cent <- as.matrix(
    cbind(
      betweenness(graf),
      degree(graf, mode = "in"),
      degree(graf, mode = "out"),
      closeness(as.undirected(graf, mode = "collapse")),
      eigen_centrality(graf)$vector
    )
  )
  colnames(cent) <-
    c(
      "intermedijarnosti",
      "dolaznog stepena",
      "odlaznog stepena",
      "bliskosti",
      "svojstvenog vektora"
    )
  p2d <- p2distance::p2distance(cent, reference_vector_function = min)
  cent <- as.data.frame(cent)
  cent$P2odstojanje <- as.vector(p2d$p2distance)
  print(paste("Redosled pokazatalja: ",p2d$variables_sort))
  return(cent[order(-cent$P2odstojanje), ])
}


#8 met_cent_bezsortiranja()

#Funkcija met_cent koja ne sortira po p2odstojanju
require(p2distance)

met_cent_bezsortiranja <- function(graf) {
  cent <- as.matrix(
    cbind(
      betweenness(graf),
      degree(graf, mode = "in"),
      degree(graf, mode = "out"),
      closeness(as.undirected(graf, mode = "collapse")),
      eigen_centrality(graf)$vector
    )
  )
  colnames(cent) <-
    c(
      "intermedijarnosti",
      "dolaznog stepena",
      "odlaznog stepena",
      "bliskosti",
      "svojstvenog vektora"
    )
  p2d <- p2distance::p2distance(cent, reference_vector_function = min)
  cent <- as.data.frame(cent)
  cent$P2odstojanje <- as.vector(p2d$p2distance)
  print(paste("Redosled pokazatalja: ",p2d$variables_sort))
  return(cent)
}


#9 podgraf_hom_lis()

#Funkcija "podgraf_hom_lis()" kao inpute uzima 1) osnovni graf i 2) broj zeljene 
#liste u okviru tog grafa (koji se moze dobiti pomocu: "unique(V(graf)$lista)")
#i kao autput daje podgraf te liste u kome je cvor sa najvecom intermedijarnoscu
#obelezen kvadratom. Takodje racuna koeficijent nominalne asortativnosti prema
#toj listi i koeficijent tranzitivnosti u okviru podgrafa. U podgraf ce biti
#ukljuceni svi cvorovi odabrane liste koji su medjusobno ostvarili dvosmernu 
#komunikaciju kao i cvorovi drugih lista koji su ostvarili dvosmernu komunikaciju
#sa nekim od cvorova odabrane liste. Graf koji se ubacuje u funkciju mora da bude
#igraph objekat, a cvorovi moraju da imaju atribut "lista", kao i atribut "shape"
#ispunjen "NA" vrednostima. 

podgraf_hom_lis <- function(graf, lis = NULL) {
  subgraf <- subgraph.edges(graf, 
                            as.vector(E(graf)[V(graf)[V(graf)$lista == unique(V(graf)$lista)[lis]] %--%
                                                1:length(V(graf))]))
  subgraf <- as.undirected(subgraf, mode = "mutual")
  subgraf <- delete.vertices(subgraf, v = degree(subgraf)==0)
  asortativnost <- round(assortativity.nominal(subgraf, types = as.factor(V(subgraf)$lista)), 3)
  tranzitivnost <- round(transitivity(subgraf), 3)
  V(subgraf)$shape[which(betweenness(subgraf) == max(betweenness(subgraf)))] <- "square"
  naslov <- str_c(unique(V(graf)$lista)[lis], ": hf = ", asortativnost, "; tr = ", tranzitivnost)
  subgraf_vis <- toVisNetworkData(subgraf)
  visNetwork(nodes = subgraf_vis$nodes, edges = subgraf_vis$edges, width = "100%", height ="100vh", main = naslov) %>%
    visIgraphLayout(layout = "layout_nicely")
}


#10 podgraf_hom_fra()

#Fukcija radi isto sto i "podgraf_hom_lis()", samo za frakcije

podgraf_hom_fra <- function(graf, fra = NULL) {
  subgraf <- subgraph.edges(graf, 
                            as.vector(E(graf)[V(graf)[V(graf)$frakcija == unique(V(graf)$frakcija)[fra]] %--%
                                                1:length(V(graf))]))
  subgraf <- as.undirected(subgraf, mode = "mutual")
  subgraf <- delete.vertices(subgraf, v = degree(subgraf)==0)
  asortativnost <- round(assortativity.nominal(subgraf, types = as.factor(V(subgraf)$frakcija)), 3)
  tranzitivnost <- round(transitivity(subgraf), 3)
  V(subgraf)$shape[which(betweenness(subgraf) == max(betweenness(subgraf)))] <- "square"
  naslov <- str_c(unique(V(graf)$frakcija)[fra], ": hf = ", asortativnost, "; tr = ", tranzitivnost)
  subgraf_vis <- toVisNetworkData(subgraf)
  visNetwork(nodes = subgraf_vis$nodes, edges = subgraf_vis$edges, width = "100%", height ="100vh", main = naslov) %>%
    visIgraphLayout(layout = "layout_nicely")
}
