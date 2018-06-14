library(magrittr)
naccho <- read.csv(file.choose(), header = T, stringsAsFactors = F)
View(naccho)

naccho_remove <- naccho[(naccho$Notes != "")|naccho$nacchoid == naccho$link.to,]
naccho_net <- naccho[!((naccho$Notes != "")|naccho$nacchoid == naccho$link.to),]
unique(naccho_net$state)

naccho_net[which(naccho_net$state == "Mo"),"state"] <- "MO"
naccho_net[which(naccho_net$state == "mo"),"state"] <- "MO"
naccho_net[which(naccho_net$state == "Fl"),"state"] <- "FL"

dim(naccho_net)
rownames(naccho_net) <- 1:5818
naccho_net[which(naccho_net$state == ""),]


naccho_net[756, "state"] <- "FL"
naccho_net[2503, "state"] <- "MI"
naccho_net[2709, "state"] <- "MN"
naccho_net[2946, "state"] <- "MO"
naccho_net[2950, "state"] <- "MO"
naccho_net[3581, "state"] <- "ND"
naccho_net[5248, "state"] <- "WA"
naccho_net[5525, "state"] <- "WI"

unique(naccho_net$state) %>% length()   #HI, RI missing

naccho_net[which(naccho_net$state == "Ky"), "state"] <- "KY"


# how many departments reported
unique(naccho_net$nacchoid) %>% length() #1347


# import attributes data
prevention <- read.csv(file.choose(), header = T, stringsAsFactors = F)
non_prevention <- read.csv(file.choose(), header = T, stringsAsFactors = F)



infor <- unique(c(naccho_net$nacchoid,naccho_net$link.to))
infor <- as.data.frame(infor)
infor_merge <- merge(infor, prevention, by.x = "infor", by.y ="nacchoid", all.x = T)
infor_merge <- merge(infor_merge, non_prevention, by.x = "infor", by.y ="nacchoid", all.x = T)
infor_merge <- infor_merge[, c("infor","c6q93a", "c6q90a", "c0state", "c0jurisdiction", "c0zipruca_ersdicot", "c0population", "tenure", "c5q37")]

dim(na.omit(infor_merge)) # 1397 remain
infor_full <- na.omit(infor_merge)
apply(infor_full, 2, function(x) sum(is.na(x)))



rownames(infor_full) <- 1:1397
#recode continuous variables into categorical
infor_full$pop_cat[infor_full$c0population < 25000] <- "<25000"
infor_full$pop_cat[infor_full$c0population >= 25000 & infor_full$c0population < 49999] <- "25000 - 49999"
infor_full$pop_cat[infor_full$c0population >= 50000 & infor_full$c0population < 99999] <- "50000 - 99999"
infor_full$pop_cat[infor_full$c0population >= 100000 & infor_full$c0population < 499999] <- "100000 - 499999"
infor_full$pop_cat[infor_full$c0population >= 500000] <- ">=500000"
infor_full$pop_cat <- as.factor(infor_full$pop_cat)
levels(infor_full$pop_cat)
infor_full$pop_cat <- factor(infor_full$pop_cat, levels = c("<25000","25000 - 49999","50000 - 99999","100000 - 499999",">=500000"))


infor_full$tenure_cat[infor_full$tenure <= 2] <- "0 - 2"
infor_full$tenure_cat[infor_full$tenure > 2 & infor_full$tenure <= 5] <- "3 - 5"
infor_full$tenure_cat[infor_full$tenure > 5 & infor_full$tenure <= 10] <- "6 - 10"
infor_full$tenure_cat[infor_full$tenure > 10] <- ">= 11"
infor_full$tenure_cat <- as.factor(infor_full$tenure_cat)
levels(infor_full$tenure_cat)
infor_full$tenure_cat <- factor(infor_full$tenure_cat, levels = c("0 - 2","3 - 5","6 - 10",">= 11"))

infor_full$FTE_cat[infor_full$c5q37 > 0 & infor_full$c5q37 < 25] <- "0 - 25"
infor_full$FTE_cat[infor_full$c5q37 >= 25 & infor_full$c5q37 < 50] <- "25.01 - 50"
infor_full$FTE_cat[infor_full$c5q37 >= 50 & infor_full$c5q37 < 100] <- "50.01 - 100"
infor_full$FTE_cat[infor_full$c5q37 >=100] <- ">=100"
infor_full$FTE_cat <- as.factor(infor_full$FTE_cat)
levels(infor_full$FTE_cat)
infor_full$FTE_cat <- factor(infor_full$FTE_cat, levels = c("0 - 25","25.01 - 50","50.01 - 100",">=100"))



keep_naccho <- naccho_net[(naccho_net$nacchoid %in% infor_full$infor) & (naccho_net$link.to %in% infor_full$infor),]
dim(keep_naccho)
rownames(keep_naccho) <- 1:3595




e1 <- as.matrix(keep_naccho) # coerces the data into a two-column matrix format that igraph likes
e2 <- e1[, c(1,3)]
e2[,1] <- as.character(e2[,1])
e2[,2] <- as.character(e2[,2])
library(intergraph)
library(igraph)

naccho_net1 <- graph.edgelist(e2,directed=T)

V(naccho_net1)$name %>% length()

# add attributes


# V(naccho_net1)$nutrition=as.character(infor_full$c6q90a[match(V(naccho_net1)$name,infor_full$infor)])
# V(naccho_net1)$tobacco=as.character(infor_full$c6q93a[match(V(naccho_net1)$name,infor_full$infor)])
# 
# 
# V(naccho_net1)$state=as.character(infor_full$c0state[match(V(naccho_net1)$name,infor_full$infor)])
# V(naccho_net1)$jurisdiction=as.character(infor_full$c0jurisdiction[match(V(naccho_net1)$name,infor_full$infor)])
# V(naccho_net1)$location=as.character(infor_full$c0zipruca_ersdicot[match(V(naccho_net1)$name,infor_full$infor)])
# V(naccho_net1)$population=as.array(infor_full$c0population[match(V(naccho_net1)$name,infor_full$infor)])
# V(naccho_net1)$tenure=as.array(infor_full$tenure[match(V(naccho_net1)$name,infor_full$infor)])
# V(naccho_net1)$FTE=as.array(infor_full$c5q37[match(V(naccho_net1)$name,infor_full$infor)])

V(naccho_net1)$nutrition=as.character(infor_full$c6q90a[match(V(naccho_net1)$name,infor_full$infor)])
V(naccho_net1)$tobacco=as.character(infor_full$c6q93a[match(V(naccho_net1)$name,infor_full$infor)])


V(naccho_net1)$state=as.character(infor_full$c0state[match(V(naccho_net1)$name,infor_full$infor)])
V(naccho_net1)$jurisdiction=as.character(infor_full$c0jurisdiction[match(V(naccho_net1)$name,infor_full$infor)])
V(naccho_net1)$location=as.character(infor_full$c0zipruca_ersdicot[match(V(naccho_net1)$name,infor_full$infor)])
V(naccho_net1)$population=as.character(infor_full$pop_cat[match(V(naccho_net1)$name,infor_full$infor)])
V(naccho_net1)$tenure=as.character(infor_full$tenure_cat[match(V(naccho_net1)$name,infor_full$infor)])
V(naccho_net1)$FTE=as.character(infor_full$FTE_cat[match(V(naccho_net1)$name,infor_full$infor)])



summary(naccho_net1)


# check missing
apply(prevention, 2, function(x) sum(is.na(x)))
apply(non_prevention, 2, function(x) sum(is.na(x)))

library(statnet)
library(intergraph)
naccho_net2 <- asNetwork(naccho_net1)   
gden(naccho_net2)
summary(naccho_net2, print.adj = F)

library(ergm)
model_null <- ergm(naccho_net2 ~ edges, control = control.ergm(seed = 1))
summary(model_null)

# model_1 <- ergm(naccho_net2 ~ edges + nodecov("population") + nodecov("FTE") + nodecov("tenure") + nodefactor("jurisdiction") + nodefactor("nutrition") + nodefactor("tobacco"), control = control.ergm(seed = 1))
model_2 <- ergm(naccho_net2 ~ edges +  asymmetric("state") + asymmetric("population") + asymmetric("FTE") + asymmetric("tenure") + 
                  asymmetric("jurisdiction") +  asymmetric("nutrition") +  asymmetric("tobacco"), control = control.ergm(seed = 1))
summary(model_2)

model_3 <- ergm(naccho_net2 ~ edges + asymmetric("state") + asymmetric("population") + asymmetric("FTE") + asymmetric("tenure") + 
                  asymmetric("jurisdiction") +  asymmetric("nutrition") +  asymmetric("tobacco") + mutual("tenure")+ mutual("population")+
                  mutual("FTE") + mutual("state") + mutual("jurisdiction")+ mutual("nutrition") + mutual("tobacco"),
                control = control.ergm(seed = 1))
summary(model_3)

model_4 <- ergm(naccho_net2 ~ edges + asymmetric("state") + asymmetric("population") + asymmetric("FTE") + asymmetric("tenure") + 
                  asymmetric("jurisdiction") +  asymmetric("nutrition") +  asymmetric("tobacco") + mutual("tenure")+ mutual("population")+
                  mutual("FTE") + mutual("state") + mutual("jurisdiction")+ mutual("nutrition") + mutual("tobacco")+
                  gwesp(.2, T),
                control = control.ergm(seed = 1))
summary(model_4)


Ig_naccho_net2 <- asIgraph(naccho_net2)
naccho_net_mutual <- is.mutual(Ig_naccho_net2)
which_mutual(Ig_naccho_net2)
# FL <- subgraph.edges(graph=Ig_naccho_net2, eids=which(V(Ig_naccho_net2)$state=="FL"), delete.vertices = TRUE)
# plot(FL, edge.arrow.size=.4, vertex.label= NA, vertex.size=5)
# 
# GA <- subgraph.edges(graph=Ig_naccho_net2, eids=which(V(Ig_naccho_net2)$state=="GA"), delete.vertices = TRUE)
# AL <- subgraph.edges(graph=Ig_naccho_net2, eids=which(V(Ig_naccho_net2)$state=="AL"), delete.vertices = TRUE)
# MO <- subgraph.edges(graph=Ig_naccho_net2, eids=which(V(Ig_naccho_net2)$state=="MO"), delete.vertices = TRUE)
# IL <- subgraph.edges(graph=Ig_naccho_net2, eids=which(V(Ig_naccho_net2)$state=="IL"), delete.vertices = T)
# 
# plot(FL, edge.arrow.size=.4, vertex.label= NA, vertex.size=5)
# plot(GA, edge.arrow.size=.4, vertex.label= NA, vertex.size=5)
# plot(AL, edge.arrow.size=.4, vertex.label= NA, vertex.size=5)
# plot(MO, edge.arrow.size=.4, vertex.label= NA, vertex.size=5)
# plot(IL, edge.arrow.size=.4, vertex.label= NA, vertex.size=5)


library(RColorBrewer)

# MOIL <- subgraph.edges(graph=Ig_naccho_net2, eids=which(V(Ig_naccho_net2)$state=="IL"|V(Ig_naccho_net2)$state=="MO"), delete.vertices = T)
# V(MOIL)$color <- ifelse(V(MOIL)$state == "MO", "blue", "red")
# plot(MOIL, edge.arrow.size=.4, vertex.label= NA, vertex.size=5, label = T )
# 
# 
# V(MO)$state
# plot.igraph(MOIL)
# 
netMO <- get.inducedSubgraph(naccho_net2, which(naccho_net2 %v% "state" == "MO"))
netIA <- get.inducedSubgraph(naccho_net2, which(naccho_net2 %v% "state" == "IA"))
netIL <- get.inducedSubgraph(naccho_net2, which(naccho_net2 %v% "state" == "IL"))
gden(netMO)
gden(netIA)
gden(netIL)


# netMOIL <- get.inducedSubgraph(naccho_net2, which(naccho_net2 %v% "state" == "MO"|naccho_net2 %v% "state" == "IL")) 
netMOILIA <- get.inducedSubgraph(naccho_net2, which(naccho_net2 %v% "state" == "MO"|naccho_net2 %v% "state" == "IL"|naccho_net2 %v% "state" == "IA"))

betweenness(netMOILIA) %>% sort()
order(betweenness(netMOILIA))


ifelse(netMOIL %v% "state" == "MO", "blue","red")
gplot(netMO, displaylabels =  T)

###########################################
color <- NULL
color[netMOILIA %v% "state" == "MO"] <- "#0ABAB5"
color[netMOILIA %v% "state" == "IL"] <- "goldenrod2"
color[netMOILIA %v% "state" == "IA"] <- "lightpink"
netMOILIA %v% "color" <- color

pop_size <- netMOILIA %v% "population" %>% log2()
netMOILIA %v% "pop_size" <- pop_size
netMOILIA %v% "betweenness" <- betweenness(netMOILIA)
# MO-IL-IA 
gplot(netMOILIA, vertex.col = netMOILIA %v% "color", edge.lwd = 0.5,  arrowhead.cex = 0.5, main = "Local Health Departments Network \nin Missouri, Illinois, and Iowa")

gplot(netMOILIA, vertex.col = netMOILIA %v% "color", edge.lwd = 0.5, arrowhead.cex = 0.5, main = "Local Health Departments Network \nin Missouri, Illinois, and Iowa", displaylabels = T, 
      label =  ifelse(netMOILIA %v% "betweenness" > 1000, netMOILIA%v%"vertex.names",  NA),label.pos =  10)
legend("bottomright", legend = c("MO", "IL", "IA"), col = c("#0ABAB5", "goldenrod2", "lightpink"), pch = 19, cex = 1.5, pt.cex = 2, xjust = .5, bty = "n", title = "")
# gplot(netMOILIA, vertex.col = netMOILIA %v% "color", edge.lwd = (is.mutual(Ig_naccho_net2) %>% as.numeric() *2)+1,  arrowhead.cex = 0.5)


library(RColorBrewer)
n <- 49
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector <- col_vector[1:49]
degree <- degree(naccho_net2)
naccho_net2 %v% "color" <- col_vector[as.factor(naccho_net2 %v% "state")]
naccho_net2 %v% "degree" <- degree

# full plot
gplot(naccho_net2, vertex.col = naccho_net2 %v% "color", edge.lwd = 0.5, vertex.cex = (naccho_net2 %v% "FTE" %>% log10)+0.1, arrowhead.cex = 0.5, 
      main = "Full-time Equivalent Staffing Among\n NACCHO 2016 Local Health Departments")

gplot(naccho_net2, vertex.col = naccho_net2 %v% "color", edge.lwd = 0.5, vertex.cex = (naccho_net2 %v% "FTE" %>% log10)+0.1, arrowhead.cex = 0.5, 
      main = "Full-time Equivalent Staffing Among\n NACCHO 2016 Local Health Departments", mode = "circle")

# gplot(naccho_net2, vertex.col = naccho_net2 %v% "color", edge.lwd = 0.5, vertex.cex = (naccho_net2 %v% "FTE" %>% rescale(., 1, 5))+0.1, arrowhead.cex = 0.5)
# gplot(naccho_net2, vertex.col = naccho_net2 %v% "color", edge.lwd = 0.5, vertex.cex = (naccho_net2 %v% "degree") %>% rescale(.,1, 5), arrowhead.cex = 0.5)

# model_fit <- gof(model_4, GOF = ~distance + espartners + degree + triadcensus, burnin = 1e+5, interval = 1e+5)
model_fit <- gof(model_4, GOF = ~ idegree + espartners + dspartners, burnin = 1000000, interval = 1000, seed = 567 )
summary(model_fit)
op <- par(mfrow = c(2,2))
plot(model_fit, cex.axis = 1.6, cex.label = 1.6)
par(op)

# discriptive 
summary(infor_full$c0jurisdiction %>% as.factor()) 
median(infor_full$c0population)
median(infor_full$c5q37)
median(infor_full$tenure)
