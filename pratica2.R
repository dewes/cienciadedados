
#Pós Graduação em Ciência de DadosDisciplina: Tópicos Avançados –Metodologias para Prevenção à FraudeConteúdo:  Análise de Redes ComplexasProfessora: Aline Riquetti Emidio

######## Carregando os pacotes ########setwd("C:/Users/.../.../...")
getwd()
library(igraph)
######## Lendo os arquivos ##########
link = read.csv("REDE1.csv", sep=";", header = TRUE)
nodes = read.csv("NODES1.csv", sep=";", header = TRUE)
####### Criando a rede e plotando ###########
net <-graph_from_data_frame(d = link, vertices = nodes, directed = FALSE)
par(mfrow=c(1,1))
plot(net, edge_curved=TRUE, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=2,vertex.color=c("red","blue")[factor(nodes$fraude)])

######################## Homophilia ############################################# : A tendencia dos nós de se conectarem a outros com caracteristicas semelhantes.

# Trazendo a label de link para o from e para o to
base <-merge(link,nodes,by.x="from",by.y="NODES")
names(base)[3]<-"fraude_from"
base <-merge(base,nodes,by.x="to",by.y="NODES")
names(base)[4]<-"fraude_to"

# Contando os nós por tipo
NOS_TOTAL<-nrow(nodes)
NOS_FRAUDE<-sum(nodes$fraude == 'Fraude')
NOS_LEGITIMOS<-sum(nodes$fraude == 'Não Fraude')

# Contando os links por tipo

LABEL_CRUZADA<-sum(base$fraude_from == 'Fraude' & base$fraude_to =="Não Fraude")+sum(base$fraude_from == 'Não Fraude' & base$fraude_to =="Fraude")
LINK_TOTAL<-nrow(base)

# Verificando homophilia

LABEL_CRUZADA/LINK_TOTAL2*NOS_FRAUDE/NOS_TOTAL*NOS_LEGITIMOS/NOS_TOTAL

if (LABEL_CRUZADA/LINK_TOTAL >= 2*NOS_FRAUDE/NOS_TOTAL*NOS_LEGITIMOS/NOS_TOTAL)  {print("A rede não é homophilica")} else {print("A rede é homophilica")}


##################### Medidasde vizinhança ################################
############### Graus#### Tem a  função IN OUT e ALL

deg_all <-degree(net, mode="all")
deg_in <-degree(net, mode="in")
deg_out <-degree(net, mode="out")
degrees<-rbind(deg_all, deg_in, deg_out)
degrees

########### Hub e Authorities

hub <-hub_score(net, weights=NA)$vector
autho <-authority_score(net, weights=NA)$vector
hub_auth<-rbind(hub, autho)
hub_auth

par(mfrow=c(1,2))
plot(net, edge.arrow.size=.5,  vertex.label.dist=3, vertex.color=c("red","blue")[factor(nodes$fraude)], vertex.size=hub*30, main="Hubs")

plot(net, edge.arrow.size=.5,  vertex.label.dist=3, vertex.color=c("red","blue")[factor(nodes$fraude)],vertex.size=autho*30, main="Authorities")
########### Densidade##### A proporção de nós existentes na rede, dentro o total possível.   

densidade<-edge_density(net, loops=FALSE)

densidade
##################  Distância e medidas de centralidade  ########################
#################### Tamanho médio do caminho: a média do caminho mais curto entre dois nós da rede

dist_media<-mean_distance(net, directed=T)
distances(net, weights=NA)

########### Centralidade

closeness(net, mode="all", weights=NA)

########### Entrelaçamento

betweenness(net)

