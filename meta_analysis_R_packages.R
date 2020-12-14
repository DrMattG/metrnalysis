# Visualisation of relationship between R packages that are used in Meta-analysis
library(here)
library(cranlogs)
library(RCurl)
library(XML)
library(stringr)
library(miniCRAN)
library(igraph)
library(visNetwork)
# To get a list of packages that are used in Meta-analysis we can have a look at the 
#CRAN task view for MetaAnalysis

url<-"https://CRAN.R-project.org/view=MetaAnalysis" # location of CRAN task view

# Read in the names of the packages
html <- paste(readLines(url), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
matched2<-unlist(matched)
tst<-sub(".*/packages/","",matched2)
packages.list<-gsub("^(.*?)/.*", "\\1", tst)
packages.list<-packages.list[2:562]

head(packages.list)

pkgs<-data.frame("packages"= unlist(packages.list))
# build a dependency graph
tags <- c(packages.list)
dg <- makeDepGraph(tags, enhances = TRUE)
sort(degree(dg))
Isolated = which(degree(dg)<10)
dg2 = delete.vertices(dg, Isolated)
V(dg2)
vn <- toVisNetworkData(dg2)
vn$nodes$title<-vn$nodes$label
vn$nodes$group<-ifelse(vn$nodes$id %in% pkgs$packages==TRUE, "MetaAnalysis", "Supporting")
# Nodes are sized by degree (the number of links to other packages)
degree_value <- degree(dg2, mode = "all")
vn$nodes$value <- degree_value[match(vn$nodes$id, names(degree_value))]



visNetwork(nodes = vn$nodes, edges = vn$edges,main="Both",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="blue")%>%
  visSave(file =paste0(here(),"/Plots/both.html"), selfcontained = T)



#split in to depends and imports

# Depends indicates dependency on a particular packages loaded (with library()) whenever focal package is loaded.
gDepends <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Depends"), delete.vertices = TRUE)

# Imports indicates packages that are needed by the focal package (this is more likely now - does not load the whole package just the used functions)
gImports <- subgraph.edges(graph=dg, eids=which(E(dg)$type=="Imports"), delete.vertices = TRUE)

sort(degree(gImports))

V(gImports)$name

plot(gImports,vertex.label = ifelse(degree(gImports) > 25, V(gImports)$name, NA), vertex.size=4, edge.arrow.size=.2 )

#Depends visNet
vn1 <- toVisNetworkData(gDepends)
vn1$nodes$title<-vn1$nodes$label

# Nodes are sized by degree (the number of links to other packages)
degree_value1 <- degree(gDepends, mode = "all")
vn1$nodes$value <- degree_value1[match(vn1$nodes$id, names(degree_value1))]
vn1$nodes$group<-ifelse(vn1$nodes$id %in% pkgs$packages==TRUE, "MetaAnalysis", "Supporting")
visNetwork(nodes = vn1$nodes, edges = vn1$edges,main="Depends",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="blue")%>%
  visSave(file =paste0(here(),"/Plots/Depends.html"), selfcontained = T)


#Imports visNet
vn2 <- toVisNetworkData(gImports)
vn2$nodes$title<-vn2$nodes$label
# Nodes are sized by degree (the number of links to other packages)
degree_value2 <- degree(gImports, mode = "all")
vn2$nodes$value <- degree_value2[match(vn2$nodes$id, names(degree_value2))]
vn2$nodes$group<-ifelse(vn2$nodes$id %in% pkgs$packages==TRUE, "MetaAnalysis", "Supporting")
visNetwork(nodes = vn2$nodes, edges = vn2$edges,main="Meta-analysis packages",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="red")%>%
  visSave(file =paste0(here(),"/Plots/Imports.html"), selfcontained = T)

degree_value2[order(degree_value2)]

##### Remove nodes degree <10 in the supporting group

V(gImports)$label=ifelse(V(gImports)$name %in% pkgs$packages==TRUE, "MetaAnalysis", "Supporting")
Isolated = which(degree(gImports)<10 & V(gImports)$label=="Supporting")
G2 = delete.vertices(gImports, Isolated)
LO2 = layout_with_gem(G2)
par(bg="gray40")
plot(G2, layout=LO2, vertex.label=NA, edge.arrow.size=0.2, vertex.size=degree(G2), vertex.color="red", edge.color="#C0C0C0")

vn2 <- toVisNetworkData(G2)
vn2$nodes$title<-vn2$nodes$label
# Nodes are sized by degree (the number of links to other packages)
degree_value2 <- degree(G2, mode = "all")
vn2$nodes$value <- degree_value2[match(vn2$nodes$id, names(degree_value2))]

vn2$nodes$group<-ifelse(vn2$nodes$id %in% pkgs$packages==TRUE, "MetaAnalysis", "Supporting")




visNetwork(nodes = vn2$nodes, edges = vn2$edges,main="Meta-analysis packages",height = "500px", width = "100%")%>%
  visOptions(highlightNearest = TRUE)%>%
  visNodes(color="red")%>%
  visSave(file =paste0(here(),"/Plots/Imports.html"), selfcontained = T)



degree(gImports)
betweenness(gImports)
V(gImports)$name[1:95]

# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }
# 
# 
# ipak(V(gImports)$name[1:95])


x=list()

for (i in 1:95){
  tryCatch({
    x[i]=packageDescription(V(gImports)$name[i])$Description
  },
  error=function(cond) {
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)}
  )
}


Descriptions <- sub("http://([[:alnum:]|[:punct:]])+", '', x)

corpus = tm::Corpus(tm::VectorSource(Descriptions))
corpus
corpus.cleaned <- tm::tm_map(corpus, tm::removeWords, tm::stopwords('english')) # Removing stop-words
corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words 
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces


# get information on the functions for each package

functions_list<-list(
library(help=compute.es)$info[[2]],
library(help=MAd)$info[[2]],
library(help=effectsize)$info[[2]],
library(help=MOTE)$info[[2]],
library(help=metaBLUE)$info[[2]],
library(help=meta)$info[[2]],
library(help=revtools)$info[[2]],
library(help=metafuse)$info[[2]],
library(help=epiR)$info[[2]],
library(help=psychometric)$info[[2]],
#library(help=concurve)$info[[2]]
library(help=forestplot)$info[[2]],
library(help=baggr)$info[[2]],
library(help=metamisc)$info[[2]],
library(help=metaplus)$info[[2]],
library(help=pimeta)$info[[2]],
library(help=MetaUtility)$info[[2]],
library(help=metawho)$info[[2]],
library(help=forestmodel)$info[[2]],
library(help=weightr)$info[[2]],
library(help=MetaAnalyser)$info[[2]],
library(help=metaforest)$info[[2]],
library(help=boot.heterogeneity)$info[[2]],
library(help=EValue)$info[[2]],
library(help=puniform)$info[[2]],
library(help=publipha)$info[[2]],
library(help=dfmeta)$info[[2]],
library(help=metap)$info[[2]],
library(help=TFisher)$info[[2]],
library(help=mvtmeta)$info[[2]],
library(help=MBNMAdose)$info[[2]],
library(help=CIAAWconsensus)$info[[2]],
library(help=diagmeta)$info[[2]],
library(help=NMADiagT)$info[[2]],
library(help=jarbes)$info[[2]],
library(help=multinma)$info[[2]],
library(help=netmeta)$info[[2]],
library(help=NMAoutlier)$info[[2]],
library(help=nmadb)$info[[2]],
library(help=CPBayes)$info[[2]],
library(help=getspres)$info[[2]],
library(help=MBNMAtime)$info[[2]],
library(help=MetaIntegrator)$info[[2]],
library(help=RcmdrPlugin.EZR)$info[[2]],
library(help=RcmdrPlugin.RMTCJags)$info[[2]],
library(help=miniMeta)$info[[2]],
library(help=KenSyn)$info[[2]],
library(help=metabolic)$info[[2]],
library(help=MAc)$info[[2]],
library(help=metafor)$info[[2]],
library(help=psychmeta)$info[[2]],
library(help=estmeansd)$info[[2]],
library(help=SingleCaseES)$info[[2]],
library(help=metagear)$info[[2]],
library(help=clubSandwich)$info[[2]],
library(help=metapower)$info[[2]],
library(help=rmeta)$info[[2]],
library(help=mixmeta)$info[[2]],
library(help=mmeta)$info[[2]],
library(help=MetaStan)$info[[2]],
library(help=RBesT)$info[[2]],
library(help=metaBMA)$info[[2]],
library(help=rma.exact)$info[[2]],
library(help=metamedian)$info[[2]],
library(help=metagam)$info[[2]],
library(help=metarep)$info[[2]],
library(help=metaviz)$info[[2]],
library(help=metaplotr)$info[[2]],
library(help=altmeta)$info[[2]],
library(help=mc.heterogeneity)$info[[2]],
library(help=ConfoundedMeta)$info[[2]],
library(help=xmeta)$info[[2]],
library(help=PublicationBias)$info[[2]],
library(help=SCMA)$info[[2]],
library(help=metaRMST)$info[[2]],
library(help=metapro)$info[[2]],
library(help=mvmeta)$info[[2]],
library(help=metaSEM)$info[[2]],
library(help=robumeta)$info[[2]],
library(help=bamdit)$info[[2]],
library(help=CopulaDTA)$info[[2]],
library(help=GENMETA)$info[[2]],
library(help=metacart)$info[[2]],
library(help=pcnetmeta)$info[[2]],
library(help=nmathresh)$info[[2]],
library(help=gemtc)$info[[2]],
library(help=catmap)$info[[2]],
library(help=getmstatistic)$info[[2]],
library(help=GMCM)$info[[2]],
library(help=MendelianRandomization)$info[[2]],
library(help=metaMA)$info[[2]],
library(help=RcmdrPlugin.MA)$info[[2]],
library(help=MAVIS)$info[[2]],
library(help=joineRmeta)$info[[2]],
library(help=PRISMAstatement)$info[[2]])



