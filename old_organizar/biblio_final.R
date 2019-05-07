# Este script ? baseado no material do link 
# (https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html)

# O dataset utilizado foi filtrado no Web of Science com os termos
# heat shock protein, plant, stress e cold. 

# O pacote j? esta instalado, portanto basta apenas carreg?-lo.
library(bibliometrix)

# O objeto D ? um vetor de caracteres que cont?m os artigos de hsps no formato bibliometrix 
D <- readFiles("data/file1.bib")

# O objeto M ? a convers?o do objeto D (vetor de caracteres) em um dataframe
M <- convert2df(D, dbsource = "isi", format = "bibtex")

# O objeto results recebe a fun??o biblioAnalysis que serve para calcular as principais  
# medidas bibliom?tricas deste dataset
results <- biblioAnalysis(M, sep = ";")

# A fun??o summary serve para resumir as principais informa??es do dataset
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)

# A fun??o plot serve para plotar as principais informa??es do dataset
plot(x = results, k = 10, pause = FALSE)

# Verificando a estrutura dos dados (sepados por ; ou .  )
M$CR[1] # Est?o separados por ();)

# Manuscritos frequentemente mais citados
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

# Manuscritos cujo o primeiro autor ? mais citado
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

# Autores locais mais citados
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]

# Ranking de domin?ncia dos autores
DF <- dominance(results, k = 10)
DF

# H-Index dos autores
indices <- Hindex(M, field = "author", 
                  elements="RENAUT J", 
                  sep = ";", years = 10)

# Indice de impacto do autor Renaut J (escolhido ao acaso)
indices$H

# Cita??es do autor Renaut
indices$CitationList

# H-Index dos 10 autores mais produtivos
authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", 
                  elements=authors, 
                  sep = ";", years = 50)
indices$H

# Top autores mais produtivos ao longo do tempo
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

# Autores mais produtivos por ano (Tabela)
head(topAU$dfAU)

# Lista de documentos dos autores
head(topAU$dfPapersAU)

# Coeficiente de estima??o da lei de Lotka
L <- lotka(results)

# Produtividade dos autores. Distribui??o empirica
L$AuthorProd

# Estimativa do coefficiente Beta
L$Beta

# Constante
L$C

# Qualidade do ajuste
L$R2

# P-value de Kolmogorov-Smirnoff para o teste de duas amostras
L$p.value

# Distribui??o observada
Observed=L$AuthorProd[,3]

# Distribui??o te?rica com Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",
     ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",
     main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),
       col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")

# Bibliografic network matrices
# Bipartite network
A <- cocMatrix(M, Field = "SO", sep = ";")

# Ordem decrescente
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

# Citation network
A <- cocMatrix(M, Field = "CR", sep = ".  ")

# Author network
A <- cocMatrix(M, Field = "AU", sep = ";")

# Country network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
A <- cocMatrix(M, Field = "AU_CO", sep = ";")

# Author keyword network
A <- cocMatrix(M, Field = "DE", sep = ";")

# Keyword Plus network
A <- cocMatrix(M, Field = "ID", sep = ";")

# Bibliographic coupling
# Classical article coupling network
NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "references", 
                           sep = ".  ")

# Normaliza??o
NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "authors", sep = ";")

net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, 
                n = 100, Title = "Authors' Coupling", type = "fruchterman", 
                size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,
                label.n=10,label.cex=F)

# Bibliographic co-citation
# Classical reference co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", 
                           network = "references", sep = ".  ")

# Bibliographic colaboration
NetMatrix <- biblioNetwork(M, analysis = "collaboration", 
                           network = "authors", sep = ";")

# Country colaboration network
NetMatrix <- biblioNetwork(M, analysis = "collaboration", 
                           network = "countries", sep = ";")

# Descriptive analysis of network graph characteristics
# An example of a classical keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", 
                           network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)

# Verificando os nomes da rede
names(netstat$network)
names(netstat$vertex)

# Summary da rede
summary(netstat, k=10)

# Visualizing bibliographic networks
# Country Scientific Collaboration
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", 
                           network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], 
                Title = "Country Collaboration", 
                type = "circle", size=TRUE, remove.multiple=FALSE,
                labelsize=0.7,cluster="none")

# Co-citation network
# Create a co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", 
                           network = "references", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network",
                type = "fruchterman", size=T, remove.multiple=FALSE, 
                labelsize=0.7,edgesize = 5)

# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", 
                           network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, 
                n = 30, Title = "Keyword Co-occurrences", 
                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


# Co-Word Analysis: The conceptual structure of a field
# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, 
                          k.max=8, stemming=FALSE, labelsize=10, 
                          documents=10)

# Historical Direct Citation Network
# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 10, sep = ";")

# Plot a historical co-citation network
net <- histPlot(histResults, n=15, size = 20, labelsize=10, 
                size.cex=TRUE, arrowsize = 0.5, color = TRUE)

# Fim do script
# Isso ? tudo pessoal!
