# install.packages("bibliometrix", dependencies=TRUE, 
                 repos = "http://cran.us.r-project.org")

### Carregando o pacote 

library(bibliometrix)

setwd("C:/Users/Michele/Documents/Curso_R_bibliometrix/")


### Dados utilizados

#Os dados utilizados foram obtidos através do serviço de indexação de citações científicas ```Web of Science``` (http://www.webofknowledge.com). Foram pesquisadas as palavras "Landscape genetics", "Conservation" e "Climate change" entre os anos de 1900 à 2019.

### Carregamento e importação

#O arquivo foi inicialmente carregado como um vetor de caracteres grandes no formato ```BibTeX```, através da função *readFiles*.


D <- readFiles("data/file1.bib")


#E convertido em um quadro de dados (dataframe) usando a função *convert2df*.


M <- convert2df(D, dbsource = "isi", format = "bibtex")


## **Análises Bibliométricas**

### Análise descritiva

#A função *biblioAnalysis* calcula as principais medidas bibliométricas.


results <- biblioAnalysis(M, sep = ";")


#### Resumo das informações

#A função *Summary* sumariza as principais informações encontradas no dataset.

options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)


#Alguns gráficos básicos podem ser desenhados usando a função genérica *plot*.


plot(x = results, k = 10, pause = FALSE)


### Análise das referências citadas

#Para uma extração correta, primeiro identificamos o campo separador entre diferentes referências:
  

M$CR[1]


#A função *citation* gera a tabela de frequências das referências mais citadas ou os primeiros autores mais citados (de referências).

#### Manuscritos mais citados

CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])


#### Primeiro autor mais citado


CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])


#A função *localCitations* gera a tabela de frequência dos autores mais citados localmente.

##### Autores citados localmente mais frequentes


CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]


### Ranking de domínio dos autores

#A função *dominace* calcula o ranking de dominância dos autores.


DF <- dominance(results, k = 10)
DF


### H-Index dos autores

#O índice h é uma métrica no nível do autor que tenta medir o impacto da produtividade e da citação das publicações de um cientista ou estudioso.
#A função  *Hindex* calcula o índice H dos autores ou o índice H das fontes e suas variantes (índice-g e índice-m) em uma coleção bibliográfica.


indices <- Hindex(M, field = "author", elements="RENAUT J", sep = ";", years = 10)
#Bornmann's impact indices:
indices$H



indices$CitationList


#### H-Index dos primeiros 10 autores mais produtivos


authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H



### Produtividade dos principais autores ao longo do tempo

A função *AuthorProdOverTime* calcula e plota a produção dos autores (em termos de número de publicações e total de citações por ano) ao longo do tempo.


topAU <- authorProdOverTime(M, k = 10, graph = TRUE)


#### Produtividade dos autores por ano


head(topAU$dfAU)


#### Lista de documentos dos autores


head(topAU$dfPapersAU)


### Estimativa do coeficiente da Lei de Lotka

A função lotka* estima os coeficientes da lei de Lotka para a produtividade científica (Lotka AJ, 1926).Através dessa função é possível estimar o coeficiente Beta de nossa coleção bibliográfica e avaliar, através de um teste estatístico, a similaridade desta distribuição empírica com a teórica.


L <- lotka(results)
# Produtividade dos autores DistribuiÃ§Ã£o empÃ?rica
L$AuthorProd



# Estimativa do coefficiente Beta
L$Beta



# Constante
L$C



# Qualidade do ajuste
L$R2



# P-value de K-S para o teste de duas amostras
L$p.value



#### Distribuição observada


Observed=L$AuthorProd[,3]


#### Distribuição teórica com Beta = 2


Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
plot(L$AuthorProd[,1],Theoretical,type="l",col="red", ylim=c(0, 1), 
     xlab="Articles",ylab="Freq. of Authors", main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"), col=c("red","blue"),
       lty = c(1,1,1),cex=0.6,bty="n")



### Matrizes de redes bibliográficas

#### Redes bipartidas

#cocMatrix* é uma finção geral para calcular uma rede bipartida selecionando um dos atributos de metadados.


A <- cocMatrix(M, Field = "SO", sep = ";")


#Classificando, em ordem decrescente, as somas da coluna de ```A```, você pode ver as fontes de publicação mais relevantes:
  
  #### Ordem decrescente
  

sort(Matrix::colSums(A), decreasing = TRUE)[1:5]


#Seguindo essa abordagem, você pode calcular várias redes bipartidas:
  
  #### Rede de citação
  
 
A <- cocMatrix(M, Field = "CR", sep = ".  ")


#### Rede de autor


A <- cocMatrix(M, Field = "AU", sep = ";")


#### Redes do país

#Países dos autores não é um atributo padrão do quadro de dados bibliográficos. Você precisa extrair essas informações do atributo de afiliação usando a função *metaTagExtraction*.


M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
A <- cocMatrix(M, Field = "AU_CO", sep = ";")


#### Rede de palavra-chave de autores


A <- cocMatrix(M, Field = "DE", sep = ";")


#### Rede de palavras-chave adicional


A <- cocMatrix(M, Field = "ID", sep = ";")


#### Acoplamento bibliográfico

#A função *biblioNetwork* calcula, a partir de um quadro de dados bibliográficos, as redes de acoplamento mais utilizadas: Autores, Fontes e Países.

#O código a seguir calcula uma rede de acoplamento de artigos clássicos:
  
  
NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "references", sep = ".  ")


#A função *normalizeSimilarity* calcula a força de associação, inclusão, similaridade de Jaccard ou Salton entre os vértices de uma rede. *normalizeSimilaridade* pode ser recuperada diretamente da função *networkPlot ()* usando o argumento normalize .


NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, 
                n = 100, Title = "Authors' Coupling", type = "fruchterman",
                size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8, 
                label.n=10,label.cex=F)



#### Co-citação bibliográfica

#Usando a função *biblioNetwork* , você pode calcular uma rede clássica de co-citação de referência:
  
 
NetMatrix <- biblioNetwork(M, analysis = "co-citation", 
                           network = "references", sep = ".  ")


#Colaboração bibliográfica

#Usando a função *biblioNetwork* , você pode calcular a rede de colaboração de um autor:
  

NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")


#ou uma rede de colaboração do país:
  
  
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")



### Análise descritiva das características do gráfico de rede

#A função *networkStat* calcula várias estatísticas de resumo.


#Um exemplo de redes de co-ocorrência clássica
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)


#### As estatísticas resumidas da rede

names(netstat$network)


#### Os principais índices de centralidade e prestígio dos vértices


names(netstat$vertex)


#### Resumo dos principais resultados da função *networkStat* 

summary(netstat, k=10)



### Visualizando redes bibliográficas

#### Colaboração Científica no País


# Criação de uma rede de colaboração entre países
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", 
                           network = "countries", sep = ";")


#### Gráfico da rede


net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", 
                type = "circle", size=T, remove.multiple=FALSE, 
                labelsize=0.7,cluster="none")


#### Redes de co-citação


# Criação de uma rede de co-citação
NetMatrix <- biblioNetwork(M, analysis = "co-citation", 
                           network = "references", sep = ";")


#### Gráfico da rede 


net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", 
                type = "fruchterman", size=T, remove.multiple=FALSE,
                labelsize=0.7,edgesize = 5)



#### Co-ocorrência de palavras-chave


# Criação de rede de co-ocorrência de palavras-chave
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")



#### Gráfico da rede


net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)



### Análise de _Co-Word_: A estrutura conceitual de um campo



# Estrutura conceitual usando palavras-chave (método="CA")
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, k.max=8, stemming=FALSE, labelsize=10, documents=10)



#### Rede histórica de citação direta


# Criação de uma rede de citação histórica
histResults <- histNetwork(M, min.citations = 10, sep = ";")



# Gráfico de uma rede de co-citação histórica
net <- histPlot(histResults, n=15, size = 20, labelsize=10, size.cex=TRUE, arrowsize = 0.5, color = TRUE)


## **Respostas encontradas**


## **Dificuldades encontradas**

#Primeiramente, encontrei dificuldades com a obtenção dos dados pelo site __Web of science__, pois como não o conhecia, não percebi que eu poderia "adicionar linhas" com outras palavras específicas de interesse. Essa dificuldade logo foi sanada e acabei achando a plataforma muito interessante. 

#Posteriormente, tive problemas em entender com o Rmarkdown funcionava. Achei que entre as chaves "{}" era necessário colocar uma função específica para rodar o código. Conhecia apenas o símbolo "#" para a formatação das palavras, com o tempo e pesquisas, conheci outros meios de formatação. Pude me integrar melhor com o RMArkdown através do Cheat Sheet:  <https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf> e também do vídeo de um colega de laboratório, Alexandre Aono: <https://www.youtube.com/watch?v=AuXTUalb0HU&t=310s>.

#Quando tentei baixar o arquivo no formato PDF, o Rmarkdow apresentou um erro relativo ao sistema LATEX, no qual tive bastante dificuldade em sanar. Mesmo baixando o LATEX, o arquivo não era gerado. Encontrei a solução em um forum de discussão sugerido por um amigo da turma através deste link: <https://tex.stackexchange.com/questions/408798/sorry-but-pdflatex-did-not-succeed?rq=1>.

## **Bibliografia**

#Balkenhol, N.; McDevitt, A. D.; Sommer, S. Landscape genetic approaches in conservation biology and management. Conservation Genetics, v. 14, n. 2, p. 249-251, april. 2013.

#Storfer A, Murphy MA, Evans JS, Goldberg CS, Robinson S, Spear SF, Dezzani R, Delmelle E, Vierling L, Waits LP (2007) Putting the ‘landscape’ in landscape genetics. Heredity 98:128–142

#Storfer A, Murphy MA, Spear SF, Holderegger R, Waits LP (2010) Landscape genetics: where are we now? Mol Ecol 19:3496–3514

#Holderegger R, Wagner HH (2008) Landscape genetics.Bioscience 58:199–207

#Balkenhol N, Gugerli F, Cushman SA, Waits L, Coulon A, Arntzen J, Holderegger R, Wagner HH (2009) Identifying future research needs in landscape genetics: where to from here? Landsc Ecol 24:455–463

#Manel S, Joost S, Epperson BK, Holderegger R, Storfer A, Rosenberg MS, Scribner KT, Bonin A, Fortin MJ (2010) Perspectives on the use of landscape genetics to detect genetic adaptive variation in the field. Mol Ecol 19:3760–3772

#Segelbacher G, Cushman SA, Epperson BK, Fortin M-J, Francois O, Hardy OJ, Holderegger R, Manel S (2010) Applications of landscape genetics in conservation biology: concepts and challenges. Conserv Genet 11:375–385

#Epperson BK, McRae B, Scribner K, Cushman SA, Rosenberg MS, Fortin M-J, James PMA, Murphy M, Manel S, Legendre P, Dale MRT (2010) Utility of computer simulations in landscape genetics. Mol Ecol 19:3540–3564

#Sork VL, Waits L (2010) Contributions of landscape genetics—approaches, insights and future potential. Mol Ecol 19:3489–3495

#Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.
