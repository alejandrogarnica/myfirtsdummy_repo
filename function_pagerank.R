###################################
#  Funcion PageRank para salida   #
###################################

#from <- c("C","B","C","A","C") 
#to <- c("A","A","B","C","D")
#datos1<-data.frame(cbind(from,to))
datos1 <- read.table("C:/Users/Usuario/Desktop/datos.txt", header=T)

do_pagerank <- function(data, element) {

if(require(igraph)){
	d1 <- graph.data.frame(data, directed = T )
	res<-page.rank(d1)
} else {
    install.packages("igraph")
    if(require(igraph)){
	d1 <- graph.data.frame(data, directed = T )
	res<-page.rank(d1)
    } else {
	error<-("Se necesita la libreria igraph")
	return(error)
    }
}
PageRank<-sort(res$vector, decreasing=T)
x<-data.frame(cbind(PageRank))

y<-names(PageRank)
for (i in 1:length(y)) {
	if(y[i]==element){
		pos=i
		}
	}
ranking<-(1-((pos-1)/length(y)))*10
ranking_del_elemento<-matrix(c(ranking),byrow=TRUE,dimnames=list(c(element),c("ranking_del_elemento")))
write.table(ranking_del_elemento,"C:/Users/Usuario/Desktop/ranking_elemento.txt",sep="\t")
write.table(x,"C:/Users/Usuario/Desktop/page_rank.txt",sep="\t")
}

do_pagerank(datos1,"C")

