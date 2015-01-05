library(igraph)

#A=1 B=2 C=3 D=4 E=5 F=6

g1<- graph(c(
  1, 2, 1, 3, 1, 4, 
  2, 3, 2, 6, 3, 1, 
  3, 5, 4, 2, 4, 1, 
  4, 5, 5, 2, 5, 6, 
  6, 3, 6, 4), 
            directed=TRUE)
plot(g1)

M = get.adjacency(g1, sparse = FALSE)
M = t(M / rowSums(M))
n = nrow(M)

U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
e = eigen(A)
v <- e$vec[,1]
v <- as.numeric(v) / sum(as.numeric(v))
v

page.rank(g1)$vector

library(expm)
n = nrow(M)
U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
r = matrix(data=rep(1/n, n), nrow=n, ncol=1)
t(A%^%100 %*% r)


####  EJEMPLO PROPIO ####

from <- c("C","B","C","A","C") 
to <- c("A","A","B","C","D")
datos <- data.frame(cbind(from,to))
datos
d1 <- graph.data.frame(datos, directed = T )
plot.igraph(d1)

res<-page.rank(d1)
res$vector
PageRank<-sort(res$vector, decreasing=T)
x<-data.frame(cbind(PageRank))
y<-names(PageRank)
for (i in 1:length(y)) {
	if(y[i]=="D"){
		posi=i
		}
	}
f<-data.frame(c("D","ranking=",3))
plot(res$vector,col="blue")
