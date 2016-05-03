library(igraph)
#Read gml file
filepath <- '~/git/Data-Sciences/CS570/polblogs.gml'
G <- read.graph(filepath, format = "gml")
#Simply the graph by removing loops and multiple connection between same two nodes
G1 <- simplify(G,remove.multiple = TRUE, remove.loops = TRUE)
#Delete any vertices that does not connect to anyone else
G1 <- delete.vertices(G1,which(degree(G1)< 2))
#Color the right wing red and left wing blue
V(G1)$color <- V(G1)$value
V(G1)$color <- gsub(0,"red",V(G1)$color)
V(G1)$color <- gsub("1","blue",V(G1)$color)
#Layout is Fruchterman Reingold. Play with normalization so that graph fits window
coords <- layout.fruchterman.reingold(G1)
coords <- layout.norm(coords,xmin= -2, xmax = 2, ymin = -1.5, ymax = 1.5)
par(mai=c(0,0,1,0)) 
#png(filename = "~/RFiles/Blogplot.png")
#Plot the graph
plot(G1, layout=coords,rescale=FALSE,vertex.label=NA,vertex.size=2, edge.arrow.size=0.1)
#dev.off()
#centrality score
cent.score <- evcent(G1)
nodes.cscore <- data.frame(V(G1)$label,cent.score$vector,stringsAsFactors = FALSE)
names(nodes.cscore) <- c("Label","Score")
nodes.cscore <- nodes.cscore[order(-nodes.cscore$Score),]
#Most influential blogs
for(i in 1:10){
  print(nodes.cscore$Label[i])
  i <- i+1
}
#Least influential blogs
for (i in 0:9) {
  print(nodes.cscore$Label[nrow(nodes.cscore) - i])
  i < i + 1
}
