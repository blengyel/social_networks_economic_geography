# Community Detection and Network Visualization with R

rm(list=ls())

# 0. Set up working directory

setwd("c:/Users/lengyel.balazs/Desktop/LengyelB_2022June/Rajk kurzus 2023")

# 0. Install and call in package

install.packages("igraph") # In case you have not installed the package yet. 
#It is worth to repeat installation on a -say- yearly basis. Igraph R syntax is changing frequently.

install.packages("sand")

library(igraph) # Call this package in case you work with network objects.
library(sand) # Sand is important for teaching purposes only: it contains built-in data.


# 1. Network measures: Density, Transitivity, Average Path Length
data(karate) # very famous data collected from the Zachary karate club
plot(karate)

graph.density(karate)
transitivity(karate) ## [1] 0.2556818

transitivity(karate, "local", vids=c(1,34))  ## [1] 0.1500000 0.1102941
average.path.length(karate)
diameter(karate)


# 2. Project and plot bipartite networks

kevin=read.table("kevin_bacon.csv", header=T, sep=",") # read in movie-actor data in a data.frame format

kevin_edgelist=merge(kevin, kevin, by="movie", all=T) # create co-occurrence edgelist by merging data to itself
names(kevin_edgelist)=c("movie", "actor1", "actor2") # give names to the columns

View(kevin_edgelist)

kevin_edgelist=kevin_edgelist[which(kevin_edgelist$actor1!=kevin_edgelist$actor2),c(2:3)] 
# keep only those edges where actors are not identical
# drop the column of movie names

kevin_net=graph_from_data_frame(kevin_edgelist, directed = F, vertices = NULL) # create undirected projection of the network
kevin_net=simplify(kevin_net)  

plot(kevin_net) # simplest plot

# Create a better plot
coords <-layout_on_grid(kevin_net, width = 0, height = 0, dim = 2) # determine the position of nodes
plot(kevin_net, 
     layout=coords)

plot(kevin_net, 
     layout=layout.fruchterman.reingold,
     vertex.label=NA,
     vertex.size=degree(kevin_net)*1.5,
     vertex.color=heat.colors(3, rev=T)[graph.coreness(kevin_net)],
     edge.width=edge.betweenness(kevin_net, directed=F)*0.02)


# 3. Community structure

kc <- fastgreedy.community(karate) # running the fast greedy algorithm to identify communities

# kc is a community structure object containing the following elements:

length(kc) # the number of communities
sizes(kc) # size distribution of community structure
membership(kc) # node characteristics 

modularity(kc) # calculates the Q index of modularity

plot(kc,karate) # simple plot of communities

plot(karate, 
     layout=layout.fruchterman.reingold,
#     vertex.label=NA,
     vertex.size=20,
     vertex.color=heat.colors(length(kc))[membership(kc)],
     edge.width=2)

mycolors=c("deeppink", # community 1
           "cyan", # community 2
           "magenta") # community 3

plot(karate, 
     layout=layout.fruchterman.reingold,
     #     vertex.label=NA,
     vertex.size=20,
     vertex.color=mycolors[membership(kc)],
     edge.width=2)

# Community finding algorithms and their running times: 
# https://www.r-bloggers.com/summary-of-community-detection-algorithms-in-igraph-0-6/


# 4. Exercise: the global airline network

# Read, clean the data and plot the network
air_nodes=read.table("airlines_nodes.csv", header=T, sep=";")
  air_nodes$lng=as.numeric(air_nodes$lng)
  air_nodes=air_nodes[!is.na(air_nodes$lng),]
air_edges=read.table("airlines_edges.csv", header=T, sep=";")

air_g=graph_from_data_frame(air_edges, vertices=air_nodes, directed=F) # network object from edgelist and nodelist
  air_g=simplify(air_g) # remove self-loops and multiple edges

coordinates<-matrix(c(V(air_g)$lng, V(air_g)$lat),nrow=length(V(air_g)$lat),ncol=2) # coordinate matrix to locate nodes in geographical networks

# plot the network into a PNG file
png(filename="air_net.png", width=6000, height=2000, units="px")
plot.igraph(air_g,
     layout=coordinates, 
     # vertex.color=V(air_g)$col, 
     # vertex.frame.color=V(air_g)$fc, 
     vertex.size=0.5,
     edge.color="gray50", edge.curved=F,
     edge.width=1,
     axes=F, frame=F, rescale=T,
     vertex.label=NA
)
dev.off()

# Color network after community finding algorithm 
library(viridis)

set.seed(1024)
air_c=cluster_louvain(air_g) # the Louvain algorithm is a very popular method that is fast on large graphs
  length(air_c)
  length(V(air_g))
  length(air_c[sizes(air_c)>10])

c=as.data.frame(sizes(air_c)) # create community table
  names(c)=c("comm", "size")

v=data.frame(as.integer(V(air_g)$name), as.vector(membership(air_c))) # membership table
  names(v)=c("Id", "comm")

col=c[c$size>10,] # colors into community and membership tables
  #col$col=magma(length(col$comm))
  col$col=c(magma(length(air_c[sizes(air_c)>10])-11),
            "olivedrab1", "navajowhite3", "khaki2", "darkblue",
            "orange","cyan", "deeppink", "red",
            "green4", "brown", "dodgerblue")
  col=col[,c("comm", "col")]
  c=merge(c,col, by="comm", all.x=T)
  v=merge(v,c, by="comm", all.x=T)

air_nodes_new=merge(air_nodes,v, by="Id", all.x=T) # colors into nodes table

# regenerate network and coordinates
air_g=graph_from_data_frame(air_edges, vertices=air_nodes_new, directed=F) # network object from edgelist and nodelist
coordinates<-matrix(c(V(air_g)$lng, V(air_g)$lat),nrow=length(V(air_g)$lat),ncol=2) # coordinate matrix to locate nodes in geographical networks
  
  
# plot the network with built-in community visualization
png(filename="air_comm.png", width=6000, height=2000, units="px")
plot(air_g,
     layout=coordinates, 
     vertex.color=V(air_g)$col,
     vertex.frame.color=V(air_g)$col, 
     vertex.size=1,
     edge.color=NA,
     edge.width=NULL,
     axes=F, frame=F, rescale=T,
     vertex.label=NA
)
dev.off()

# 5. Evaluate the community finding algorithm

set.seed(24)
c1=cluster_louvain(air_g, resolution = 2)
c2=cluster_louvain(air_g, resolution = 2)

compare(membership(c1), membership(c2), method="nmi")
