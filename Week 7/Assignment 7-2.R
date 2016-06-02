edges = read.csv("edges.csv")
users = read.csv("users.csv")
146*2/59

table(users$locale)

library(igraph)
g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)

length(which(degree(g) >= 10))

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
summary(V(g)$size)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
table(V(g)$size, V(g)$color)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
