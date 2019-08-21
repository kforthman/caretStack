funcs <- list.files("R")
funcs <- substr(funcs, 1, nchar(funcs)-2)

relationTree <- matrix(nrow = length(funcs), ncol = length(funcs))

rownames(relationTree) <- funcs
colnames(relationTree) <- funcs

for(i in 1:length(funcs)){for(j in 1:length(funcs)){
used <- grep(funcs[j], readLines(paste0("R/", funcs[i],".R")))
relationTree[i,j] <- ifelse(length(used) > 0, 1, 0)
}}

relationTree["PermTest.rNCV", "rNCV"] <- 0
relationTree["PermuPerf.rNCV", "rNCV"] <- 0
relationTree["permuPred", "modelPerf"] <- 0
relationTree["rNCV.perf.summ", "rNCV"] <- 0
relationTree["rNCV.perm", "rNCV"] <- 0
relationTree["summarize_one","predict_one"] <- 0
relationTree["summarize_one","rNCV"] <- 0
relationTree["summarize_one","VarImp"] <- 0
relationTree["varImp_rNCV","rNCV"] <- 0

diag(relationTree) <- 0

library(igraph)

no.edges <- which(colSums(relationTree) == 0 & rowSums(relationTree) == 0)

noedgeTree <- relationTree[no.edges, no.edges]
relationTree <- relationTree[-no.edges, -no.edges]

noedgeTree.mat <- as.matrix(noedgeTree)
noedgeTree.graph <- graph_from_adjacency_matrix(noedgeTree.mat, mode = "directed")
noedgeTree.lay <- cbind(seq(-4,5,length.out = 8), rep(-1,8))


relationTree.mat <- as.matrix(relationTree)
relationTree.graph <- graph_from_adjacency_matrix(relationTree.mat, mode = "directed")
relationTree.lay.tree <- layout_as_tree(relationTree.graph, root = c("summarize_one", "predict_one", "PermuPerf.rNCV", "permuPred", "ML.similarity"))

relationTree.lay.tree[,2] <- relationTree.lay.tree[,2]+1
relationTree.lay.tree[relationTree.lay.tree[,2]==1,2] <- 0
relationTree.lay.tree[c(1, 6, 12),2] <- 1

png("Function_Map_tree.png",  width = 12, height = 12, res = 300, units = "in")
plot.igraph(relationTree.graph %du% noedgeTree.graph, layout = rbind(relationTree.lay.tree, noedgeTree.lay),
            #edge.curved=rep(c(1,-1),sum(relationTree)),
            edge.color = rep(yarrr::piratepal("basel"), sum(relationTree)))
dev.off()
#
# relationTree.lay.circ <- layout_in_circle(relationTree.graph)
#
# png("Function_Map_circ.png", width = 1200, height = 1200)
# plot.igraph(relationTree.graph, layout = relationTree.lay.circ,
#             edge.curved=rep(c(-0.5,0.5),sum(relationTree)),
#             edge.color = rep(yarrr::piratepal("basel"), sum(relationTree)))
# dev.off()

# relationTree.lay.nice <- layout_nicely(relationTree.graph)

# png("Function_Map_nice.png", width = 12, height = 12, res = 300, units = "in")
# plot.igraph(relationTree.graph, layout = relationTree.lay.nice,
#             edge.curved=rep(c(-0.5,0.5),sum(relationTree)),
#             edge.color = rep(yarrr::piratepal("basel"), sum(relationTree)))
# dev.off()

# for(i in 1:dim(relationTree)[1]){
#   message(rownames(relationTree)[i])
#   message(" -- calls -- ")
#   message(paste(rownames(relationTree)[relationTree[i,] == 1], collapse = ", \n"))
#   message("\n=========================================")
# }

# plotting a simple ring graph, all default parameters, except the layout
