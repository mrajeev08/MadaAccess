## getting min ttimes
catch_mat <- read.csv("output/catch_mat.csv", row.names = 1)

min <-apply(catch_mat, 1, function (x) (range(x[is.finite(x)])[1]))

inds <-apply(catch_mat, 1, function (x) {
  which(x == range(x[is.finite(x)])[1], arr.ind=TRUE)[1]
})

catchments <- as.data.frame(cbind(rownames(catch_mat), colnames(catch_mat)[inds], min))

write.csv(catchments, "output/catchments2.csv")
