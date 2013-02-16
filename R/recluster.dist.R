recluster.dist <-function(mat, dist="simpson") {
	mat<-as.matrix(mat)
	if(is.numeric(mat) == "FALSE") 	stop("ERROR: not numeric matrix")
	if(prod(rowSums(mat)) == 0) stop("ERROR: empty site(s) in matrix")
	if(prod(colSums(mat)) == 0) stop("ERROR: species with 0 occurrence in matrix")

	options<-c("simpson","beta3","jaccard","sorensen","richness","nestedness")
	if (is.na(pmatch(dist,options))) {
			res <- designdist(mat, method = dist,terms = c("binary"))
			attr(res,"dist") <- dist
			return(res)
	}

	dist <- match.arg(dist)

	res <- switch(dist,
		simpson = {
			res <- designdist(mat, method = "(pmin(A,B)-J)/(pmin(A,B))",terms = c("binary"))
			attr(res,"dist") <- "simpson"
			res
		},
		beta3 = {
			res <- designdist(mat, method = "2*((pmin(A,B)-J)/(A+B-J))",terms = c("binary"))
			attr(res,"dist") <- "beta3"
			res
		},
		jaccard = {
			res <- designdist(mat, method = "(A+B-2*J)/(A+B-J)",terms = c("binary"))
			attr(res,"dist") <- "jaccard"
			res
		},
		sorensen = {
			res <- designdist(mat, method = "(A+B-2*J)/(A+B)",terms = c("binary"))
			attr(res,"dist") <- "sorensen"
			res
		},
		richness = {
			res <- designdist(mat, method = "(abs((A-J)-(B-J)))/(A+B-J)",terms = c("binary"))
			attr(res,"dist") <- "richness"
			res
		},
		nestedness = {
			res <- designdist(mat, method = "(abs(A-B)/(A+B))*(J/pmin(A,B))",terms = c("binary"))
			attr(res,"dist") <- "nestedness"
			res
		}
	)
}
