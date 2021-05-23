EMM=function(x,n)   
{
	set.seed(1234)
	ret.em <- init.EM(x, nclass = n, method = "em.EM")
	
	#ret.Rnd <- init.EM(x, nclass = n, method = "Rnd.EM", EMC = .EMC.Rnd)
	#ret.Rndp <- init.EM(x, nclass = n, method = "Rnd.EM", EMC = .EMC.Rndp)
	
	#emobj <- simple.init(x, nclass = n)
	#ret.init <- emcluster(x, emobj, assign.class = TRUE)
	
	#ret=em.EM(x, nclass = n, lab = NULL, EMC = .EMC ,stable.solution = TRUE, min.n = NULL, min.n.iter = 10)
	
	#ret=rand.EM(x, nclass = n, lab = NULL, EMC = .EMC.Rnd,stable.solution = TRUE, min.n = NULL, min.n.iter = 10)
	
	#ret=exhaust.EM(x, nclass = n, lab = NULL,EMC = .EMControl(short.iter = 1, short.eps = Inf),method = c("em.EM", "Rnd.EM"),stable.solution = TRUE, min.n = NULL, min.n.iter = 10)
	#ret <- pmclust(as.matrix(x),K=n)
	#ret <- init.EM(x, nclass = n)
	#ret.new <- assign.class(x, ret, return.all = FALSE)
	
	#ret=starts.via.svd(x, nclass = n, method = "em",EMC = .EMC)
	#ret=emgroup(x, nclass = n, EMC = .EMC)
	#emobj=emgroup(x, nclass = n, EMC = .EMC)
	#ret=summary(emobj)
	
	#emobj <- simple.init(x, nclass = n)
	#emobj <- shortemcluster(x, emobj)
	#summary(emobj)
	#ret <- emcluster(x, emobj, assign.class = TRUE)
	
	
	return(ret.em$class)
}


