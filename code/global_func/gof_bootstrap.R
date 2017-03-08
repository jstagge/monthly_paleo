# *------------------------------------------------------------------
# | FUNCTION NAME: gof_bootstrap
# | FILE NAME: gof_bootstrap.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        fit - a fitdistrplus object
# |                n_sims - number of replications, defaults to 5e4 (50,000)
# |                parallel - TRUE or FALSE option to run in parallel
# |                
# |     Out:       gof_result - a list with KS, AD, and CVM test statistic and p-value
# | 
# |     Desc:      This function was created because published p-values for the 
# |					Kolmogorov-Smirnov, Anderson-Darling and Cramer-von-Mises tests 
# |					are not valid if the distribution is estimated from the data.  
# |				   This function uses bootstrapping to obtain the p-value based on
# |					the distribution and parameters.
# |                
# *------------------------------------------------------------------



#########################
###  This is the primary function to run
#########################

gof_bootstrap <- function(fit, n_sims=5e4, parallel=FALSE) {
	### Find the name of the distribution and create functions for this distribution
	distr <- fit$distname
	rand_distr <- match.fun(paste("r",distr,sep=""))
	p_distr <- match.fun(paste("p",distr,sep=""))

	### Create a list to hold the parameters for random. Loop through the number 
	### of estimated parameters, attach them to the list and rename them to match the function
	param_list <- list()
	for (j_param in seq(1,length(fit$estimate))) {
		param_name <- names(fit$estimate)[j_param]
		param_list[param_name] <- fit$estimate[j_param]
	}

	### Replicate generating random values from distribution and calculating gof test
	### statistics n_sims times.  Either regular or in parallel.  On my laptop with
	### 50,000 replications and 4 cores, parallel cut time by 40%
	if (parallel == FALSE) {
		### Run without parallel
		stats <- sapply(1:n_sims, gof_replicate, param_replicate=param_list, n=fit$n, distr_name=distr)
	} else {
		### Run with parallel
		### Create clusters
		library(parallel)
		cl <- makeCluster(detectCores()-1)  
		### Export gof_stats function to clusters
		clusterExport(cl,c("gof_stats"))
		### Then parallel replicate
		stats <- parSapply(cl,1:n_sims, gof_replicate, param_replicate=param_list, n=fit$n, distr_name=distr)
		### Stop the cluster
		stopCluster(cl)
	}

	### Extract the test stats
	ks_stats <- unlist(stats[1,])
	ad_stats <- unlist(stats[2,])
	cvm_stats <- unlist(stats[3,])
	
	### Build an empirical cumulative distribution for each stat
	ks_ecdf <- ecdf(ks_stats)
	ad_ecdf <- ecdf(ad_stats)
	cvm_ecdf <- ecdf(cvm_stats)

	### Calculate goodness of fit stats for original data
	gof_result <- gof_stats(x=fit$data, dist_name=distr, param_list) 

	### Calculate p values based on these test statistics
	gof_result["ks_p"] <- 1 - ks_ecdf(gof_result$ks_stat)
	gof_result["ad_p"] <- 1 - ad_ecdf(gof_result$ad_stat)
	gof_result["cvm_p"] <- 1 - cvm_ecdf(gof_result$cvm_stat)
	
	return(gof_result)
}



#########################
###  This is the function to calculate KS, AD, and CVM.  Can be used separately also
#########################

gof_stats <- function(x, dist_name, param_list) {
	require(goftest)
	gof_results <- list()
	
	### Check for duplicates in x, if so, apply the machine smallest number to
	### break ties
	if (sum(duplicated(x)) > 0) {
		dup_test <- duplicated(x)
		dup_values <- x[dup_test]
		x[dup_test] <- dup_values + rnorm(length(dup_values),0,.Machine$double.eps*5)
	}
		
	### Run the Kolmogorov-Smirnov test. Prepare the parameters
	run_list <- param_list
	run_list$x <- x
	run_list$y <- paste0("p",dist_name)
	
	gof_results["ks_stat"] <- as.numeric(do.call(ks.test, run_list)$statistic)
	
	### Run the Anderson-Darling and Cramer von Mises tests. 
	### Requires different input format
	run_list <- param_list
	run_list$x <- x
	run_list$null <- paste0("p",dist_name)
	
	gof_results["ad_stat"] <- as.numeric(do.call(ad.test, run_list)$statistic)
	gof_results["cvm_stat"] <- as.numeric(do.call(cvm.test, run_list)$statistic)
	
	return(gof_results)
	}



#########################
###  This is the function that generates random samples and returns the
###  goodness of fit test statistics
#########################
gof_replicate <- function(i, param_replicate, n, distr_name) {
	rand_distr <- match.fun(paste("r",distr_name,sep=""))
	
	rand_param <- param_replicate
	rand_param["n"] <-n
	
	r <- do.call(rand_distr,rand_param)
	return(gof_stats(x=r, dist_name=distr_name, param_replicate))
}

