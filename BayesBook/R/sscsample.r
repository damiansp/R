sscsample  =  function (size, n.samples, sample.type = "simple", x = NULL, strata = NULL
                       , cluster = NULL, ret = FALSE, print = TRUE)
{
    ## Written initially by:
    ## James M. Curran,
    ## Dept. of Statistics, University of Waikato
    ## Hamilton, New Zealand
    ##
    ## Modified, corrected and improved by:
    ## Janko Dietzsch
    ## Proteomics Algorithm and Simulation
    ## Zentrum f. Bioinformatik Tuebingen
    ## Fakultaet f. Informations- und Kognitionswissenschaften
    ## Universitaet Tuebingen
    ## R. Mark Sharp
    ## Southwest National Primate Center
    ## Southwest Foundation for Biomedical Research

    group.idx.by.name  =  function(name,names.vec,idx){
        return(idx[names.vec == name])
    }

    draw.stratum  =  function(thresholds) {
        r  =  runif(1)
        for (i in 1:length(thresholds))
            if (r < thresholds[i]){
                stratum  =  i; break;
            }
        return(stratum)
    }

    data(sscsample.data)
    sscsample.data = sscsample.data

    if (is.null(x))
        x  =  sscsample.data$value

    nx  =  length(x)

    if (size > nx)
        stop("Sample size must be less than population size")

    if (is.null(strata))
        strata  =  sscsample.data$stratum

    strata.names  =  unique(strata)
    n.strata  =  length(strata.names)

    if (nx != length(strata))
        stop("The length of the strata and data vectors must be equal")

    if (is.null(cluster))
        cluster  =  sscsample.data$cluster

    n.clusters  =  length(unique(cluster))

    if (nx != length(cluster))
        stop("The length of the cluster and data vectors must be equal")

    samples  =  matrix(0, nrow = size, ncol = n.samples)

    if(sample.type == "stratified" | sample.type == 2){
        idx.vec  =  1:nx
        stratified.data  =  lapply(strata.names,group.idx.by.name,
                                  names.vec=strata,idx=idx.vec)
        names(stratified.data)  =  strata.names

        sample.strata.size  =  size * sapply(stratified.data, length) / nx
        sample.strata.units  =  floor(sample.strata.size) ## integer part of units
        sample.strata.fractions  =  sample.strata.size - sample.strata.units ## determine the fractional unit parts for every stratum
        sample.unit.residuals  =  sum(sample.strata.fractions) ## how many remaining units are determined by fractions

        ## prepare the random draw of the residual units
        if (sample.unit.residuals > 0){
            for (i in 2:length(sample.strata.fractions))
                sample.strata.fractions[i]  =  sample.strata.fractions[i] + sample.strata.fractions[i-1]
            sample.strata.thresholds  =  sample.strata.fractions / sample.unit.residuals
        }
    }else if (sample.type == "cluster" | sample.type == 3) {
        cluster.names  =  unique(cluster)
        cluster.names  =  sort(cluster.names) ## clustered.data should be ordered to be useful inside the 'sampling-loop'
        idx.vec  =  1:nx
        clustered.data  =  lapply(cluster.names,group.idx.by.name,names.vec=cluster,idx=idx.vec)
        names(clustered.data)  =  cluster.names
    }
    for (r in 1:n.samples) {
        if (sample.type == "simple" | sample.type == 1){
            sample.idx  =  sample(1:nx, size)
        }else if (sample.type == "stratified" | sample.type == 2){
            for (stratum in 1:n.strata) { ## Sample the whole units from all strata
                if (stratum == 1)
                    sample.idx  =  sample(stratified.data[[stratum]],
                                         sample.strata.units[stratum])
                else
                    sample.idx  =  c(sample.idx, sample(stratified.data[[stratum]],
                                                        sample.strata.units[stratum]))
            }
            if (sample.unit.residuals > 0) { ## Are there fractional parts?
                for (i in 1:sample.unit.residuals) { ## sample the residual units randomly but according the fractions
                    selected.stratum  =  draw.stratum(sample.strata.thresholds)
                    repeat { ## draw a unit that was not already selected
                        draw.idx  =  sample(stratified.data[[selected.stratum]],1)
                        if (! draw.idx %in% sample.idx) break ## Have we already sampled this unit?
                    }
                    sample.idx  =  c(sample.idx, draw.idx)
                }
            }
        }
        else if (sample.type == "cluster" | sample.type == 3) {
            ## This part samples as many clusters as necessary to reach the specified
            ## sampling size.
            sample.idx  =  vector(mode="numeric")
            temp.cluster.names  =  cluster.names
            while (size > length(sample.idx)) {
                sampled.cluster.name  =  sample(temp.cluster.names,1)
                rest  =  size - length(sample.idx)
                if (length(clustered.data[[sampled.cluster.name]]) <= rest) {
                    sample.idx  =  c(sample.idx, clustered.data[[sampled.cluster.name]])
                } else {
                    sample.idx  =  c(sample.idx, sample(clustered.data[[sampled.cluster.name]], rest))
                }
                temp.cluster.names  =  temp.cluster.names[temp.cluster.names != sampled.cluster.name]
            }
        }
        else stop(paste("Unknown sampling sample.type :", sample.type))
        samples[, r]  =  sample.idx
    }
    means  =  rep(0, n.samples)
    s.strata  =  matrix(0, nrow = n.samples, ncol = n.strata)
    sample.out  =  matrix(0, nrow = size, ncol = n.samples)

    if(print){
        cat("Sample\tMean   \tStratum 1\tStratum 2\tStratum 3\n")
        cat("------\t-------\t---------\t---------\t---------\n")
    }

    for (r in 1:n.samples) {
        idx  =  samples[, r]
        means[r]  =  mean(x[idx])
        for (j in 1:n.strata)
            s.strata[r, j]  =  sum(strata[idx] == strata.names[j])
        sample.out[, r]  =  x[idx]
       if(print){
           cat(paste(r, "\t", round(means[r], 4), "\t",
                     s.strata[r,1], "\t\t", s.strata[r, 2], "\t\t", s.strata[r, 3],
                     "\n", sep = ""))
       }
    }
    if (ret){
        cat("The argument ret is deprecated.\n")
        cat("The results are now always returned invisibly\n")
    }

    invisible(list(samples = samples, s.strata = s.strata, means = means))
}
