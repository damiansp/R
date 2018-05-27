setwd('~/Learning/R')

# Before upgrading:
tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
save(installedpkgs, file = "installed_old.rda")

# Install new version... then:
# Reload saved packages
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()

# Note: If you had any packages from BioConductor, you can update those too!
source("http://bioconductor.org/biocLite.R")
chooseBioCmirror()
biocLite()
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
for (i in 1:length(missing)) biocLite(missing[i])