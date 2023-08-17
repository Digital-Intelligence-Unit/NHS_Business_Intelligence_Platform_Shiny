
# clear up compiler.. looks like it's not working the way it should work but somehow it all works at the end
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")

if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC",
        "CXX14 = g++ -std=c++1y", 
	    file = M, sep = "\n", append = TRUE)


# clear environment
if (file.exists(".RData")) file.remove(".RData")

# install BH otherwise it would complain with the errors
install.packages('https://cran.r-project.org/src/contrib/Archive/BH/BH_1.62.0-1.tar.gz')

# install remotes to pull rstan from the source
install.packages("remotes", type = "source")

library('remotes')
remotes::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan", build_opts = "")

# somehow prophet installs correctly (ignore all the red things)
library("rstan")
install.packages('prophet', type="source")

library('prophet')