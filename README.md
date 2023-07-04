# \<pama\>: Package Management
*Python-style* package management tool for R. Use ***requirement.txt*** files to specify versions of packages to install (into your project folders lib) and load specific package versions in your scripts based on specified #lists in the requirement.txt. 

Making your project ***reproducible*** and ***production-ready*** was never easier :)

# Install <pama>
```
# Install and load required package
  if(!require("remotes")) install.packages("remotes")
  library("remotes") # includes the function install_github()
  # OR
  if(!require("devtools")) install.packages("devtools")
  library(devtools) # also includes the function install_github()

# Install <pama> from GitHub
  install_github("https://github.com/SimonRess/PaMaTo/", build_manual = TRUE, build_vignettes = TRUE) # also get the vignette
  library("pama")
```

#Usage
...
