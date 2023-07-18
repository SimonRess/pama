# \<pama\>: Package Management <a href="https://github.com/SimonRess/pama"><img src="https://github.com/SimonRess/pama/blob/main/Logo.png" align="right" height="138" /></a>
*Python-style* package management tool for R. Use ***requirement.txt*** files to specify versions of packages to install (into your project folders lib) and load specific package versions in your scripts based on specified #lists in the requirement.txt. 

Making your project ***reproducible*** and ***production-ready*** was never easier :)

# Overview

### MIT License
Copyright (c) **2023 SimonRess**

**Citation:**
```
Ress, Simon (2023). pama: Python-style package management tool for R. R package
  version 0.1.1, <https://github.com/SimonRess/pama>.
```
**BibTeX entry** for LaTeX users:
```
  @Manual{,
    title = {pama: Python-style package management tool for R},
    author = {Simon Ress},
    year = {2023},
    note = {R package version 0.1.1},
    url = {https://github.com/SimonRess/pama},
  }
```

The above **copyright notice** shall be included in all
copies, substantial portions of the Software, and other artefacts made with *pama***.

### News
- Check the newest features and changes: [NEWS.md](NEWS.md)
- Newest version: [v0.1.1](https://github.com/SimonRess/pama/tree/v0.1.1)

### Description
When working on **long-life projects**, you must **ensure reproducibility** on new machines. For this, the following steps are fundamental:
-   Understand the basis for reproducibility (e.g. external connections, technologies such as R and its packages).
-   New R and/or package versions can change functions' inputs, outputs and functionality.
-   Therefore, be able to save and restore the versions of the package and of R with which it was built.
-   Allow easy access to different versions of the packages to facilitate updates

The "pama" package makes these requirements quick and easy to implement:
-   **Working on the project**
    -   Using `create_requirements()` a "requirements" file can be created that contains all necessary information (R version, package name & package version) to replicate the used packages in your R scripts.
    -   Loading package version lists from the "requirements" file is enabled using `load_requirements()`.
    -   Individual packages with specific versions can be loaded with `library_version()`.
-   **Replicating the project**
-   In the new project directory a library folder with the standard packages of the desired R version can be created with `setupLibrary()`.
-   After that, all packages with the used versions from the "requirements" files can be installed into the new library folder using `install_requirements()`.


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

# Usage
#### Take a loot at the pama vignette locally
```
help(package = "pama")
```

### 1. Make your project reproducible

#### 1.1 Creating a "requirements" file: `create_requirements()`
```
#Creating a req-file in "C:/Project" called "requirements_<date>.txt", with list structure, but no package versions
create_reqirements_file(draft = FALSE, script.paths = "./")
```

#### 1.2 Adjust package-loading in R scripts to pama-style: `apply_library_version()`
```
#Adjust package-loading within R scripts in .\Scripts to pama-style, using versions from the "requirements.txt"-file
apply_library_version(req.file.path = r"(C:\Users\...\Desktop\titanic-r-master)",
                      req.file.name = paste0("requirements_",Sys.Date(), ".txt"),
                      script.paths = r"(C:\Users\...\Desktop\titanic-r-master\Scripts)")
```

### 2. Reproducing your project

#### 2.1 Setup a project library: `setupLibrary()`
```
#Creating a "lib"-folder with all standard packages of the specified R Version (-> here specified by R.home())
setupLibrary(new.lib = "lib", main.lib.path = paste0(R.home(), "/library"))
```

#### 2.2 Installing requires packages: `install_requirements()`
```
#Install all package versions specified in "requirements_<date>.txt"-file in the WD
install_requirements(req.file.name = paste0("requirements_",Sys.Date(), ".txt"))
```

### 3. Work on your project

#### 3.1 `modify_libPaths()` function
```
modify_libPaths(reset = file.path(getwd(),"lib"))
```

#### 3.2 `install_package_version()` function
```
#Install ggplot2 version 3.0.0 into your projects lib-folder
install_package_version("ggplot2", "3.0.0", lib.install.path = file.path(getwd(),"lib"))
```

#### 3.2 `library_version()` function
```
# Load v3.4.0 of ggplot2 from folder "ggplot2_3.4.0" in search paths
library_version("ggplot2", "3.4.2")
```

#### 3.3 `load_requirements()` function
```
load_requirements(req.file.name = paste0("requirements_",Sys.Date(), ".txt"))
```

#### 3.4 `citations2()` function
```
#Creating a lib_citations_<date>.csv from the "requirements.txt" within the working directory
citations2(req.file.name = paste0("requirements_",Sys.Date(), ".txt"))
```

---

## Getting help
If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/SimonRess/pama/issues).
