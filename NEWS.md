# pama 0.1.1 (2023-??-??)

**More consistent passing of values to nested functions**

-   Adding `parent.frame()$<obj>` to default `args` probably already defined in the enclosing function

**More flexible and structured UTL construction**

-   New function:

    -   Added the `build_package_url()` function\
        (-\> *Creates the URL of a package file for installation)*
        -   [x] Description: Added title, dependencies, description, all param's, Side effects, Return, note, seealso, keywords, & examples
        -   [x] Arg-Checks: Checking .main, .repo, .package, .version, .cran.mirror, .cran.mirror, .archiv.path
        -   [ ] Tests: ...

-   Adapted functions:

    -   [x] `find_package_version_on_cran`
        -   [x] Add `repo` to description
        -   [x] Add `repo` to *args*
        -   [x] Add arg-checks for `repo`
        -   [x] Replacing manual URL construction by function `build_package_url()` (incl. args: , `.main`, `.package` & `.version`)
    -   [x] `get_dependencies()`
        -   [x] Add `repo` to description
        -   [x] Add `repo` to *args*
        -   [x] Add arg-checks for `repo`
        -   [x] adding control flow to decide whether to untar or unzip downloaded package file
        -   [x] add `repo` arg to calls of `find_package_version_on_cran()`
    -   [x] `install_package_version()`
        -   [x] Add `repo` to description
        -   [x] Add `repo` to *args*
        -   [x] Add arg-checks for `repo`
        -   [x] add `repo` arg to calls of `find_package_version_on_cran()`
        -   [x] add `repo` arg to calls of `get_dependencies()`
        -   [x] Replacing manual URL construction by function `build_package_url()` (incl. args: `.main`, `.package` & `.version`)

**Other Changes:**

-   [x] `install_requirements()`: Added `cran.mirror`, `main.path`, `archiv.path` and `repo` to the *args* & *description* and pass the arguments to function `install_package_version` (x2)

------------------------------------------------------------------------

# pama 0.1.0 (2023-06-29)

-   Created the package ヽ(•‿•)ノ
-   Added a vignette
-   Added a manual for every function
-   Functions are available to ***ensure the reproducibility*** of ***long-lived projects***
    -   **Make your project reproducible:**:
        -   Added a `apply_library_version()` function
        -   Added a `create_reqirements_file()` function
    -   **Reproduce your project:**
        -   Added a `setupLibrary()` function
        -   Added a `install_requirements()` function
    -   **Work on your project:**
        -   Added a `modify_libPaths()` function
        -   Added a `install_package_version()` function
        -   Added a `library_version()` function
        -   Added a `load_requirements()` function
        -   Added a `citations2()` function
    -   Helper functions:
        -   Added a `detach_none_base()` function
        -   Added a `find_package_version_on_cran()` function
        -   Added a `get_dependencies()` function
        -   Added a `get_installed_packages()` function
        -   Added a `get_requirements()` function
        -   Added a `unloadRecursive()` function
        -   Added a `update_packages_search_path()` function
