unloadRecursive = function(namespace = NULL, lib_error_message = NULL){

  #Types of "Unloading/Loading Errors"
  #1:
    # > message = try(library("rlang", lib.loc = r"(C:\Users\<User>\Desktop\titanic-r-master\lib\rlang_1.1.0)", character.only = TRUE), silent = TRUE)
    # > message
    # [1] "Error in value[[3L]](cond) : \n  Paket ‘rlang’ Version 1.0.6 kann nicht entladen werden:\n Error in unloadNamespace(package) : Namensraum ‘rlang’ ist importiert von ‘lifecycle’, ‘vctrs’, ‘pillar’ und kann deshalb nicht entladen werden\n\n"
    # attr(,"class")
    # [1] "try-error"
    # attr(,"condition")
    # <simpleError in value[[3L]](cond): Paket ‘rlang’ Version 1.0.6 kann nicht entladen werden:
    #  Error in unloadNamespace(package) : Namensraum ‘rlang’ ist importiert von ‘lifecycle’, ‘vctrs’, ‘pillar’ und kann deshalb nicht entladen werden

  #2:
    #...

  #Define namespace to be recursive unloaded given the error of a library()-function
  if(!is.null(lib_error_message)){
    #Check type of library error:
    if(grepl("Namensraum (.*?) ist bereits geladen, aber", lib_error_message)){
      conf = regmatches(lib_error_message, gregexpr("Namensraum (.*?) ist bereits geladen, aber", lib_error_message, perl = TRUE))[[1]]
    }
    if(grepl("Namensraum (.*?) ist importiert von", lib_error_message)){
      conf = regmatches(lib_error_message, gregexpr("Namensraum (.*?) ist importiert von", lib_error_message, perl = TRUE))[[1]]
    }

    #if there is a conflict of loaded & required version -> try to unload namespace
    if(!identical(character(0), conf)){
      conf = try(regmatches(conf, gregexpr("(?<=‘)\\s*(.*?)\\s+(?=ist)", conf, perl = TRUE))[[1]], silent=T)
      conf_p = trimws(strsplit(conf, "’ ")[[1]][1])
    }
  }

  #Define namespace to be recursive unloaded given its name
  if(!is.null(namespace)){
    conf_p = namespace
  }

  #Recursive unloading namespaces until initial namespace could be unloaded
    conflict = T
    while(conflict){
      conf_namespaces = try(unloadNamespace(conf_p), silent = TRUE)
      #if namespace is loaded by other namespaces -> try to unload these namespaces first
      if(!is.null(conf_namespaces) & any(grepl("importiert von",x = conf_namespaces))){
        conf_namespaces = regmatches(conf_namespaces, gregexpr("(?<=importiert von)\\s*(.*?)\\s+(?=und kann)", conf_namespaces, perl = TRUE))[[1]]
        for(conf_namespace in strsplit(gsub("‘|’","", conf_namespaces), ",")[[1]]){
          unloadRecursive(namespace = trimws(conf_namespace))
        }
      } else {
        #if initially conflicting namespace is unloaded -> leave the while-loop
        conflict = FALSE
        cat("Unloaded Namespace: ", conf_p, "\n")
      }
    }

  return(FALSE)
}
