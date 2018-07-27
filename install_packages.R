#making suredplyr is installed
install_load <- function (package1, ...)  {  
  
  # convert arguments to vector
  packages <- c(package1, ...)
  
  # start loop to determine if each package is installed
  for(package in packages){
    
    # if package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # if package is not installed locally, download, then load
    else {
      install.packages(package, repos="https://cloud.r-project.org/")
      do.call("library", list(package))
    }
  } 
}

isNamespaceLoaded <- function(name) is.element(name, loadedNamespaces())
install_load(c('party', 'dplyr', 'ranger', 'RODBC', 'caret', 'broom'))

package <- c('party', 'dplyr', 'ranger', 'RODBC', 'caret', 'broom')
package %in% rownames(installed.packages())
