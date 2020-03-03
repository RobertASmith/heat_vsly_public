# INSTALL PACKAGES (Function courtesty of Paul Schneider)

f_install_n_load <- function(packages){    # install & load package.
  
  for(package in packages){ # for each package
    
    if(package %in% rownames(installed.packages()) == FALSE) {  # if the package is not already installed.
      
      print(paste("installing",package))  # print the necessary package.
      
      install.packages(package)           # install necessary package
    }
    eval(parse(text=paste('require(',package,')'))) # if it is already installed then load it.
  }
}