#========#
# Function which creates a matrix of life years remaining at each age for a given country. 
#========#

# function to create discounted life year remaning.
f_dlyr <- function(country,d.r){
  
  # Get survival probabilities for each year from 1 to 84
  v.Ps <- t(1- who_mort[country,]) %>% 
    rep(each=5) %>%  tail(., -1)
  
  # Create matrices for discount rates and life years remaining.
  m.dr <- m.lyr <- matrix(data = NA,
                          nrow = 100,
                          ncol = length(v.Ps),
                          dimnames = list(1:100))
  
  # Create expected life years per year from 1-85
  for (a in 1:length(v.Ps)){
    
    for(x in 1:length(v.Ps)){
      if(x>=a){
        m.lyr[x,a] <- prod(v.Ps[a:x])
      }else{
        m.lyr[x,a] <- 0
      }
    }
  }
  
  # create expected life years from 86-100 within the same matrix
  s.clyr85 <- floor(lyr85[country,])  # number of complete years remaining.
  
  m.lyr[(85:(85+s.clyr85-1)),]  <-  matrix(m.lyr[84,],ncol = 84 ,nrow = s.clyr85, byrow="T") # add the expected life years remaining above 85 at each age. 
  m.lyr[(85+s.clyr85),]         <-  m.lyr[84,] * (lyr85[country,] - s.clyr85) 
  m.lyr[(85+s.clyr85+1):100,]   <-  0                                           
  
  #create a matrix of discount rates using a similar method.
  for(a in 1:ncol(m.lyr)){
    m.dr[a:nrow(m.lyr),a] <- 1/(1+d.r)^(0:(nrow(m.lyr)-a))
  }
  
  m.dr[is.na(m.dr)] <- 0 # to set NAs to zero.
  
  # multiply the matrix of discount rates by the matrix of expected life by the matrix of discount rates
  d.lyr <- m.lyr * m.dr
  
  return(colSums(d.lyr))
}