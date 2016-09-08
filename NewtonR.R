##Metodo Newton Raphson

NRM <- function(x){
 
#Insertar funcion en el parentesis  
  fx <- (  )
  
#Insertar derivada en el parentesis
  dfx <- (  )
  
  
  xf <- x-(fx/dfx)
  it <- 1
  
  while (abs(fx)>0.0001 & it < 50){
    
    x <- xf

#Insertar funcion en el parentesis        
    fx<-(   )
    
#Insertar derivada en el parentesis    
    dfx<-(   )
    
    
    xf<-x-(fx/dfx)
    it<-1+1
  }
    list(a = xf, iteration = it)
  
}