popUpWarning <- function(string) shinyalert(text = string,type = "warning",closeOnEsc = T,closeOnClickOutside = T,html=T)
popUpInfo    <- function(string) shinyalert(text = string,type = "info",   closeOnEsc = T,closeOnClickOutside = T,html=T)
popUpSuccess <- function(string) shinyalert(text = string,type = "success",closeOnEsc = T,closeOnClickOutside = T,html=T)