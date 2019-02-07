#Deploy app
if(!require(rsconnect)) { install.packages("rsconnect"); require(rsconnect)}

#Go to your Shiny.io account and get a token with secret
#Paste token code into the console to connect Rstudio to Shiny.op

deployApp()