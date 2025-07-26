library(rsconnect)

rsconnect::setAccountInfo(name='8x1lwo-arani-bosire',token='4DE343CF01EB9381DFCD3F32321D4568',secret='IJPpO6NlM3v/FWP7OSzJG2EfiMVbx8MFoJD5epar')


rsconnect::deployApp(
  appDir = "~/R Studio/R_Statistical-Language/Cholera_Analysis",
  account = "8x1lwo-arani-bosire",
  server = "shinyapps.io",
  appName = "Cholera_Analysis"
)

