library('opalr')
packageVersion('opalr')
#dotenv::load_dot_env('copd.env')

upload_opal <- function(daten, userName, password, baseURL, projectName, tableName){
  myOpalParams <-
    list(
      userName = userName,
      password = password,
      baseURL = baseURL,
      projectName = projectName,
      tableName = tableName)
  
  opalConn <- opal.login(
    username = myOpalParams$userName,
    password = myOpalParams$password,
    url = myOpalParams$baseURL,
    opts=list(ssl_verifyhost=0L, ssl_verifypeer=0L)
  )
  
  opal.table_save(
    opalConn,
    daten,
    myOpalParams$projectName,
    myOpalParams$tableName,
    policy = 'generate',
    id.name = 'id',
    overwrite=TRUE,
    force=TRUE
  )
  
  opal.logout(opalConn)
}




