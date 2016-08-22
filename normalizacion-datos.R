# Curso N2

# Convierto en variables Dummies al atributo relevado como:
#   "En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento"

redes.social=data.frame(Amazon=rep(0,30),Gmail=rep(0,30),Facebook=rep(0,30),Twitter=rep(0,30),Spotify=rep(0,30),LinkedIn=rep(0,30),Otro=rep(0,30))

redes.social[grep("Amazon",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Amazon=1
redes.social[grep("Gmail",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Gmail=1
redes.social[grep("Facebook",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Facebook=1
redes.social[grep("Twitter",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Twitter=1
redes.social[grep("Spotify",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Spotify=1
redes.social[grep("LinkedIn",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$LinkedIn=1
redes.social[grep("INTA",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Otro=1
redes.social[grep("Scoop",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Otro=1
redes.social[grep("Whast",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Otro=1
redes.social[grep("Youtube",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Otro=1
redes.social[grep("Yammer",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Otro=1
redes.social[grep("ninguna",encuesta30u$X.En.cuales.de.las.siguientes.redes.sociales.tiene.iniciada.sesión.en.este.momento.),]$Otro=1

encuesta.dummy=data.frame(encuesta30u,redes.social)

# Las respuestas 1, 2 y 3 fueron pasadas a formato json donde solo quedaron los titulos de cada resultado.
#   - atletismo.txt
#   - florencia.txt
#   - palermo.txt

# Proceso los archivos de respuestas. También convierto los resultados esos en variables dummies
library("rjson", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

json2df=function(file.name){
  id=c();res=c()  
  result <- fromJSON(file = file.name)
  for(r in result){
    id = c(id, r$id)
    res = c(res,r$resultado)
  }
  return(data.frame(id,res))
}

result.atle=json2df("atletismo.txt")
result.flor=json2df("florencia.txt")
result.pale=json2df("palermo.txt")

result2dummies=function(df.resp, items, txt_consulta){
  # armo el DF dummies 
  dummies=data.frame(rep(0,30))
  nombres=c('')
  for(id in df.resp$id){
    nombres=c(nombres, paste(txt_consulta,id,sep=""))
    dummies=data.frame(dummies,  rep(0,30))
  }
  names(dummies)=nombres
  dummies=dummies[-c(1)]
  
  # Ahora recorro las respuesta y marco cuales resultados están en cada respuesta. Y grepearla!
  #for(item in items){ # Estas son todas las respuestas
    for(id in 1:nrow(df.resp)){ # Los resultados diferentes para cada query
      print(id)
      n_matches=agrep(as.character(df.resp[id,]$res), as.character(items), max.distance = 0.0)
      if(length(n_matches) > 0){
        dummies[n_matches,][id] = 1  
      }
    }
  #}
  return(dummies)
} # Fin 
dummies.atle=result2dummies(result.atle,encuesta30u$Respuestas.consulta.1,"C1.")
dummies.flor=result2dummies(result.flor,encuesta30u$Respuestas.consulta.2,"C2.")
dummies.pale=result2dummies(result.pale,encuesta30u$Respuestas.consulta.3,"C3.")

encuesta.dummy=data.frame(encuesta.dummy, dummies.atle, dummies.flor, dummies.pale)
encuesta.dummy.abreviado=encuesta.dummy[-c(5,8,9,10)]


km=kmeans(encuesta.dummy.abreviado[-c(1,2,3,4,5,6)], 2, nstart = 100)
enc.du.km=data.frame(encuesta.dummy.abreviado, km$cluster)

parallel(enc.du.km)
parcoord(encuesta.dummy.abreviado[-c(1,2,3,4,5,6)])

encuesta.dummy.abreviado[grep("Arg",encuesta.dummy.abreviado$País, ignore.case = T),]$País="Argentina"

write.csv(encuesta.dummy.abreviado, file = "encuesta_procesada.csv")


