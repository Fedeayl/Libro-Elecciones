###################    ELECCIONES INTERNAS     ###################

Base <- rio::import(here::here("Internas hojas_interna2019.xlsx"))
head(Base)

Datos <- Base[Base$TipoHoja == "ODN", c(1,2,4,6,7)]
Datos <- unique(Datos)

# Hojas por sublema (ODN)
sum(Datos$PartidoPolitico == "Frente Amplio")
sum(Datos$PartidoPolitico == "Partido Nacional")
sum(Datos$PartidoPolitico == "Partido Colorado")
sum(Datos$PartidoPolitico == "Cabildo Abierto")


Base2 <- rio::import(here::here("Internas totales-generales-por-circuito.xlsx"))
Datos2 <- Base2
Tabla1 <- cbind(Habilitados = xtabs(Total_Habilitados ~ Departamento, Datos2),
                Emitidos= xtabs(Total_Votos_Emitidos ~ Departamento, Datos2),
                Anulados = xtabs(Total_Anulados ~ Departamento, Datos2),
                Blanco = xtabs(Total_En_Blanco ~ Departamento, Datos2)) # Crea la base general de habilitados, blancos, etc

Base3 <- rio::import(here::here("Interna desglose-de-votos.xlsx"))
Datos3 <- Base3[,c(2,5,6,8)]
Tabla2 <- xtabs(CANTIDAD_VOTOS ~ DEPARTAMENTO + LEMA, Datos3) # Tabla de votos por lema por departamento

Tabla1 <- reshape::melt(Tabla1)
names(Tabla1) <- c("DEPARTAMENTO", "LEMA", "value")
Tabla2 <- reshape::melt(Tabla2)
Resultados <- bind_rows(Tabla1, Tabla2)

Resultados <- reshape::cast(Resultados, formula = DEPARTAMENTO~...)

# Resultados Frente Amplio por departamento
DatosFA <- Datos3[Datos3$LEMA == "Frente Amplio",]
TablaFA <- xtabs(CANTIDAD_VOTOS ~ DEPARTAMENTO + DESCRIPCIÓN_1, DatosFA) # Deagrega por candidato FA

#Resultados Partido Nacional por departamento
DatosPN <- Datos3[Datos3$LEMA == "Nacional",]
TablaPN <- xtabs(CANTIDAD_VOTOS ~ DEPARTAMENTO + DESCRIPCIÓN_1, DatosPN) # Deagrega por candidato PN

#Resultados Partido Colorado por departamento
DatosPC <- Datos3[Datos3$LEMA == "Partido Colorado",]
TablaPC <- xtabs(CANTIDAD_VOTOS ~ DEPARTAMENTO + DESCRIPCIÓN_1, DatosPC) # Deagrega por candidato PC




#################### ELECCIONES NACIONALES #####################

# Carga de datos
NacionalesHAB <- rio::import(here::here("totales-generales.xlsx")) # Datos nulos, blanco, etc
Nacionales <- rio::import(here::here("desglose-de-votos.xlsx")) # Datos por Lema

# Crear las tablas
Tabla1 <- xtabs(CantidadVotos ~ Departamento + Lema, data=Nacionales) # Tabla cruzada Depto vs Lema
Tabla1b <- cbind(EnBlanco = xtabs(TotalEnBlanco ~ Departamento, data = NacionalesHAB),
                 Anulados = xtabs(TotalAnulados ~  Departamento, data = NacionalesHAB),
                 Emitidos = xtabs(TotalVotosEmitidos ~ Departamento, data = NacionalesHAB),
                 Habilitados = xtabs(TotalHabilitados ~ Departamento, data = NacionalesHAB)
) # Tabla cruzada, blanco, anulado, por depto

Tabla1 <- as.data.frame(reshape::melt(Tabla1)) # Cambio de formato para combinar
Tabla1b <- as.data.frame(reshape::melt(Tabla1b)) 
names(Tabla1b) <- c("Departamento", "Lema", "value") # Asignar mismos nombres a Tabla2 que a Tabla1

Tabla <- rbind(Tabla1, Tabla1b) # Combinar ambas talas
Tabla <- reshape::cast(Tabla, ... ~ Lema) # Reformatear para que quede la salida deseada
Tabla2 <- dplyr::bind_rows(Tabla, colSums(Tabla), .id=NULL)
names(Tabla)

##### Se quiere obtener una tabla de votos por sublema #####

# Crear una base con hojas por sublema
Sublemas <- rio::import(here::here("integracion-de-hojas.xlsx"))
Sublemas <- cbind.data.frame(Lema = Sublemas$PartidoPolitico, Hoja=Sublemas$Numero, Sublema=Sublemas$Sublema)
Sublemas <- unique(Sublemas)
Sublemas <- Sublemas[Sublemas$Sublema != "No aplica", ]


## FRENTE AMPLIO - Crear una tabla de votos por sublema del FA
SubFA <- Sublemas[Sublemas$Lema == "Frente Amplio",]

Tabla2 <- as.data.frame(xtabs(CantidadVotos ~  Descripcion1 + Lema, data=Nacionales)) # Tabla cruzada Lema vs Hoja
SsFA <- Tabla2[Tabla2$Lema == "Partido Frente Amplio",] # Selecciona solamente las entradas "Frente Amplio"
SsFA <- SsFA[SsFA$Freq != 0,] # Elimina las entradas con ceros
names(SsFA) <- c("Hoja", "Lema","Freq") # Asigna nombres a las categorías para el merge siguiente


datos <- merge(SsFA, SubFA, by = "Hoja")
a <- dplyr::group_by(datos, Sublema, Lema.x, Lema.y)
Frente <- unique(dplyr::summarise(a, Sublema, Votos=sum(Freq)))
Frente <- Frente[c(1,4)]
Frente <- as.data.frame(dplyr::arrange(Frente, desc(Frente$Votos)))# Ordenar decreciente
Frente2<- cbind(as.data.frame(rep("Frente Amplio", length(Frente$Sublema))), Frente)
names(Frente2) <- c("Lema", "Sublema", "Votos")
Frente2


## PARTIDO NACIONAL
SubPN <- Sublemas[Sublemas$Lema == "Partido Nacional",]

Tabla2 <- as.data.frame(xtabs(CantidadVotos ~  Descripcion1 + Lema, data=Nacionales)) # Tabla cruzada Lema vs Hoja
SsPN <- Tabla2[Tabla2$Lema == "Partido Nacional",] # Selecciona solamente las entradas "Frente Amplio"
SsPN <- SsPN[SsPN$Freq != 0,] # Elimina las entradas con ceros
names(SsPN) <- c("Hoja", "Lema","Freq") # Asigna nombres a las categorías para el merge siguiente


datos <- merge(SsPN, SubPN, by = "Hoja")
a <- dplyr::group_by(datos, Sublema, Lema.x, Lema.y)
PNacional <- unique(dplyr::summarise(a, Sublema, Votos=sum(Freq)))
PNacional <- PNacional[c(1,4)]
PNacional <- as.data.frame(dplyr::arrange(PNacional, desc(PNacional$Votos)))# Ordenar decreciente
PNacional2<- cbind(as.data.frame(rep("P.Nacional", length(PNacional$Sublema))), PNacional)
names(PNacional2) <- c("Lema", "Sublema", "Votos")
PNacional2


## PARTIDO COLORADO
SubPC <- Sublemas[Sublemas$Lema == "Partido Colorado",]

Tabla2 <- as.data.frame(xtabs(CantidadVotos ~  Descripcion1 + Lema, data=Nacionales)) # Tabla cruzada Lema vs Hoja
SsPC <- Tabla2[Tabla2$Lema == "Partido Colorado",] # Selecciona solamente las entradas "Frente Amplio"
SsPC <- SsPC[SsPC$Freq != 0,] # Elimina las entradas con ceros
names(SsPC) <- c("Hoja", "Lema","Freq") # Asigna nombres a las categorías para el merge siguiente


datos <- merge(SsPC, SubPC, by = "Hoja")
a <- dplyr::group_by(datos, Sublema, Lema.x, Lema.y)
PColorado <- unique(dplyr::summarise(a, Sublema, Votos=sum(Freq)))
PColorado <- PColorado[c(1,4)]
PColorado <- as.data.frame(dplyr::arrange(PColorado, desc(PColorado$Votos)))# Ordenar decreciente
PColorado2 <- cbind(as.data.frame(rep("P.Colorado", length(PColorado$Sublema))), PColorado)
names(PColorado2) <- c("Lema", "Sublema", "Votos")
PColorado2

VotosSublemas <- dplyr::bind_rows(Frente2, PNacional2, PColorado2)




########################         REFORMA       ##############################

BaseP <- rio::import(here::here("PLEBISCITO.xlsx"))

DatosP <- BaseP[, c(3,7,10,11,12,13,14)]
names(BaseP)







########################         BALOTAJE       ##############################

















library(xlsx)
write.xlsx(Tabla2, file="Resultados.xlsx", sheetName="Nacional-Total", row.names=FALSE)
write.xlsx(VotosSublemas, file="Resultados.xlsx", sheetName="Sublemas-Total", append=TRUE, row.names=FALSE)

