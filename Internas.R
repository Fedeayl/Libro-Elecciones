#########    Elecciones internas     #########

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

