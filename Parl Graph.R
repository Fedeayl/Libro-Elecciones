
################### Gráficos - Integración parlamento ########################

if("ggparliament" %in% list.files(.libPaths()) == FALSE){
        devtools::install_github("robwhickman/ggparliament")
} 

library(ggplot2)
library(ggparliament)


Partidos <- c("Frente Amplio (42)", "Partido Nacional (30)", "Partido Colorado (13)", "Cabildo Abierto (10)", "Partido Independiente (1)", "Partido de la Gente (1)", "P.E.R.I (1)")
PartidosS <- c("Frente Amplio (13)", "Partido Nacional (10)", "Partido Colorado (4)", "Cabildo Abierto (3)")
Senadores <- c(13, 10, 4, 3)
Diputados <- c(42, 30, 13, 11, 1, 1, 1)

Dip_palette <- c("gray85", "gray55", "gray75", "gray45", "gray65", "gray35", "gray80" )
Sen_palette <- c("gray85", "gray55", "gray75", "gray45")

Representantes <- as.data.frame(cbind(Year = "2019",
                              country = "Uruguay",
                              house = "Representantes",
                              party_long = Partidos,
                              party_short = c("FA", "PN", "PC", "CA", "PI", "PG", "PERI"),
                              seats = Diputados, 
                              goverment = c(0,1,1,1,1,1,0),
                              colour= Dip_palette),
                        stringsAsFactors = FALSE)

RepresentantesUy <- parliament_data(election_data = Representantes,
                            type = "semicircle",
                            parl_rows = 5,
                            party_seats = as.numeric(Representantes$seats))


Senado <- as.data.frame(cbind(Year = "2019",
                              country = "Uruguay",
                              house = "Senado",
                              party_long = PartidosS[1:length(Senadores)],
                              party_short = c("FA", "PN", "PC", "CA"),
                              seats = Senadores, 
                              goverment = c(0,1,1,1),
                              colour= Sen_palette), 
                        stringsAsFactors = FALSE)

SenadoUy <- parliament_data(election_data = Senado,
                            type = "semicircle",
                            parl_rows = 3,
                            party_seats = as.numeric(Senado$seats))


SENplot <- ggplot(SenadoUy, aes(x, y, colour = party_long)) +
        geom_parliament_seats(size = 16) + 
        theme_ggparliament(legend = T) +
        scale_colour_manual(values = SenadoUy$colour,
                            limits = SenadoUy$party_long)+
        geom_highlight_government(goverment == 1, colour = "black", size = 16) +
        draw_majoritythreshold(n = 15, label = F, type = 'semicircle')+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(legend.position = 'bottom') +
        labs(colour = NULL, title = "Integración - Cámara de Senadores - Uruguay 2020",
            subtitle = "Coalición de gobierno circulada en negro",
            caption = "Fuente: elaboración propia sobre datos de la Corte Electoral")

DIPplot <- ggplot(RepresentantesUy, aes(x, y, colour = party_long)) +
        geom_parliament_seats(size = 7) + 
        theme_ggparliament(legend = T) +
        scale_colour_manual(values = RepresentantesUy$colour,
                            limits = RepresentantesUy$party_long)+
        geom_highlight_government(goverment == 1, colour = "black", size = 7)+
        draw_majoritythreshold(n = 50, label = F, type = 'semicircle')+
        theme(legend.position = 'bottom') +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(colour = NULL, title = "Integración - Cámara de Representantes - Uruguay 2020",
             subtitle = "Coalición de gobierno circulada en negro",
             caption = "Fuente: elaboración propia sobre datos de la Corte Electoral")

DIPplot



SENplot







