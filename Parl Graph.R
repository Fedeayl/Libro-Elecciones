
################### Gráficos - Integración parlamento ########################

if("ggparliament" %in% list.files(.libPaths()) == FALSE){
        devtools::install_github("robwhickman/ggparliament")
        } 

library(ggparliament)


Partidos <- c("Frente Amplio", "Partido Nacional", "Partido Colorado", "Cabildo Abierto", "Partido Independiente", "Partido de la Gente", "P.E.R.I")
Senadores <- c(13, 10, 4, 3)
Diputados <- c(42, 30, 13, 11, 1, 1, 1)

Dip_palette <- gray.colors(length(Diputados), start = 0, end = 1, gamma=1)
Sen_palette <- gray.colors(length(Senadores), start = 0.3, end = 0.9, gamma = 2.2)

Representantes <- as.data.frame(cbind(Year = "2019",
                              country = "Uruguay",
                              house = "Representantes",
                              party_long = Partidos,
                              party_short = c("FA", "PN", "PC", "CA", "PI", "PG", "PERI"),
                              seats = Diputados, 
                              goverment = c(0,1,1,1,1,1,0),
                              colour= Dip_pallete),
                        stringsAsFactors = FALSE)

RepresentantesUy <- parliament_data(election_data = Representantes,
                            type = "semicircle",
                            parl_rows = 5,
                            party_seats = as.numeric(Representantes$seats))


Senado <- as.data.frame(cbind(Year = "2019",
                              country = "Uruguay",
                              house = "Senado",
                              party_long = Partidos[1:length(Senadores)],
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
        # add bar showing proportion of seats by party in legislature
        geom_parliament_bar(colour = colour, party = party_long, label = T) +
        theme_ggparliament(legend = T) +
        scale_colour_manual(values = SenadoUy$colour,
                            limits = SenadoUy$party_long)+
        geom_highlight_government(goverment == 1, colour = "black", size = 16) +
        draw_majoritythreshold(n = 15, label = F, type = 'semicircle')+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(legend.position = 'bottom') +
        labs(colour = NULL, title = "Cámara de Senadores", 
             subtitle = "Coalición de gobierno circulada en negro")


DIPplot <- ggplot(RepresentantesUy, aes(x, y, colour = party_long)) +
        geom_parliament_seats(size = 7) + 
        # add bar showing proportion of seats by party in legislature
        geom_parliament_bar(colour = colour, party = party_long, label = T) + 
        theme_ggparliament(legend = T) +
        scale_colour_manual(values = RepresentantesUy$colour,
                            limits = RepresentantesUy$party_long)+
        geom_highlight_government(goverment == 1, colour = "black", size = 7)+
        draw_majoritythreshold(n = 50, label = F, type = 'semicircle')+
        theme(legend.position = 'bottom') +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(colour = NULL, title = "Cámara de Representantes", 
             subtitle = "Coalición de gobierno circulada en negro", caption = "center")


SENplot
DIPplot






