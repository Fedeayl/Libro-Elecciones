
################### Gráficos - Integración parlamento ########################

if("ggparliament" %in% list.files(.libPaths()) == FALSE){
        devtools::install_github("robwhickman/ggparliament")
        } 

library(ggparliament)


Partidos <- c("Frente Amplio", "Partido Nacional", "Partido Colorado", "Cabildo Abierto", "Partido Independiente", "Partido de la Gente", "P.E.R.I")
Senadores <- c(13, 10, 4, 3)
Diputados <- c(42, 30, 13, 11, 1, 1, 1)

Dip_palette <- gray.colors(length(Diputados), start = 0.3, end = 0.9, gamma = 2.2)
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
                            parl_rows = 3,
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
        geom_parliament_seats(size = 12) + 
        # add bar showing proportion of seats by party in legislature
        geom_parliament_bar(colour = colour, party = party_long, label = T) + 
        theme_ggparliament(legend = T) +
        scale_colour_manual(values = SenadoUy$colour,
                            limits = SenadoUy$party_long)


DIPplot <- ggplot(RepresentantesUy, aes(x, y, colour = party_long)) +
        geom_parliament_seats(size = 7) + 
        # add bar showing proportion of seats by party in legislature
        geom_parliament_bar(colour = colour, party = party_long, label = T) + 
        theme_ggparliament(legend = T) +
        scale_colour_manual(values = RepresentantesUy$colour,
                            limits = RepresentantesUy$party_long)



SENplot 
DIPplot






