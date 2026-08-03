cross_ <- function(x= vec_(),y=rules_()){
  outer(x,y, paste0)
}

vec_ <- function(x=big4_()){
 setNames(x$value, x$name)
}

rules_ <- function(){
  agent <- c("miet_t2","kauf_t2","kauf_haus")
  names <- c("m2","k2","kh")
  setNames(letters[1:length(agent)],agent)
  setNames(names,agent)
}

big4_ <- function(x = db_()){
  x |>
    dplyr::filter(stringr::str_detect(type, "^portal90"))
}

spot_ <- function(x = db_()){
  x |>
    dplyr::filter(stringr::str_detect(type, "^spot"))
}

db_ <- function() {
  data <-tibble::tribble(
    ~name, ~value, ~type, ~suchagent, 
    "willhaben ",   1, "portal90", list("miet_T2,kauf_T2"),
    "immoscout24",   2,  "portal90", NA,
    "immowelt",   3,  "portal90",NA,
    "der_standard",   4,  "portal90",NA,
    "immo.at",   5,  "portal90",NA,
    "immokralle",   1,  "portal",NA,
    "flatbee",   3,  "portal",NA,
    "lystio",   11,  "portal_active",NA,
    "K&P immo",   3, "agentTop",NA,
    "Remax NPK",   3, "agentTop",NA,
    "FullHouseImmo",   3, "agent",NA,
    "Golden immo",   3, "agent",NA,
    "sREAL",   3, "agent",NA,
    "Raiffeisen",   3, "agent",NA,
    "Kompagnon",   3, "agent",NA,
    "Immo Consulting",   3, "agent",NA,
    "Schick",   3, "agent",NA,
    "Russinov",   3, "agent",NA,
    "Nageler",   3, "agent",NA,
    "Neuhauser",   3, "agent",NA,
    "perkonig",   3, "agent",NA,
    "Fischer",   3, "agent",NA,
    "seeRaum",   3, "agent",NA,
    "RFS",   3, "misc",NA,
    "Riedergarten",   3, "misc",NA,
    "JKM",   3, "bauträger",NA,
    "Bazar.at",   3, "portal",NA,
    "IMMO.at",   3, "portal",NA,
    "keinmakler",   3, NA,NA,
    "Airbnb",   3, NA,NA,
    "Booking.com",   3, NA,NA,
    "Flatio",   3,"6months",NA,
    "HousingAnywhere",   3, "6months",NA,
    "Spotahome",   3,"6months",NA,
    "Uniplaces",   3, "6months",NA,
    "WG‑Gesucht.at",   3, NA,NA,
    "Urlaub am Bauernhof",   3, NA,NA,
    "Pensionen & Gästehäuser",   3, NA,NA,
    "Monteurzimmer",   3, NA,NA,
    "niceflats",   3, NA,NA,
    "Pensionen & Gästehäuser",   3, NA,NA,
    "KURZZEiTmiete",   3, NA,NA,
    "cozycozy",   3, NA,NA,
    "vacationRenter",   3, NA, NA,
    "Landskron|Treffen|Warmbad|St.Magdalen",   3, "spot_vil", NA,
    "Annabichl|Waidmannsdorf|St.Peter|Viktring",   3, "spot_kla",NA
  ) |> data.table::as.data.table()
  return(data)
}

