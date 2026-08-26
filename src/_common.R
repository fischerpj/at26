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

vitrine_ <- function(x = db_()){
  x |>
    dplyr::filter(stringr::str_detect(type, "^agentVitrine"))
}

spot_ <- function(x = db_()){
  x |>
    dplyr::filter(stringr::str_detect(type, "^spot"))
}

locals_ <- function(x = db_()){
  x |>
    dplyr::filter(stringr::str_detect(type, "^agent_lind"))
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
    "immoi",   3, "agentVitrine","https://www.immoi.at/",
    "marktluecke",   3, "agentVitrine","https://www.marktluecke.at/",
    "Raiffeisen Kärnten",   3, "agentVitrine","https://www.raiffeisen-immobilien.at/de",
    "Remax-idea",   3, "agentVitrine","https://www.remax.at/de/ib/remax-idea-villach",
    "volksbank-kaernten",   3, "agentVitrine","https://www.volksbank-kaernten.at/immobilien/immobilien_startseite.page",
    "fischer-immobilien",   3, "agentVitrine","https://www.fischer-immobilien.at/",
    "topmiete",   3, "agentVitrine","https://topmiete.at/",
    "nageler",   3, "agentVitrine","https://nageler.biz/",
    "markerimmobilien",   3, "agentVitrine","https://www.mi.at/",
    "perkonig",   3, "agentVitrine","http://www.realitaeten-perkonig.at",
    "immobilien-at",   3, "agentVitrine","https://www.immobilien-at.com/immobilienmakler/?ma9037-realitaeten_perkonig",
    "immobilienmarkt",   3, "agentVitrine","https://www.a-immobilienmarkt.at/immobilienmakler/immobilien/?ma9037-realitaeten_perkonig",
    "schiffer",   3, "agentVitrine","https://www.schiffer-immobilien.at/immobilien",
    "sp-immo",   3, "agentVitrine","https://sp-immo.at/",
    "adunka",   3, "agentVitrine","https://www.adunka.at/leistungen/",
    "wallner",   3, "agentVitrine","https://wallner-immobilien.at/",
    "remax-friends",   3, "agentVitrine","http://www.remax-friends.at",
    "deinzuhauseimmobilien",   3, "agentVitrine","https://www.deinzuhauseimmobilien.com/",
    "willi mann",   3, "agentVitrine","https://www.wmi.at",
    "blattgold",   3, "agentVitrine","https://anleihe.kollitsch.eu/projekt-blattgold-in-klagenfurt/",
    "sreal",   3, "agentVitrine","https://www.sreal.at/de/home",
    "patronum",   3, "agentVitrine","https://patronum.at/",
    "raireal",   3, "agentVitrine",NA,
    "FullHouseImmo",   3, "agent",NA,
    "Golden immo",   3, "agent",NA,
    "sREAL",   3, "agent",NA,
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
    "Annabichl|Waidmannsdorf|St.Peter|Viktring",   3, "spot_kla",NA,
    "ERA Immobilien Villach", 3,"agent_lind", "https://era.at/de/",
  ) |> data.table::as.data.table()
  return(data)
}

