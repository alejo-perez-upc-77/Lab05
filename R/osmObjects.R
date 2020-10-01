
#' gets Coordinates of objects in the specified Location
#'
 
#' @field Location character. 
#'
#' @field OsmData list. 
#' @field Coordinates . 
#'
#' @return osmObjects class
#' @export osmObjects
#' @import ggplot2 methods httr sf
#' 
#' 
osmObjects <- setRefClass("osmObjects",
                      # Include fields -----------
                      fields = list(Location = "character",
                                    Coordinates = "data.frame",
                                    Key = "character"),
                      
                      # Include methods
                      methods = list(
                        # Initialization of fields 
                        initialize = function(city, object) { "constructor object = atm, bank, pharmacy, hospital, supermarket, fuel"
                          
                          Keys <- list(atm = "amenity",
                                        bank = "amenity",
                                        pharmacy = "amenity",
                                        hospital = "amenity",
                                        supermarket = "shop",
                                        fuel = "amenity" )
                          
                          stopifnot(is.character(object), is.character(city), "object is not from the list" = tolower(object) %in% names(Keys))
                          
                          Location <<- city
                          Key <<- tolower(object)
                          # q <- opq(bbox = city,timeout = 25) %>%
                          #   add_osm_feature(Keys[[Key]], Key)
                          # 
                          # OsmData <<- osmdata_sf(q)
                          # coord <- as.matrix(sf::as_Spatial(OsmData$osm_points)@coords)
                          # rownames(coord) <- OsmData$osm_points$name
                          # Coordinates <<- coord
                          
                          Coordinates <<- getElements(getBoundingBox(Location), Keys[[Key]], Key )
                          
                          
                        },
                        plot = function(){ "Plots map with marked objects"
                          
                          Points <- st_as_sf(x = Coordinates, 
                                                  coords = c("lon", "lat"),
                                                  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
                          
                          
                          if (requireNamespace("ggmap", quietly=TRUE)) {
                            suppressMessages(map <- ggmap::get_map(getBoundingBox(Location), source = "osm"))
                            suppressMessages(ggmap::ggmap(map)+
                                               geom_sf(data = Points,
                                                       inherit.aes = FALSE,
                                                       colour = "red",
                                                       fill = "red",
                                                       size = 1)+
                                               labs(x = "", y = ""))           # package foo in Suggests
                          } else {
                            warning("Would need ggmap for plot")  # message optional
                          }
                          
                        },
                        # Print function
                        print = function(){ "prints coordinates of requested objects"

                          cat("Call:\n")
                          cat("osmObjects(city = ", Location, ", object = ", Key ,")\n\n", sep = "")
                          cat("Coordinates:\n")
                          write.table(Coordinates)
                        }
                        
                      )
)


#' Get request for boundary box of the element
#'
#' @param city character  - name of the location, can be not only city
#'
#' @return matrix with coordinates
#' @export

getBoundingBox <- function(city){
  tryCatch(
    data <- GET("https://nominatim.openstreetmap.org/search?", query = list(city = city, format = "json", limit = 1)),
    error = function(x){stop("connection problem")})
  stopifnot("getbb status code is not 200" = data$status_code == 200)
  content <- content(data,"parsed")
  stopifnot("city not found" = length(content) >= 1 )
  coords <- as.numeric(content[[1]]$boundingbox)
  bbox <- matrix(nrow = 2, ncol = 2)
  bbox[2, ] <- unlist(coords[1:2])
  bbox[1, ] <- unlist(coords[3:4])
  return(bbox)
}



#' get request for element from overpass API
#'
#' @param bbox  boundaries
#' @param key   ex. amenity
#' @param value  ex. atm
#'
#' @return data frame of coordinates
#' @export


getElements <- function(bbox, key, value){ 
  
  #forming query
  bboxString <- paste("[bbox: ", bbox[2,1], ", ", bbox[1,1], ", ", bbox[2,2], ", ", bbox[1,2] ,"]" ,sep = "")
  nodeString <- paste("node[", key, "=", value, "];")
  q <- paste0("[out:json]", bboxString, "[timeout:800];(", nodeString, ");  out body;")
  
  
  # api request
  baseurl <- 'http://overpass-api.de/api/interpreter'
  
  tryCatch(
    resp <- GET(baseurl, query = list(data = q)),
    error = function(x){stop("connection problem")})
  stopifnot("getbb status code is not 200" = resp$status_code == 200)
  
  
  # parsing data
  content <- content(resp, "parsed")$elements
  data <- lapply(content, function(x){return(list(lat = x$lat, lon = x$lon))})
  return(do.call(rbind.data.frame, data))
}
