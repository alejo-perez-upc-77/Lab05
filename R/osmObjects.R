
#' gets Coordinates of objects in the specified Location
#'
 
#' @field Location character. 
#'
#' @field OsmData list. 
#' @field Coordinates . 
#'
#' @return osmObjects class
#' @export osmObjects
#' @import osmdata ggmap ggplot2
#' 
#' 
osmObjects <- setRefClass("osmObjects",
                      # Include fields -----------
                      fields = list(Location = "character",
                                    OsmData = "list",
                                    Coordinates = "matrix"),
                      
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
                          
                          stopifnot(is.character(object), is.character(city), tolower(object) %in% names(Keys))
                          
                          Location <<- city
                          Key <- tolower(object)
                          q <- opq(bbox = city,timeout = 25) %>%
                            add_osm_feature(Keys[[Key]], Key)
                          
                          OsmData <<- osmdata_sf(q)
                          coord <- as.matrix(sf::as_Spatial(OsmData$osm_points)@coords)
                          rownames(coord) <- OsmData$osm_points$name
                          Coordinates <<- coord
                          
                          
                        },
                        plot = function(){ "Plots map with marked objects"
                          suppressMessages(map <- get_map(getbb(Location), source = "osm"))
                          suppressMessages(ggmap(map)+
                            geom_sf(data = OsmData$osm_points,
                                    inherit.aes = FALSE,
                                    colour = "red",
                                    fill = "red",
                                    size = 1)+
                            labs(x = "", y = ""))
                        },
                        # Print function
                        print = function(){ "prints coordinates of requested objects"

                          cat("Call:\n")
                          cat("osmObjects(city = ", Location, ", object = ", Key ,")\n\n", sep = "")
                          cat("Coordinates:\n")
<<<<<<< HEAD
                          write.table(as.data.frame(Coordinates))
=======
                          #cat(dimnames(RegressionCoeficients)[[1]], "\n")
                          
                          
                          write.table(as.data.frame(Coordinates))

>>>>>>> 93b156474e9bbd432aea5b537785a34a56587972
                        }
                        
                      )
)
