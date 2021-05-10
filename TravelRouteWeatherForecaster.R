
# Load required libraries
library(ggmap)
library(stringr)
library(owmr)

# Set up working directory
setwd("~")
setwd("TrafficRouteWeatherForecaster")

# Set Open Weather Map API
Sys.setenv(
  OWM_API_KEY = 
    "xxxxxxxx"
  )

# Shipping Origin
origin1 = 
  "1520 Luna Rd, Carrollton, TX 75006"

origin2 = 
  "4779 Hanoverville Rd, Bethlehem, PA 18020"

origin3 = 
  "164 W 31st St, Chattanooga, TN 37410"

# Register Google Cloud Key
register_google(
  key = 
    "xxxxxxxx"
)

# Read in Test Data
orders = 
  read.csv("input/Orders.csv")

# Add a leading zero to 4-digit ZIPs
orders$Postal.Code = 
  sapply(
    orders$Postal.Code, 
    function(x){
      if(
        nchar(x) < 5
      ){
        paste0(0, x)
      } 
      else{
        x
      }
    }
  )

orders$Shipping.Address = 
  paste(
    orders$Address.Line.1, 
    orders$City, 
    orders$State, 
    orders$Postal.Code
  )

shipping_addresses = 
  orders$Shipping.Address

order_numbers = 
  orders$Order.Number

# A function that calculates the shipping warehouse closest to the order address
calculate_optimal_origin = 
  function(x){
  
    
    #### Origin 1 ####
    
    origin1_dist = 
      mapdist(
        from = 
          origin1, 
        to = 
          x
        ) %>%
      select(
        miles,
        hours
      )
    
    origin1_dist$shipping_origin =
      origin1
    
    origin2_dist = 
      mapdist(
        from = 
          origin2, 
        to = 
          x
      ) %>%
      select(
        miles,
        hours
      )
    
    origin2_dist$shipping_origin =
      origin2
    
    origin3_dist = 
      mapdist(
        from = 
          origin3, 
        to = 
          x
      ) %>%
      select(
        miles,
        hours
      )
    
    origin3_dist$shipping_origin =
      origin3
    
    origin_dists = 
      rbind(
        origin1_dist,
        origin2_dist,
        origin3_dist
      )
    
    origin = 
      origin_dists %>%
        filter(
          hours == min(hours)
          ) %>% 
      select(
        shipping_origin
      )
  
    
    return(origin)
  }

# Apply calculate_optimal_origin() to each shipping address
shipping_warehouse = 
  lapply(
    shipping_addresses, 
    calculate_optimal_origin
    )

# Transform shipping_warehouse into a dataframe
shipping_origin = 
  as.data.frame(
    rbindlist(
      shipping_warehouse
      )
    )

# Assign shipping warehouse origins to a vector
shipping_warehouses =
  shipping_origin$shipping_origin

# Add the shipping warehouse origins to the 'orders' dataframe
orders$Shipping.Warehouse = 
  shipping_warehouses

# Pull Driving Route from Point of Origin to Destination
get_shipping_routes = 
  function(x, y, z){
    
    routes = 
      route(
        from =
          x, 
        to =
          y,
        structure = 
          "legs"
      )

    # Pull all start points for longitude and latitude
    start = 
      routes %>%
      select(
        start_lon,
        start_lat,
      )
    
    # Rename the column names
    colnames(start) = 
      c(
        "lon",
        "lat"
      )
    
    # Pull all end points for longitude and latitude
    end = 
      routes %>%
      select(
        end_lon,
        end_lat
      )
    
    # Rename the column names
    colnames(end) = 
      c(
        "lon",
        "lat"
      )
    
    # Combine the longitude-latitude coordinate 
    coordinates = 
      as.data.frame(
        rbind(
          start,
          end
        )
      )
    
    coordinates = 
      round(coordinates, 2)
    
    coordinates = 
      unique(coordinates)
    
    
    res <- 
      mapply(
        FUN = 
          function(lon, lat){
            revgeocode(
              c(
                lon, 
                lat
                ), 
              output = 
                "address"
              )
            },
        coordinates$lon, 
        coordinates$lat
        )
    
    zip = 
      str_extract(
        res, 
        "\\d{5},"
        )
    
    zip = 
      unique(zip)
    
    zip = 
      paste0(
        zip, 
        " US"
        )
    
    #### Get Weather Forecast Function ####
    getForecast = 
      function(x){
        
        route_forecasts = 
          get_forecast(
            x, 
            cnt = 
              40, 
            units = 
              "imperial"
          )
        
        route_forecasts_2 = 
          route_forecasts$list
        
        route_forecasts_2$zip = 
          x
        
        return(route_forecasts_2)
        
      }
    
    forecasts = 
      lapply(
        zip, 
        getForecast
        )
    
    forecasts = 
      rbindlist(forecasts, fill = T)
    
    forecasts = 
      forecasts %>%
      group_by(
        zip
      ) %>%
      summarize(
        max_temp = 
          max(
            main.temp_max
          )
      )
    
    forecasts$exceeds_max_temp = 
      ifelse(
        forecasts$max_temp >= 75, 
        TRUE, 
        FALSE
      )
    
    forecasts$supplement_form = 
      ifelse(
        forecasts$exceeds_max_temp == FALSE, 
        "bottle", 
        "blister pack"
      )
    
    forecasts$supplement_form = 
      ifelse(
        any(
          forecasts$exceeds_max_temp == TRUE
        ), 
        "bottle", 
        "blister"
      )
    
    forecasts$Order.Number = 
      z

    data_to_append_to_order = 
      forecasts %>%
      select(
        Order.Number, 
        supplement_form
      ) %>%
      group_by(
        Order.Number
      ) %>% 
      unique()
    
    return(data_to_append_to_order)
    
    
  }

df = 
  mapply(
    get_shipping_routes, 
    shipping_warehouses, 
    shipping_addresses, 
    order_numbers,
    SIMPLIFY = 
      FALSE
    )

df = 
  rbindlist(df)


