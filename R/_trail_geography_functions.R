fxn_map <- function(.sample_trail) {
  #' View a map of a given trail
  #'
  #' @description Create a mapview map of a given trail and its identified OSM segments
  #'
  #' @param .sample_trail: sf object of osm lines associated with a given trail
  if (map_issues == TRUE) {
    mapview::mapview(
      list(
        .sample_trail,
        filter(trail_polygons, unit_id == .sample_trail$unit_id[1])
      ),
      col.regions = c("green"),
      map.types = c("OpenStreetMap", "Esri.WorldImagery")
    )
  }
}

fxn_map_add <- function(.sample_trail, .sysbikeped) {
  #' View a map of a given trail and nearby OSM line segments
  #'
  #' @description Create a leaflet map of a given trail and all nearby osm segments. The map can be used to identify OSM segments to add or remove.
  #'
  #' @param .sample_trail: sf object of osm lines associated with a given trail
  #' @param .sysbikeped: sf object of all osm lines identified for a given trail system (DNR, Greater MN, or Metro Regional)
  if (map_issues == TRUE) {
    # all osm segments initially identified for unit_id
    all_segments <- filter(osm_lines, str_detect(unit_id, .sample_trail$unit_id[1]))
    # filtered osm segments identified for unit_id
    idd_segments <- filter(.sysbikeped, str_detect(unit_id, .sample_trail$unit_id[1]))

    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(
        data = filter(st_transform(trail_polygons, 4326), str_detect(unit_id, .sample_trail$unit_id[1])),
        weight = 10, color = "orange", fill = "orange"
      ) %>%
      addPolylines(
        data = idd_segments,
        color = "black", weight = 6,
        label = ~ all_segments$osm_id,
        popup = paste0(all_segments$osm_id, "<br>highway: ", all_segments$highway)
      ) %>%
      addPolylines(
        data = all_segments,
        label = ~ all_segments$osm_id,
        popup = paste0(all_segments$osm_id, "<br>highway: ", all_segments$highway)
      )
  }
}

fxn_map_edits <- function(.sample_trail, .added, .removed) {
  #' View a map of a given trail with manual additions
  #'
  #' @description Create a mapview map of a given trail and manually added OSM segments
  #'
  #' @param .sample_trail: sf object of osm lines associated with a given trail
  #' @param .added: data.frame containing manually added OSM segments for a given trail system (DNR, Greater MN, Metro Regional)
  #' @param .removed: data.frame containing manually removed OSM segments for a given trail system (DNR, Greater MN, Metro Regional)
  if (map_issues == TRUE) {
    added_to_trail <- .added %>%
      filter(unit_id == .sample_trail$unit_id[1])

    added_sf <- osm_lines %>%
      filter(osm_id %in% added_to_trail$osm_id) %>%
      select(osm_id, geometry)

    removed_from_trail <- .removed %>%
      filter(unit_id == .sample_trail$unit_id[1])

    sample_removed <- .sample_trail %>%
      filter(!osm_id %in% removed_from_trail$osm_id)

    trail_polygon <- filter(trail_polygons, unit_id == .sample_trail$unit_id[1]) %>%
      # buffer for more clarity in map
      st_buffer(50)

    if (nrow(added_sf) > 0 & nrow(removed_from_trail) > 0) {
      mapview(trail_polygon,
        col.regions = "green",
        map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery")
      ) +
        mapview(sample_removed) +
        mapview(added_sf, color = "orange")
    } else if (nrow(added_sf) > 0 & nrow(removed_from_trail) == 0) {
      mapview(trail_polygon,
        col.regions = "green",
        map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery")
      ) +
        mapview(.sample_trail) +
        mapview(added_sf, color = "orange")
    } else if (nrow(added_sf) == 0 & nrow(removed_from_trail) > 0) {
      mapview(trail_polygon,
        col.regions = "green",
        map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery")
      ) +
        mapview(sample_removed)
    } else {
      mapview(trail_polygon,
        col.regions = "green",
        map.types = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery")
      ) +
        mapview(.sample_trail)
    }
  }
}
