#############-----------------------   LOAD DATA  ----------------------###############


spine_png <- image_read(path = "spine_posterior.png", density = 5)

posterior_spine_ggdraw <- ggdraw() +
  draw_image(
    spine_png,
    scale = 1,
    y = 0,
    valign = 0,
    x = 0,
    height = 1, clip = "on"
  ) 

anterior_spine_jpg <- image_read(path = "spine_anterior.jpg")

anterior_spine_ggdraw <- ggdraw() +
  draw_image(
    anterior_spine_jpg,
    scale = 1,
    y = 0,
    valign = 0,
    x = 0,
    height = 1, clip = "on"
    # width = 1
  ) 

# implant_starts_df <- fread(file = "full_coordinates_no_empty.csv", header = TRUE, check.names = TRUE) 

all_object_ids_df <- fread(file = "all_object_ids_df.csv") 

# imported_coordinates <- fread("imported_coordinates_rounded.csv")
# 
# all_implants_constructed_df <<- all_object_ids_df %>%
#   filter(category == "implant") %>%
#   left_join(fread("imported_coordinates_rounded.csv") %>%
#               group_by(object_id) %>%
#               nest() %>%
#               mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#               select(object_id, object_constructed))

all_implants_constructed_df <<- all_object_ids_df %>%
  filter(category == "implant") %>%
  left_join(fread("coordinates/implant.csv") %>%
              group_by(object_id) %>%
              nest() %>%
              mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
              select(object_id, object_constructed))

# coordinate_file_names_list <- list(anterior_body = "coordinates/anterior_body.csv",
#                                    anterior_disc = "coordinates/anterior_disc.csv",
#                                    anterior_interbody_fusion = "coordinates/anterior_interbody_fusion.csv",
#                                    decompression = "coordinates/decompression.csv",
#                                    implant = "coordinates/implant.csv",
#                                    incision_drainage = "coordinates/incision_drainage.csv",
#                                    interbody = "coordinates/interbody.csv",
#                                    osteotomy = "coordinates/osteotomy.csv",
#                                    tumor = "coordinates/tumor.csv")
# 
# load_and_bind_coordinates_function <- function(all_implants_df, coordinate_file_name = "x"){
#   coordinate_objects_constructed_df <- all_object_ids_df %>%
#     filter(category == str_remove_all(coordinate_file_name, "coordinates/|.csv")) %>%
#     left_join(fread(paste0(coordinate_file_name)) %>%
#                 group_by(object_id) %>%
#                 nest() %>%
#                 mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#                 select(object_id, object_constructed))
# 
#   all_implants_df %>%
#     bind_rows(coordinate_objects_constructed_df) %>%
#     distinct()
# 
# }
# 
# for (i in coordinate_file_names_list) {
# 
#   all_implants_constructed_df <- load_and_bind_coordinates_function(all_implants_df = all_implants_constructed_df,
#                                                                     coordinate_file_name = i)
# 
# }
#############-----------------------   Build Key Dataframes  ----------------------###############

left_label_x <- 0.32
right_label_x <- 1 - left_label_x

labels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-") == FALSE) %>%
  mutate(y = c(0.925, 0.88, 0.865, 0.847, 0.828, 0.811, 0.793, 0.779, 0.757, 0.733, 0.711, 0.682, 0.653, 0.623, 0.593, 0.561, 0.53, 0.491, 0.457, 0.421, 0.385, 0.353, 0.311, 0.271, 0.238, 0.21, 0.19, 0.17))

interbody_levels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-"))

labels_anterior_df <- all_object_ids_df %>%
  filter(object == "corpectomy") %>%
  select(level, x, y) %>%
  distinct()  %>%
  mutate(y = case_when(
    level == "C3" ~ y + 0.005,
    level == "C4" ~ y + 0.01,
    level == "C5" ~ y + 0.01,
    level == "C6" ~ y + 0.01,
    level == "C7" ~ y + 0.01,
    level == "T1" ~ y + 0.01,
    level == "T2" ~ y + 0.01,
    level == "T3" ~ y + 0.01,
    level == "T4" ~ y + 0.01,
    level == "T5" ~ y + 0.01,
    TRUE ~ y
  ))

implant_levels_numbered_df <- all_object_ids_df %>%
  filter(implant == "yes") %>%
  select(level, vertebral_number) %>%
  distinct() %>%
  arrange(vertebral_number)

all_screw_coordinates_df <- fread("all_screw_coordinates_df.csv")

all_cages_df <- all_object_ids_df %>%
  filter(
    object == "anterior_interbody_implant" |
      object == "tlif" |
      object == "plif" |
      object == "llif" |
      object == "anterior_disc_arthroplasty" |
      object == "corpectomy_cage") %>%
  select(level, side, vertebral_number, object, approach) %>%
  # union_all(intervertebral_cage_df) %>%
  distinct() %>%
  arrange(vertebral_number) %>%
  mutate(cage_id = paste(str_to_lower(str_replace_all(level, "-", "_")), side, object, sep = "_")) 

#############-----------------------   Build Revision Implants  ----------------------###############

revision_screws_df <- all_object_ids_df %>%
  filter(category == "implant") %>%
  filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw") %>%
  filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
  filter(approach == "posterior") %>%
  arrange(vertebral_number)%>%
  left_join(all_implants_constructed_df)


#############-----------------------   Build: arthroplasty  ----------------------###############

arthroplasty_coordinates_df <- all_object_ids_df %>%
  filter(str_detect(object, "arthropl")) %>%
  mutate(inferior_endplate_y = c(0.965, 0.925, 0.897, 0.87, 0.846, 0.821, 0.795, 0.77, 0.74, 0.712, 0.682, 0.65, 0.62, 0.591, 0.56, 0.53, 0.495, 0.457, 0.415, 0.367, 0.317, 0.263, 0.215, 0.168), 
         superior_endplate_y = c(0.955, 0.918, 0.888, 0.865, 0.838, 0.815, 0.79, 0.76, 0.732, 0.705, 0.675, 0.645, 0.613, 0.584, 0.555, 0.525, 0.49, 0.45, 0.406, 0.357, 0.305, 0.255, 0.205, 0.16), 
         width = c(0.0175, 0.021, 0.02275, 0.02275, 0.0245, 0.0245, 0.0245, 0.02625, 0.02625, 0.028, 0.028, 0.02975, 0.02975, 0.0315, 0.0315, 0.0315, 0.03325, 0.035, 0.035, 0.035, 0.03675, 0.0385, 0.04025, 0.042)
  )


arthroplasty_function <- function(y_for_inferior_endplate, y_for_superior_endplate, endplate_width){
  endplate_height <- y_for_inferior_endplate - y_for_superior_endplate
  
  bottom_oval <- st_ellipse(st_point(c(0.5, y_for_superior_endplate)), ex = endplate_width/2, ey = endplate_height*0.75)
  
  left_bottom_point <- c(0.5 - endplate_width/2, y_for_superior_endplate)
  right_bottom_point <- c(0.5 + endplate_width/2, y_for_superior_endplate)
  # bottom_top_point <- c(0.5, y_for_superior_endplate + endplate_height/2)
  
  top_left_point <- c(0.5 - endplate_width/2, y_for_inferior_endplate)
  top_right_point <- c(0.5 + endplate_width/2, y_for_inferior_endplate)
  
  full_disc_sf <- st_buffer(st_polygon(list(rbind(left_bottom_point, right_bottom_point, top_right_point, top_left_point, left_bottom_point))), dist = endplate_height*0.1)
  
  top_disc_buff <- st_buffer(st_difference(x = full_disc_sf, y = bottom_oval), dist = -0.0004)
  
  bottom_disc_buff <- st_buffer(st_intersection(x = bottom_oval, y = full_disc_sf), dist = -0.0004)
  
  disc_df <- tibble(object_constructed = c(st_geometry(top_disc_buff), st_geometry(bottom_disc_buff[[1]]))) %>%
    mutate(color = c("blue", "lightblue"))
  
  return(disc_df)
}
# 
# l6_levels_vector <- c('Occiput', 'O-C1', 'C1', 'C1-C2', 'C2', 'C2-C3', 'C3', 'C3-C4', 'C4', 'C4-C5', 'C5', 'C5-C6', 'C6', 'C6-C7', 'C7', 'C7-T1', 'T1', 'T1-T2', 'T2', 'T2-T3', 'T3', 'T3-T4', 'T4', 'T4-T5', 'T5', 'T5-T6', 'T6', 'T6-T7', 'T7', 'T7-T8', 'T8', 'T8-T9', 'T9', 'T9-T10', 'T10', 'T10-T11', 'T11', 'T11-T12', 'T12', 'T12-L1', 'L1', 'L1-L2', 'L2', 'L2-L3', 'L3', 'L3-L4', 'L4', 'L4-L5', 'L5', "L5-L6", "L6", 'L6-S1', 'S1', 'Sacro-iliac', 'Iliac', 'S2AI')
# 
# l6_vertebral_numbers_vector <- c(0,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 24, 24.5, 25, 25.5, 26, 26.5, 26, 27)
# 
# l6_levels_numbered_df <- tibble(level = l6_levels_vector, vertebral_number = l6_vertebral_numbers_vector)
# 
# l6_jh_get_vertebral_number_function <- function(level_to_get_number){
#   vert_number <-
#     case_when(
#       level_to_get_number == 'Occiput' ~ 0,
#       level_to_get_number == 'O-C1' ~ 0.5,
#       level_to_get_number == 'C1' ~ 1,
#       level_to_get_number == 'C1-C2' ~ 1.5,
#       level_to_get_number == 'C2' ~ 2,
#       level_to_get_number == 'C2-C3' ~ 2.5,
#       level_to_get_number == 'C3' ~ 3,
#       level_to_get_number == 'C3-C4' ~ 3.5,
#       level_to_get_number == 'C4' ~ 4,
#       level_to_get_number == 'C4-C5' ~ 4.5,
#       level_to_get_number == 'C5' ~ 5,
#       level_to_get_number == 'C5-C6' ~ 5.5,
#       level_to_get_number == 'C6' ~ 6,
#       level_to_get_number == 'C6-C7' ~ 6.5,
#       level_to_get_number == 'C7' ~ 7,
#       level_to_get_number == 'C7-T1' ~ 7.5,
#       level_to_get_number == 'T1' ~ 8,
#       level_to_get_number == 'T1-T2' ~ 8.5,
#       level_to_get_number == 'T2' ~ 9,
#       level_to_get_number == 'T2-T3' ~ 9.5,
#       level_to_get_number == 'T3' ~ 10,
#       level_to_get_number == 'T3-T4' ~ 10.5,
#       level_to_get_number == 'T4' ~ 11,
#       level_to_get_number == 'T4-T5' ~ 11.5,
#       level_to_get_number == 'T5' ~ 12,
#       level_to_get_number == 'T5-T6' ~ 12.5,
#       level_to_get_number == 'T6' ~ 13,
#       level_to_get_number == 'T6-T7' ~ 13.5,
#       level_to_get_number == 'T7' ~ 14,
#       level_to_get_number == 'T7-T8' ~ 14.5,
#       level_to_get_number == 'T8' ~ 15,
#       level_to_get_number == 'T8-T9' ~ 15.5,
#       level_to_get_number == 'T9' ~ 16,
#       level_to_get_number == 'T9-T10' ~ 16.5,
#       level_to_get_number == 'T10' ~ 17,
#       level_to_get_number == 'T10-T11' ~ 17.5,
#       level_to_get_number == 'T11' ~ 18,
#       level_to_get_number == 'T11-T12' ~ 18.5,
#       level_to_get_number == 'T12' ~ 19,
#       level_to_get_number == 'T12-L1' ~ 19.5,
#       level_to_get_number == 'L1' ~ 20,
#       level_to_get_number == 'L1-L2' ~ 20.5,
#       level_to_get_number == 'L2' ~ 21,
#       level_to_get_number == 'L2-L3' ~ 21.5,
#       level_to_get_number == 'L3' ~ 22,
#       level_to_get_number == 'L3-L4' ~ 22.5,
#       level_to_get_number == 'L4' ~ 23,
#       level_to_get_number == 'L4-L5' ~ 23.5,
#       level_to_get_number == 'L5' ~ 24,
#       level_to_get_number == 'L5-L6' ~ 24.5,
#       level_to_get_number == 'L6' ~ 25,
#       level_to_get_number == 'L6-S1' ~ 25.5,
#       level_to_get_number == 'S1' ~ 26,
#       level_to_get_number == "Sacro-iliac" ~ 26.5,
#       level_to_get_number == 'Iliac' ~ 27,
#       level_to_get_number == 'S2AI' ~ 28
#     )
#   return(vert_number)
# }
# 
# l6_jh_get_vertebral_level_function <- function(number) {
#   level = case_when(
#     number == 0 ~ 'Occiput',
#     number == 0.5 ~ 'O-C1',
#     number == 1 ~ 'C1',
#     number == 1.5 ~ 'C1-C2',
#     number == 2 ~ 'C2',
#     number == 2.5 ~ 'C2-C3',
#     number == 3 ~ 'C3',
#     number == 3.5 ~ 'C3-C4',
#     number == 4 ~ 'C4',
#     number == 4.5 ~ 'C4-C5',
#     number == 5 ~ 'C5',
#     number == 5.5 ~ 'C5-C6',
#     number == 6 ~ 'C6',
#     number == 6.5 ~ 'C6-C7',
#     number == 7 ~ 'C7',
#     number == 7.5 ~ 'C7-T1',
#     number == 8 ~ 'T1',
#     number == 8.5 ~ 'T1-T2',
#     number == 9 ~ 'T2',
#     number == 9.5 ~ 'T2-T3',
#     number == 10 ~ 'T3',
#     number == 10.5 ~ 'T3-T4',
#     number == 11 ~ 'T4',
#     number == 11.5 ~ 'T4-T5',
#     number == 12 ~ 'T5',
#     number == 12.5 ~ 'T5-T6',
#     number == 13 ~ 'T6',
#     number == 13.5 ~ 'T6-T7',
#     number == 14 ~ 'T7',
#     number == 14.5 ~ 'T7-T8',
#     number == 15 ~ 'T8',
#     number == 15.5 ~ 'T8-T9',
#     number == 16 ~ 'T9',
#     number == 16.5 ~ 'T9-T10',
#     number == 17 ~ 'T10',
#     number == 17.5 ~ 'T10-T11',
#     number == 18 ~ 'T11',
#     number == 18.5 ~ 'T11-T12',
#     number == 19 ~ 'T12',
#     number == 19.5 ~ 'T12-L1',
#     number == 20 ~ 'L1',
#     number == 20.5 ~ 'L1-L2',
#     number == 21 ~ 'L2',
#     number == 21.5 ~ 'L2-L3',
#     number == 22 ~ 'L3',
#     number == 22.5 ~ 'L3-L4',
#     number == 23 ~ 'L4',
#     number == 23.5 ~ 'L4-L5',
#     number == 24 ~ 'L5',
#     number == 24.5 ~ 'L5-L6',
#     number == 25 ~ 'L6',
#     number == 25.5 ~ 'L6-S1',
#     number == 26 ~ 'S1',
#     number == 26.5 ~ "Sacro-iliac",
#     number == 27 ~ 'Iliac',
#     number == 28 ~ 'S2AI'
#   )
#   return(level)
# }
# 
# 
# left_label_x <- 0.32
# right_label_x <- 1 - left_label_x
# 
# l6_labels_df <- l6_levels_numbered_df %>%
#   filter(str_detect(level, pattern = "-") == FALSE) %>%
#   mutate(y = c(0.925,
#                0.88,
#                0.865,
#                0.847,
#                0.828,
#                0.811,
#                0.793,
#                0.779,
#                0.757, 
#                0.733,
#                0.711, 
#                0.682, 
#                0.653,
#                0.623,
#                0.593,
#                0.561,
#                0.53, 
#                0.491,
#                0.457,
#                0.421,
#                0.385, 
#                0.353, 
#                0.311,
#                0.271,
#                0.238,
#                0.20, 
#                .175, 
#                0.15, 
#                .13))
# 
# l6_interbody_levels_df <- l6_levels_numbered_df %>%
#   filter(str_detect(level, pattern = "-"))
# 
# 
# #############-----------------------   POSTERIOR  ----------------------###############
# #############-----------------------   POSTERIOR  ----------------------###############
# #############-----------------------   POSTERIOR  ----------------------###############
# #############-----------------------   POSTERIOR  ----------------------###############
# #############-----------------------   POSTERIOR  ----------------------###############
# 
# 
# #############-----------------------   LOAD DATA  ----------------------###############
# 
# l6_spine_png <- image_read(path = "spine_posterior_6_lumbar_vert.png")
# 
# posterior_spine_plot_l6 <- ggdraw() +
#   draw_image(
#     l6_spine_png,
#     scale = 1,
#     y = 0,
#     valign = 0,
#     x = 0,
#     height = 1
#     # width = 1
#   ) 
# 
# l6_anterior_spine_png <- image_read(path = "spine_anterior_6_lumbar_vert.png")
# 
# 
# jh_change_object_df_to_l6_function <- function(all_objects_input_df = tibble(level = character())){
#   l5_to_l6_shifted_down_df <- all_object_ids_df %>%
#     select(-x, -y) %>%
#     left_join(imported_coordinates) %>%
#     filter(vertebral_number > 23.9) %>%
#     mutate(level = str_replace_all(string = level, pattern = "L5", replacement = "L6")) %>%
#     mutate(vertebral_number = vertebral_number + 1) %>%
#     mutate(y = y - 0.04) %>%
#     mutate(level = str_replace_all(string = object_id, pattern = "L5", replacement = "L6"))
# 
# 
# 
#   l5_l6_matrices_df <- all_object_ids_df %>%
#     select(-x, -y) %>%
#     left_join(imported_coordinates) %>%
#     filter(vertebral_number > 23.9) %>%
#     filter(vertebral_number < 24.7) %>%
#     mutate(level = str_replace_all(string = level, pattern = "S1", replacement = "L6")) %>%
#     mutate(level = str_replace_all(string = object_id, pattern = "S1", replacement = "L6")) %>%
#     union_all(l5_to_l6_shifted_down_df)
# 
#   l6_implants_constructed_df <- l5_l6_matrices_df %>%
#     filter(str_detect(object_id, "arthroplasty") == FALSE) %>%
#     select(object_id, x, y) %>%
#     group_by(object_id) %>%
#     # filter(is.na(y)) %>%
#     nest() %>%
#     mutate(object_constructed = map(.x = data, .f = ~ st_linestring(as.matrix(.x)))) %>%
#     select(object_id, object_constructed)
# 
#   l5_to_l6_shifted_down_df_for_linking <- all_object_ids_df %>%
#     filter(vertebral_number > 23.9) %>%
#     mutate(level = str_replace_all(string = level, pattern = "L5", replacement = "L6")) %>%
#     mutate(vertebral_number = vertebral_number + 1) %>%
#     mutate(y = y - 0.04) %>%
#     mutate(level = str_replace_all(string = object_id, pattern = "L5", replacement = "L6"))
# 
# 
#   l5_l6_for_linking <- all_object_ids_df %>%
#     # select(-x, -y) %>%
#     # left_join(imported_coordinates) %>%
#     filter(vertebral_number > 23.9) %>%
#     filter(vertebral_number < 24.7) %>%
#     mutate(level = str_replace_all(string = level, pattern = "S1", replacement = "L6")) %>%
#     mutate(level = str_replace_all(string = object_id, pattern = "S1", replacement = "L6")) %>%
#     union_all(l5_to_l6_shifted_down_df_for_linking)
# 
# 
#   l6_implants_constructed_df <- l5_l6_for_linking %>%
#     distinct() %>%
#     filter(str_detect(object_id, "arthroplasty") == FALSE) %>%
#     left_join(l6_implants_constructed_df)
# 
#   l6_all_implants_constructed_df <- all_objects_input_df %>%
#     filter(vertebral_number < 23.9) %>%
#     bind_rows(l6_implants_constructed_df)
# 
#   l6_revision_implants_df <- l6_all_implants_constructed_df %>%
#     filter(object == "pedicle_screw" | object == "pelvic_screw" | object == "occipital_screw") %>%
#     filter(approach == "posterior") %>%
#     arrange(vertebral_number) %>%
#     distinct() %>%
#     group_by(level, object, side) %>%
#     filter(y == max(y)) %>%
#     ungroup()%>%
#     remove_empty(which = c("rows", "cols"))
# 
#   return(list(l6_implants_constructed_df = l6_all_implants_constructed_df,
#               l6_revision_implants_df = l6_revision_implants_df))
# 
# }

