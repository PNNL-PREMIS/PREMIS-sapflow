## Function to calculate sapwood area
## Returns data frame of Js trees and estimated sapwood area
## Created January 8, 2021 | Stephanie Pennington

sapwood_area_calc <- function(js_trees, sapwood_depth) {
  # A linear model is used to predict our tree's sapwood area
  sd_model <- lm(sapwood_d ~ DBH * Species, data = sapwood_depth)
  
  js_trees$pred_sd <- predict.lm(object = sd_model, newdata = js_trees)
  
  # Test to make sure predictor works 
  ggplot(sapwood_depth, aes(x = DBH, y = sapwood_d, colour = Species)) + 
    geom_point() + 
    geom_smooth(method = lm, se =  FALSE) + 
    geom_point(data = js_trees, aes(x = DBH, y = pred_sd, 
                                    colour = Species), shape = 1, size = 5)
  
  js_trees %>% 
    # calculate area of the tree and heartwood, the difference will give us sapwood area
    mutate(t_area_cm2 = pi * (DBH / 2)^2, 
           hw_area_cm2 = pi * ((DBH - 2 * pred_sd) / 2) ^ 2,
           sw_area_cm2 = t_area_cm2 - hw_area_cm2) %>% 
    select(Tree, sw_area_cm2)

}