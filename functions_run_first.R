#define data cleaning function
clean_data <- function(df, progress_threshold = 100, AC1_value = 7, AC2_value = 1) {
  filtered_df <- df %>%
    filter(Progress >= progress_threshold) %>% #Exclude if Progress is below threshold
    filter(AC1 == AC1_value) %>% #Exclude if attention check 1 was missed
    filter(AC2 == AC2_value) %>% #Exclude if attention check 2 was missed
    filter(Duration__in_seconds > (mean(Duration__in_seconds, na.rm = TRUE) - 3 * sd(Duration__in_seconds, na.rm = TRUE))) %>%#Exclude if time is 3SD below mean
    filter(Lan < 3) #language < very good
  return(filtered_df)
}
