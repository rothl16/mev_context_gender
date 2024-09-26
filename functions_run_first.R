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

#define preparation function
preparation <- function(df){
  prepared_df <- df %>%
    #low effort
    rename(warmth_lo = Ch_LoEf_2, #rename for warmth
           competence_lo = Ch_LoEf_6) %>% #rename for competence
    mutate(core_good_lo = rowMeans(select(.,c( #compute core goodness
      Ch_LoEf_3, Ch_LoEf_9, Ch_LoEf_11, Ch_LoEf_10, Ch_LoEf_13, Ch_LoEf_14
    )))) %>%
    mutate(value_comm_lo = rowMeans(select(.,c( #compute value commitment
      Ch_LoEf_1, Ch_LoEf_5, Ch_LoEf_15, Ch_LoEf_8, Ch_LoEf_4, Ch_LoEf_12, Ch_LoEf_7
    )))) %>%
    #high effort
    rename(warmth_he = Ch_HiEf_2, #rename for warmth
           competence_he = Ch_HiEf_6) %>% #rename for competence
    mutate(core_good_he = rowMeans(select(.,c( #compute core goodness
      Ch_HiEf_3, Ch_HiEf_9, Ch_HiEf_11, Ch_HiEf_10, Ch_HiEf_13, Ch_HiEf_14
    )))) %>%
    mutate(value_comm_he = rowMeans(select(.,c( #compute value commitment
      Ch_HiEf_1, Ch_HiEf_5, Ch_HiEf_15, Ch_HiEf_8, Ch_HiEf_4, Ch_HiEf_12, Ch_HiEf_7
    )))) %>%
    #rename
    rename(effort_lo = Ef_LoEf, #effortfulness
           effort_he = Ef_HiEf,
           quality_lo = Qu_LoEf, #quality of product
           quality_he = Qu_HiEf,
           difficulty_lo = Dif_LoEf, #difficulty of work
           difficulty_he = Dif_HiEf,
           valuable_lo = Val_LoEf, #value of work
           valuable_he = Val_HiEf,
           pay_deservingness_lo = Sal_LoEf_1, #pay deservingness
           pay_deservingness_he = Sal_HiEf_1,
           cooperation_lo = Coop_LoEf, #cooperation partner satisfaction
           Cooperation_he = Coop_HiEf) 
  return(prepared_df)
}

#create function to compute difference scores and filter by criterion
difference_scores <- function(df){
  df_difference <- df %>%
    mutate(effort_check = (effort_he-effort_lo)) %>% #create variable to compare effort perceptions
    filter(effort_check > 0) %>% #filter out "wrong" perceptions
    mutate(core_good_diff = (core_good_he-core_good_lo)) %>% #effort moralization effect: core goodness
    mutate(value_comm_diff = (value_comm_he-value_comm_lo)) #effort moralization effect: value commitment
  
  return(df_difference)
}
