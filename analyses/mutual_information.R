library(here)
library(tidyverse)
library(feather)
library(entropy)

childes_htmm_pdwz <- read_feather(here("data/childes_htmm_15.feather")) 
childes_keep <- childes_htmm_pdwz %>%
  mutate(speaker_role = if_else(speaker_role == "Mother" | speaker_role == "Father",
                                "Parent", speaker_role)) %>%
  group_by(transcript_id, speaker_role) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(speaker_role, transcript_id, fill = list(n = 0)) %>%
  arrange(transcript_id) %>%
  pivot_wider(names_from = "speaker_role", values_from = "n") %>%
  filter(Parent > 10, Target_Child > 10)

childes_htmm_pdwz <- childes_htmm_pdwz %>%
  mutate(speaker_role = if_else(speaker_role == "Mother" | speaker_role == "Father",
                                "Parent", speaker_role)) %>%
  filter(transcript_id %in% childes_keep$transcript_id) %>%
  mutate(age_in_months = floor(target_child_age)) %>%
  filter(age_in_months >= 6) %>%
  mutate(age_bin = cut(age_in_months, breaks = 9))

num_topics <- n_distinct(childes_htmm_pdwz$max_topic)
topic_nums <- c(1:num_topics)
topic_probs <- paste0("topic_", topic_nums)


avg_chunks <- childes_htmm_pdwz %>%
  group_by(transcript_id) %>%
  mutate(
    lag_speaker = lag(speaker_role),
    temp = ifelse(speaker_role != lag_speaker, 1, 0),
    temp_id = cumsum(c(0, temp[!is.na(temp)])),
    chunk = temp_id
  ) %>% ungroup() %>%
  group_by(speaker_role, transcript_id, target_child_age, chunk) %>%
  summarise_at(topic_probs, mean) %>%
  ungroup() %>%
  mutate(age_bin = floor(target_child_age/6) *6) %>%
  arrange(transcript_id, chunk)


matrix_from_rows <- function(row_1, row_2) {
  matrix <- t(as.matrix(row_1)) %*% as.matrix(row_2)
  return(matrix)
}

get_prob_t_matrix <- function(df) {
  child_rows <- which(df$speaker_role == "Target_Child")
  child_rows <- child_rows[child_rows > 1]
  parent_rows <- which(df$speaker_role == "Parent")
  parent_rows <- parent_rows[parent_rows > 1]
  df <- df %>%
    select(all_of(topic_probs))
  
  parent_matrix <- map(parent_rows, 
                       ~matrix_from_rows(df %>% slice(.x - 1), df %>% slice(.x))) %>% 
    Reduce('+', .) %>% as.tibble() %>%
    mutate(speaker = "parent") %>%
    cbind(topic_probs) 
  
  child_matrix <- map(child_rows, 
                      ~matrix_from_rows(df %>% slice(.x - 1), df %>% slice(.x))) %>%
    Reduce('+', .) %>% as.tibble() %>%
    mutate(speaker = "child") %>%
    cbind(topic_probs) 
  
  child_rows <- child_rows[child_rows > 2]
  
  child_self <- map(child_rows, 
                    ~matrix_from_rows(df %>% slice(.x - 2), df %>% slice(.x))) %>%
    Reduce('+', .) %>% as.tibble() %>%
    mutate(speaker = "child_self") %>%
    cbind(topic_probs) 
  
  return(bind_rows(parent_matrix, child_matrix, child_self) %>% 
           rename(prior_topic = topic_probs))
}


chunks_nested <- avg_chunks %>%
  group_by(age_bin, transcript_id) %>%
  filter(n() > 2) %>%
  nest() %>% 
  mutate(matrix = map(data, get_prob_t_matrix))

epsilon = 0.000000000001

joint_probs <- chunks_nested %>%
  select(transcript_id, age_bin, matrix) %>%
  unnest(matrix) %>%
  mutate_at(topic_probs, ~ . + epsilon)

#code for checking various joint prob tables
# joint_probs <- chunks_nested %>%
#   select(transcript_id, age_bin, matrix) %>%
#   unnest(matrix) %>%
#   pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob") %>%
#   mutate(prob = if_else(prior_topic == current_topic, 100, 0.01)) %>%
#   pivot_wider(names_from = current_topic, values_from = prob)

get_mis_sequential <- function(transcripts) {
  
  joint_probs_by_age <- joint_probs %>%
    filter(transcript_id %in% transcripts) %>%
    group_by(age_bin, speaker, prior_topic) %>%
    summarise_at(topic_probs, sum) %>%
    ungroup()
  
  table_sums <- joint_probs_by_age %>% 
    mutate(row_sum = rowSums(select(., all_of(topic_probs)))) %>% 
    mutate_at(all_of(topic_probs), ~ ./row_sum) %>%
    group_by(age_bin, speaker) %>%
    summarise(sum = sum(row_sum))
  
  joint_probs_normed <- joint_probs_by_age %>% 
    left_join(table_sums, by = c("age_bin", "speaker")) %>%
    mutate_at(all_of(topic_probs), ~ ./sum) %>%
    select(-sum) %>%
    pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob")
  
  prior_topic_marginal <- joint_probs_normed %>%
    group_by(age_bin, speaker, prior_topic) %>%
    summarise(prior_topic_prob = sum(prob)) %>% ungroup()
  
  current_topic_marginal <- joint_probs_normed %>%
    group_by(age_bin, speaker, current_topic) %>%
    summarise(current_topic_prob = sum(prob)) %>% ungroup()
  
  all_probs <- joint_probs_normed %>%
    rename(joint_prob = prob) %>%
    left_join(prior_topic_marginal, 
              by = c("age_bin", "speaker", "prior_topic")) %>%
    left_join(current_topic_marginal, 
              by = c("age_bin", "speaker", "current_topic"))
    
  parent_given_child <- all_probs %>%
    filter(speaker == "parent") %>%
    mutate(term = joint_prob * log2(joint_prob/(prior_topic_prob * current_topic_prob))) %>%
    group_by(age_bin) %>%
    summarise(mi = sum(term)) %>%
    mutate(type = "parent_given_child")
  
  child_given_parent <- all_probs %>%
    filter(speaker == "child") %>%
    mutate(term = joint_prob * log2(joint_prob/(prior_topic_prob * current_topic_prob))) %>%
    group_by(age_bin) %>%
    summarise(mi = sum(term)) %>%
    mutate(type = "child_given_parent")
  
  child_given_child <- all_probs %>%
    filter(speaker == "child_self") %>%
    mutate(term = joint_prob * log2(joint_prob/(prior_topic_prob * current_topic_prob))) %>%
    group_by(age_bin) %>%
    summarise(mi = sum(term)) %>%
    mutate(type = "child_given_child")
  
  all_mis <- rbind(child_given_parent, parent_given_child, child_given_child)
  return(all_mis)
}



all_transcripts <- chunks_nested %>% ungroup() %>%
  select(transcript_id) %>%
  distinct(transcript_id)

n_transcripts <- all_transcripts %>% nrow()

transcript_lists <- replicate(500, c(sample_n(all_transcripts, n_transcripts, replace = TRUE)))

mis <- map(transcript_lists, get_mis_sequential)

mis_df <- do.call(rbind, mis)

mi_cis <- mis_df %>%
  group_by(age_bin, type) %>%
  summarise(bootstrap_mean = mean(mi), 
            ci_upper = quantile(mi, 0.975),
            ci_lower = quantile(mi, 0.025)) 

mi_means <- get_mis_sequential(all_transcripts$transcript_id)

mis_all <- mi_cis %>%
  left_join(mi_means, by = c("age_bin", "type"))


#write_csv(mis_all, here("data/mutual_information_vals.csv"))

joint_probs_by_age <- joint_probs %>%
  group_by(age_bin, speaker, prior_topic) %>%
  summarise_at(topic_probs, sum) %>%
  ungroup()

table_sums <- joint_probs_by_age %>% 
  mutate(row_sum = rowSums(select(., all_of(topic_probs)))) %>% 
  mutate_at(all_of(topic_probs), ~ ./row_sum) %>%
  group_by(age_bin, speaker) %>%
  summarise(sum = sum(row_sum))

joint_probs_normed <- joint_probs_by_age %>% 
  left_join(table_sums, by = c("age_bin", "speaker")) %>%
  mutate_at(all_of(topic_probs), ~ ./sum) %>%
  select(-sum) %>%
  pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob")

joint_probs_normed %>%
  filter(speaker == "child") %>%
  mutate(prior_topic = as.integer(str_remove(prior_topic, "topic_")),
         current_topic = as.integer(str_remove(current_topic, "topic_")),
         age_median = case_when(age_bin <= 10 ~ "6 to 10 months",
                                age_bin <= 20 ~ "11 to 20 months",
                                age_bin > 20 ~ "21 to 60 months"),
         age_median = factor(age_median, levels = c("6 to 10 months",
                                                    "11 to 20 months",
                                                    "21 to 60 months"))) %>%
  ggplot(aes(x=prior_topic, y=current_topic, fill=log(prob))) + 
  geom_tile() +
  facet_wrap(~age_median, scales = "free") +
  scale_fill_gradientn(colours = viridis(256, option = "D"))

#write_csv(joint_probs_normed, here("data/joint_probs_normed.csv"))