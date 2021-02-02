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

conditional_probs <- chunks_nested %>%
  select(transcript_id, age_bin, matrix) %>%
  unnest(matrix) 

get_mis <- function(transcripts) {
  
  conditional_probs_by_age <- conditional_probs %>%
    filter(transcript_id %in% transcripts) %>%
    group_by(age_bin, speaker, prior_topic) %>%
    summarise_at(topic_probs, sum) %>%
    ungroup()
  
  conditional_probs_normed <- conditional_probs_by_age %>% 
    mutate(row_sum = rowSums(select(., all_of(topic_probs)))) %>% 
    mutate_at(all_of(topic_probs), ~ ./row_sum) 

  
  parent_probs <- avg_chunks %>%
    filter(speaker_role == "Parent",
           transcript_id %in% transcripts) %>%
    group_by(age_bin) %>%
    summarise_at(topic_probs, sum) %>%
    ungroup() %>%
    mutate(row_sum = rowSums(select(., all_of(topic_probs)))) %>% 
    mutate_at(all_of(topic_probs), ~ ./row_sum) %>%
    pivot_longer(cols = topic_probs, names_to = "state", values_to = "prob") %>%
    group_by(age_bin) %>%
    mutate(entropy = entropy(prob)) %>%
    ungroup()
  
  child_probs <- avg_chunks %>%
    filter(speaker_role == "Target_Child",
           transcript_id %in% transcripts) %>%
    group_by(age_bin) %>%
    summarise_at(topic_probs, sum) %>%
    ungroup() %>%
    mutate(row_sum = rowSums(select(., all_of(topic_probs)))) %>% 
    mutate_at(all_of(topic_probs), ~ ./row_sum)  %>%
    pivot_longer(cols = topic_probs, names_to = "state", values_to = "prob") %>%
    group_by(age_bin) %>%
    mutate(entropy = entropy(prob)) %>%
    ungroup()
  
  child_given_parent <- conditional_probs_normed %>%
    filter(speaker == "child") %>%
    pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob") %>%
    group_by(age_bin, prior_topic) %>% 
    mutate(entropy = entropy(prob)) %>%
    summarise(conditional_entropy = first(entropy)) %>% ungroup()
  
  parent_given_child <- conditional_probs_normed %>%
    filter(speaker == "parent") %>%
    pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob") %>%
    group_by(age_bin, prior_topic) %>% 
    mutate(entropy = entropy(prob)) %>%
    summarise(conditional_entropy = first(entropy)) %>% ungroup()
  
  child_given_child <- conditional_probs_normed %>%
    filter(speaker == "child_self") %>%
    pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob") %>%
    group_by(age_bin, prior_topic) %>% 
    mutate(entropy = entropy(prob)) %>%
    summarise(conditional_entropy = first(entropy)) %>% ungroup()
  
  sums_child_given_parent <- child_given_parent %>%
    left_join(parent_probs, by = c("prior_topic" = "state", "age_bin")) %>%
    mutate(weighted_entropy = conditional_entropy * prob) %>%
    group_by(age_bin) %>%
    summarise(sum_weighted_entropies = sum(weighted_entropy)) %>%
    ungroup()
  
  sums_parent_given_child <- parent_given_child %>%
    left_join(child_probs, by = c("prior_topic" = "state", "age_bin")) %>%
    mutate(weighted_entropy = conditional_entropy * prob) %>%
    group_by(age_bin) %>%
    summarise(sum_weighted_entropies = sum(weighted_entropy)) %>%
    ungroup()
  
  sums_child_given_child <- child_given_child %>%
    left_join(child_probs, by = c("prior_topic" = "state", "age_bin")) %>%
    mutate(weighted_entropy = conditional_entropy * prob) %>%
    group_by(age_bin) %>%
    summarise(sum_weighted_entropies = sum(weighted_entropy)) %>%
    ungroup()
  
  mi_child_given_parent <- child_probs %>%
    group_by(age_bin) %>%
    summarise(entropy = first(entropy)) %>% ungroup() %>%
    left_join(sums_child_given_parent, by = "age_bin") %>%
    mutate(mi = entropy - sum_weighted_entropies,
           type = "child_given_parent")
  
  mi_parent_given_child <- parent_probs %>%
    group_by(age_bin) %>%
    summarise(entropy = first(entropy)) %>% ungroup() %>%
    left_join(sums_parent_given_child, by = "age_bin") %>%
    mutate(mi = entropy - sum_weighted_entropies,
           type = "parent_given_child")
  
  mi_child_given_child <- child_probs %>%
    group_by(age_bin) %>%
    summarise(entropy = first(entropy)) %>% ungroup() %>%
    left_join(sums_child_given_child, by = "age_bin") %>%
    mutate(mi = entropy - sum_weighted_entropies,
           type = "child_given_child")
  
  all_mis <- rbind(mi_child_given_parent, mi_parent_given_child, mi_child_given_child)
  return(all_mis)
}

sample_mis <- get_mis(c(sample_n(all_transcripts, 20) %>% pull())) 


all_transcripts <- chunks_nested %>% ungroup() %>%
  select(transcript_id) %>%
  distinct(transcript_id)

conditional_probs_normed %>%
  pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob") %>%
  ggplot(aes(x=prior_topic, y=current_topic, fill=prob)) + 
  geom_tile() +
  facet_grid(speaker~age_bin, scales = "free") 


#write_csv(conditional_probs_normed, here("data/conditional_probs.csv"))
#write_csv(all_mis, here("data/mutual_information_vals.csv"))
