# childlanguageinfo

A set of analyses for classifying communicative acts in children's conversations and characterizing their dynamics over development.

Analysis pipeline:
1. Cleaning of the corpus, preparation for HTMM, and cleaning of HTMM output files happens in analyses/htmm_pipeline.Rmd  
Code for the HTMM itself, by Gruber, Weiss, and Rosen-Zvi (2007), can be found at https://code.google.com/archive/p/openhtmm/ 
Code for examining and deciding on the desired number of topics is in analyses/n_topics.Rmd 
2. Entropy analyses occur in analyses/entropy_analysis_code.Rmd 
3. Mutual information analyses occur in analyses/mutual_information.R 
4. From there, all final analyses can be run in the files for generating the papers: writing/cogsci2021.Rmd and writing/topiCS.Rmd
