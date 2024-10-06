# Install the 'remotes' package if you don't have it
install.packages("remotes")

# Install 'litsearchr' from GitHub
remotes::install_github("elizagrames/litsearchr")
1
####################################################################
library(litsearchr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
packageVersion("litsearchr")

getwd()
naive_results<- import_results(file = "pubmed-mutationsA-set.nbib")
nrow(naive_results)
View(naive_results)
colnames(naive_results)

naive_results[1, "title"]
naive_results[1, "keywords"]
sum(is.na(naive_results[, "keywords"]))
extract_terms(keywords = naive_results[, "keywords"], method = "tagged")
?extract_terms

keywords<- extract_terms(keywords = naive_results[, "keywords"],
                         method = "tagged", min_n = 1)
keywords 

extract_terms(keywords = naive_results[, "keywords"],
              method = "tagged", min_n = 2)

extract_terms(text = naive_results[, "title"],
              method = "fakerake", min_n = 2, min_freq = 3)

extract_terms(text = naive_results[, "title"],
              method = "fakerake", min_n = 1, min_freq = 1)


###################Creating stopwords########################

# Create the content to write to the file
file_content_stopword <- c("therapy", "treated","status","putative","qatar","parasite",
                  "years","mutations eleven","mutations eleven years","molecular analyses", 
                  "molecular detection","mediators","molecular","infection",
                  "isolates","epidemiology","eleven years","eleven","detection",
                  "detected","clinical","clinical isolates","children","adoption",
                  "absence","5 years","2012-2019")

# Define the path where the file will be saved
file_path <- ("C:/Users/Brain Computers/Desktop/R-files/mutation.txt")

# Write the content to the file
writeLines(file_content_stopword, con = file_path)
file_content_stopword

all_stopwords<- c(get_stopwords("English"), file_content_stopword)


title_terms<- extract_terms(text = naive_results[, "title"],
              method = "fakerake", min_n = 1, min_freq = 3, stopwords = all_stopwords)
title_terms
################ combine the search results removing any duplicate######
terms <- unique(c(keywords, title_terms))
terms
##########Network analysis_ abstract and title####################
docs<- paste(naive_results[, "title"], naive_results[, "abstract"])
docs[1]

dfm<- create_dfm(elements = docs, features = terms)
dfm

g<- create_network(dfm, min_studies = 3)
g

################################################################
ggraph(g, layout = "stress") + 
  coord_fixed() + 
  expand_limits(x = c(-2, 2)) +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(shape = "circle filled", fill = "white") +
  
  # Adjust node text with repel and size
  geom_node_text(aes(label = name), 
                 hjust = "outward", 
                 check_overlap = FALSE, 
                 size = 3,           # Increase text size
                 repel = TRUE,       # Prevent label overlap
                 nudge_x = 0.1,      # Nudge text slightly in x direction
                 nudge_y = 0.1) +    # Nudge text slightly in y direction
  guides(edge_alpha = "none")

strengths<- strength(g)
strengths
library(dplyr)

data.frame(term=names(strengths), strength=strengths, row.names = NULL)%>%
  mutate(rank=rank(strength, ties.method = "min"))%>%
  arrange(strength)->term_strengths

term_strengths
library(ggrepel)

cutoff_fig <- ggplot(term_strengths, aes(x = rank, y = strength, label = term)) +
  geom_line() +
  geom_point() +
  # Use geom_text_repel to prevent text overlap and allow repelling
  geom_text_repel(data = filter(term_strengths, rank > 0), 
                  hjust = "right", 
                  check_overlap = FALSE, 
                  size = 3,     
                  nudge_x = 0.1, 
                  nudge_y = 0.1)

print(cutoff_fig)

cutoff_cum<- find_cutoff(g, method = "cumulative", percent = 1)
cutoff_cum
cutoff_fig+
  geom_hline(yintercept = cutoff_cum, linetype="dashed")

selected_terms1<-get_keywords(reduce_graph(g, cutoff_cum))
selected_terms1
#############Multiple cutoffs
cutoff_change<- find_cutoff(g, method="changepoint", knot_num = 3)
cutoff_change

cutoff_fig+
  geom_hline(yintercept = cutoff_change, linetype="dashed")

#We can pick one of the point i.e 1 will retain above the first line##
g_redux<- reduce_graph(g, cutoff_change[1])
selected_terms<- get_keywords(g_redux)
selected_terms
#####Add back terms from the Original naive search if they####
#####happen not to show up in the final search/filters#####
#####Here i will be using search results from selected_terms1#####
extract_terms<-c("mutations", "Pfk13")
selected_terms1

selected_terms<- c(selected_terms1, extract_terms)
selected_terms
######Group base on sub_topics: Nigeria, Pfk13, mutations
grouped_terms <- list(
  mutations=selected_terms[c(15, 9)], 
  Pfk13=selected_terms[c(16,3,5,6,8,13)],
  nigeria=selected_terms[4]
)
grouped_terms

write_search(
  grouped_terms,
  languages = "English",
  exactphrase = T,
  stemming = F,
  closure = "left",
  writesearch = T
)
1
cat(read_file("search-inEnglish.txt"))

##############Checking the new search#####################
new_results<- import_results(file = "pubmed-mutationsO-set.nbib")
nrow(new_results)
View(new_results)
##########Against naive results#######################
naive_results %>%
  mutate(in_new_results=title %in% new_results[, "title"]) ->
  naive_results

naive_results %>%
  filter(!in_new_results) %>%
  select(title, keywords)

important_titles <- c("Prevalence of potential mediators of artemisinin resistance in African isolates of Plasmodium falciparum.",
                      "P fmdr 1 and kelch 13 genes distribution among children that are 5 years and below in Akure, Nigeria.",
                      "Epidemiology of Plasmodium falciparum infection and drug resistance markers in Ota Area, Southwestern Nigeria.",
                      "Absence of Plasmodium falciparum artemisinin resistance gene mutations eleven years after the adoption of artemisinin-based combination therapy in Nigeria."
  
)

data.frame(check_recall(important_titles, new_results[, "title"]))
library(BiocManager)
BiocManager::install("EBImage")
library(metagear)

set.seed(123)
new_results_sample<- new_results %>% sample_frac(0.25) %>% select(journal,
                    abstract, title, volume, pages, author)
dim(new_results_sample)

new_results_sample["journal"]

############Initialize the data set; prime the study_reference########
theRefs<- effort_initialize(new_results_sample)
names(theRefs)

#############Distribute randomly################
the_Team<- c("Aaron_1", "Aaron_2", "Aaron_3")
set.seed(123)
theRefs_unscreened<- effort_distribute(theRefs, reviewers = the_Team)
theRefs_unscreened[c("STUDY_ID", "REVIEWERS")]
##############Distribute unevnely##############
set.seed(123)
theRefs_unscreened<- effort_distribute(theRefs, reviewers = the_Team, effort = 
                                           c(80, 10, 10), save_split = T)
theRefs_unscreened[c("STUDY_ID", "REVIEWERS")]
list.files(pattern = "effort")

#######Screening abstract of References###################
abstract_screener("effort_Aaron_1.csv", aReviewer = "Aaron_1",
                  reviewerColumnName = "REVIEWERS", unscreenedColumnName = "INCLUDE",
                  abstractColumnName = "abstract", titleColumnName = "title")

abstract_screener("effort_Aaron_2.csv", aReviewer = "Aaron_2",
                  reviewerColumnName = "REVIEWERS", unscreenedColumnName = "INCLUDE",
                  abstractColumnName = "abstract", titleColumnName = "title")

abstract_screener("effort_Aaron_3.csv", aReviewer = "Aaron_3",
                  reviewerColumnName = "REVIEWERS", unscreenedColumnName = "INCLUDE",
                  abstractColumnName = "abstract", titleColumnName = "title")

##I just concluded 25% of the search result. To complete the rest 75% 
##I assume I will have to repeat the same process with different names
## new_results_sample, new_results_sample1, new_results_sample2
##new_results_sample3 (25% each). Then finally I will the sum them up
##into a name e.g new_results_sample_sum<- c(new_results_sample,
##new_results_sample1,new_results_sample2,new_results_sample3)

########Merging Screened references########################
screenedTeam<- c("Aaron_1", "Aaron_2", "Aaron_3")
theRefs_screened<-effort_merge(reviewers = screenedTeam)
theRefs_screened[c("STUDY_ID", "REVIEWERS", "INCLUDE")]
theSummary<- effort_summary(theRefs_screened)

########PRISMA (prefferred reporting items for systematic reviews 
########and meta-analysis) plot

library(metagear)
phases<- c("START_PHASE: # of the studies identified through database searching",
           "START_PHASE: # of additional studies identified through other sources",
           "# of studies after duplicates removed",
           "# of studies with title and abstract screened",
           "EXCLUDE_PHASE: # of studies excluded",
           "# of full-text articles assessed for eligibility", 
           "EXCLUDE_PHASE: # of full-text articles excluded, not fitting eligibility criteria",
           "# of studies included in qualitative synthesis",
           "EXCLUDE_PHASE: # studies excluded, incomplete data reported",
           "# of final studies included in quantitative synthesis (meta-analysis)")
thePlot<- plot_PRISMA(phases, excludeDistance = 1, colWidth = 50)
### Generating different PRISMA plot layouts
plot_PRISMA(phases, design = "cinnamonMint", excludeDistance = 1, colWidth = 50)

plot_PRISMA(phases, design = "grey", excludeDistance = 1, colWidth = 50)

plot_PRISMA(phases, design = "greyMono", excludeDistance = 1, colWidth = 50)

plot_PRISMA(phases, design = "vintage", excludeDistance = 1, colWidth = 50)

thePlot1<- plot_PRISMA(phases, design = c(E="lightcoral", flatArrow = TRUE), excludeDistance = 1, colWidth = 50)
