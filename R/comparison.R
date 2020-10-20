eg_theme <- theme(
  text = element_text(family = 'Community'),
  legend.position = 'bottom',
  legend.key = element_blank(),
  legend.text = element_text(family = 'Community', size = 21),
  legend.title = element_blank(),
  legend.background = element_rect(fill = "#FDFAF6"),
  plot.title = element_text(family = 'Community', color = '#44712e', size = 45),
  plot.subtitle = element_text(family = 'Community', color = '#38434f', size = 20),
  axis.text.x = element_text(family = 'Community', color = '#38434f', size = 24),
  axis.text.y = element_text(family = 'Community', color = '#38434f', size = 24),
  panel.grid.major.y = element_line(colour = '#e9e5df', size = .2),
  panel.background = element_rect(fill='#FDFAF5',color='#FDFAF5'),
  plot.background = element_rect(fill = "#FDFAF5"),
  panel.grid.major.x = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_text(family = 'Community', face = 'bold', color = '#38434f', size = 36),
  plot.caption = element_text(hjust = 0, size = 18)
)

demo <- function(df,first_order) {
  result <- (df %>%
               group_by(first_order = get(first_order), industry_group_name_a) %>%
               summarise(total = n_distinct(member_id)) %>% mutate(pct = total/sum(total) * 100))

  result_1 <- (ggplot(data=result, aes(x=reorder(industry_group_name_a, pct), y=pct, fill=first_order))
               + geom_bar(stat = "identity", position = 'dodge') + xlab("Industry")
               + ylab("Percentage of Members")
               + coord_flip()
               + guides(fill=guide_legend(title=first_order))
               + eg_theme)

  result_1
}


transit <- function(df, first_order) {

  df <- (df %>% filter(gap > 0))
  result <- (ggplot(df, aes(gap, industry_group_name_b))
               + geom_boxplot()
               + facet_wrap(~ get(first_order))
               + labs(y="Destination Industry", x="Months", title = "Time To Transit To An Industry ")
               + eg_theme)
  result
}


#' Generates ggplots by comparing among different user-defined groups.
#'
#' `load_file()` loads .csv file that contains `u_egdata.base_job_tran_1step` column fields.
#' The function returns 2 ggplots:
#' 1) `transit` - Box plot for the time it takes to transit to various industries by user-defined groups
#' 2) `$demo` - Bar plot for the distribution of user-defined groups across various industries
#'
#'
#' @import magrittr
#' @import ggplot2
#' @import dplyr
#' @param file Path to the input file
#' @param group_by user-defined groups. By default, it will be gender.
#' @return `load_file()` returns 2 ggplots objects `transit` and `demo`
#' @author Kai Wei Tan <kaitan@linkedin.com>
#' @export
#' @examples
#'
#' #load library
#' library('comparison')
#'
#' #calling load_file function
#' test <- load_file("./dummy_data.csv", group_by = 'age_bracket')
#'
#' #Plot Box plot for the time it takes to transit to various industries by different age-bracket groups
#' test$transit
#'
#' #Bar plot for the distribution of different age-bracket groups across various industries
#' test$demo

load_file <- function(file, group_by = NULL){
  df <- read.csv(file)

  first_order <- ''
  if (is.null(group_by)) {
    first_order <- 'gender'
  } else {
    first_order <- group_by
  }

  result_1 <- demo(df,first_order)
  result_2 <- transit(df,first_order)

  return <- list("demo" = result_1, "transit" = result_2)
  }

#' Generates cosine histogram across all possible pairwise industries
#'
#' `load_cosine()` loads .csv file that contains `industry_group_name_a`, `industry_group_name_b` and `cosine_similarity_group` column fields.
#' The function returns 1 ggplot:
#' 1) `skills` - Histogram plot of cosine similarity for all possible pairwise industries
#'
#' @import magrittr
#' @import ggplot2
#' @import dplyr
#' @param file Path to the input file
#' @param field Column name of the cosine index. By default, `skills_similarity` is the column field.
#' @param industries Vector of the destination industries. By default, 'Software & IT Services', 'Finance', 'Health Care', 'Recreation & Travel' will be the destination industries.
#' @return `load_cosine()` returns 1 ggplot object `skills`
#' @author Kai Wei Tan <kaitan@linkedin.com>
#' @export
#' @examples
#'
#' #load library
#' library('comparison')
#'
#' #calling load_file function
#' test <- load_cosine("./dummy_data.csv", field = 'skill_similarity', industries = c('Corporate Services','Finance', 'Hardware & Networking', 'Manufacturing', 'Software & IT Services'))
#'
#' #Plot histogram across all possible pairwise industries
#' test$skills
#'
load_cosine <- function(file, field = NULL, industries=c('Software & IT Services', 'Finance', 'Health Care', 'Recreation & Travel')){
  df <- read.csv(file)
  df <- df %>% filter(industry_group_name_b %in% industries)

  cosine <- ''
  if (is.null(field)) {
    cosine <- 'skill_similarity'
  } else {
    cosine <- field
  }

  result <- ggplot(df, aes(get(cosine))) +
    geom_density() +
    eg_theme +
    labs(y = "Density", x='Similarity') + scale_x_continuous(breaks=seq(0, 1, 1)) +
    facet_grid(industry_group_name_b~industry_group_name_a, scales = "free")

  return <- list("skills" = result)
}



