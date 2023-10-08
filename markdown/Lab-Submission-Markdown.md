Business Intelligence Lab Submission Markdown
================
<Specify your group name here>
<Specify the date when you submitted the lab>

- [Student Details](#student-details)
- [Setup Chunk](#setup-chunk)
- [Customize the Visualizations, Tables, and Colour
  Scheme](#customize-the-visualizations-tables-and-colour-scheme)
  - [purple-grey colour scheme](#purple-grey-colour-scheme)
  - [Custom theme for visualizations](#custom-theme-for-visualizations)
  - [Table Scheme](#table-scheme)
- [Load the Dataset](#load-the-dataset)
- [Create a subset of the data](#create-a-subset-of-the-data)
- [Decorated tabular output](#decorated-tabular-output)
- [Decorated visual bar chart](#decorated-visual-bar-chart)
- [Data Cleansing for Qualitative Data
  Contractions](#data-cleansing-for-qualitative-data-contractions)
- [Stemming/Lemmatization](#stemminglemmatization)
- [Stopword Removal, Short Word Removal, and
  Censorship](#stopword-removal-short-word-removal-and-censorship)
- [Word Count](#word-count)
  - [Word count per gender](#word-count-per-gender)
  - [Word count per group](#word-count-per-group)
  - [Top 10 words per gender](#top-10-words-per-gender)
  - [Top 10 words per group](#top-10-words-per-group)
- [Word Cloud](#word-cloud)
- [Term Frequency - Inverse Document Frequency
  (TF-IDF)](#term-frequency---inverse-document-frequency-tf-idf)
  - [TF-IDF Score per Group](#tf-idf-score-per-group)
  - [TF-IDF Score per Gender](#tf-idf-score-per-gender)

# Student Details

<table>
<colgroup>
<col style="width: 33%" />
<col style="width: 66%" />
</colgroup>
<tbody>
<tr class="odd">
<td><strong>Student ID Numbers and Names of Group Members</strong></td>
<td><p><em>&lt;list one student name, group, and ID per line; you should
be between 2 and 5 members per group&gt;</em></p>
<ol type="1">
<li><p>119630 - A - Venus Karanja</p></li>
<li><p>ID - Group - Name</p></li>
<li><p>ID - Group - Name</p></li>
<li><p>ID - Group - Name</p></li>
<li><p>ID - Group - Name</p></li>
</ol></td>
</tr>
<tr class="even">
<td><strong>GitHub Classroom Group Name</strong></td>
<td>BEASTS</td>
</tr>
<tr class="odd">
<td><strong>Course Code</strong></td>
<td>BBT4206</td>
</tr>
<tr class="even">
<td><strong>Course Name</strong></td>
<td>Business Intelligence II</td>
</tr>
<tr class="odd">
<td><strong>Program</strong></td>
<td>Bachelor of Business Information Technology</td>
</tr>
<tr class="even">
<td><strong>Semester Duration</strong></td>
<td>21<sup>st</sup> August 2023 to 28<sup>th</sup> November 2023</td>
</tr>
</tbody>
</table>

# Setup Chunk

**Note:** the following “*KnitR*” options have been set as the
defaults:  
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy.opts = list(width.cutoff = 80), tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

**Note:** the following “*R Markdown*” options have been set as the
defaults:

> output:  
>   
> github_document:  
> toc: yes  
> toc_depth: 4  
> fig_width: 6  
> fig_height: 4  
> df_print: default  
>   
> editor_options:  
> chunk_output_type: console

# Customize the Visualizations, Tables, and Colour Scheme

## purple-grey colour scheme

``` r
# Purple Grey Color Schemes
purple_grey_colours_11 <- c("#E2B6CE", "#B9BCC2", "#888A8E", "#582340", "#E4E0C8",
    "#D1B0E0", "#B191C9", "#A27DAF", "#8D6CAB", "#7B5E9A", "#6A5290")

purple_grey_colours_6 <- c("#E2B6CE", "#B9BCC2", "#888A8E", "#A27DAF", "#8D6CAB",
    "#7B5E9A")

purple_grey_colours_4 <- c("#E2B6CE", "#B9BCC2", "#A27DAF", "#6A5290")

purple_grey_colours_2 <- c("#E2B6CE", "#6A5290")

purple_grey_colours_1 <- c("#E2B6CE")
```

## Custom theme for visualizations

``` r
purple_grey_theme <- function() {
    theme(axis.ticks = element_line(linewidth = 1, linetype = "dashed", lineend = NULL,
        color = "#582340"), axis.text = element_text(face = "bold", color = "#582340",
        size = 12, hjust = 0.5), axis.title = element_text(face = "bold", color = "#582340",
        size = 14, hjust = 0.5), plot.title = element_text(face = "bold", color = "#582340",
        size = 16, hjust = 0.5), panel.grid = element_line(linewidth = 0.1, linetype = "dashed",
        lineend = NULL, color = "#582340"), panel.background = element_rect(fill = "#E4E1C8"),
        legend.title = element_text(face = "plain", color = "#582340", size = 12,
            hjust = 0), legend.position = "right")
}
```

## Table Scheme

``` r
kable_theme <- function(dat, caption) {
    kable(dat, "html", escape = FALSE, caption = caption) %>%
        kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
            full_width = FALSE)
}
```

# Load the Dataset

``` r
library(readr)
student_performance_dataset <- read_csv("data/20230412-20230719-BI1-BBIT4-1-StudentPerformanceDataset.CSV",
    col_types = cols(class_group = col_factor(levels = c("A", "B", "C")), gender = col_factor(levels = c("1",
        "0")), YOB = col_date(format = "%Y"), regret_choosing_bi = col_factor(levels = c("1",
        "0")), drop_bi_now = col_factor(levels = c("1", "0")), motivator = col_factor(levels = c("1",
        "0")), read_content_before_lecture = col_factor(levels = c("1", "2", "3",
        "4", "5")), anticipate_test_questions = col_factor(levels = c("1", "2", "3",
        "4", "5")), answer_rhetorical_questions = col_factor(levels = c("1", "2",
        "3", "4", "5")), find_terms_I_do_not_know = col_factor(levels = c("1", "2",
        "3", "4", "5")), copy_new_terms_in_reading_notebook = col_factor(levels = c("1",
        "2", "3", "4", "5")), take_quizzes_and_use_results = col_factor(levels = c("1",
        "2", "3", "4", "5")), reorganise_course_outline = col_factor(levels = c("1",
        "2", "3", "4", "5")), write_down_important_points = col_factor(levels = c("1",
        "2", "3", "4", "5")), space_out_revision = col_factor(levels = c("1", "2",
        "3", "4", "5")), studying_in_study_group = col_factor(levels = c("1", "2",
        "3", "4", "5")), schedule_appointments = col_factor(levels = c("1", "2",
        "3", "4", "5")), goal_oriented = col_factor(levels = c("1", "0")), spaced_repetition = col_factor(levels = c("1",
        "2", "3", "4")), testing_and_active_recall = col_factor(levels = c("1", "2",
        "3", "4")), interleaving = col_factor(levels = c("1", "2", "3", "4")), categorizing = col_factor(levels = c("1",
        "2", "3", "4")), retrospective_timetable = col_factor(levels = c("1", "2",
        "3", "4")), cornell_notes = col_factor(levels = c("1", "2", "3", "4")), sq3r = col_factor(levels = c("1",
        "2", "3", "4")), commute = col_factor(levels = c("1", "2", "3", "4")), study_time = col_factor(levels = c("1",
        "2", "3", "4")), repeats_since_Y1 = col_integer(), paid_tuition = col_factor(levels = c("0",
        "1")), free_tuition = col_factor(levels = c("0", "1")), extra_curricular = col_factor(levels = c("0",
        "1")), sports_extra_curricular = col_factor(levels = c("0", "1")), exercise_per_week = col_factor(levels = c("0",
        "1", "2", "3")), meditate = col_factor(levels = c("0", "1", "2", "3")), pray = col_factor(levels = c("0",
        "1", "2", "3")), internet = col_factor(levels = c("0", "1")), laptop = col_factor(levels = c("0",
        "1")), family_relationships = col_factor(levels = c("1", "2", "3", "4", "5")),
        friendships = col_factor(levels = c("1", "2", "3", "4", "5")), romantic_relationships = col_factor(levels = c("0",
            "1", "2", "3", "4")), spiritual_wellnes = col_factor(levels = c("1",
            "2", "3", "4", "5")), financial_wellness = col_factor(levels = c("1",
            "2", "3", "4", "5")), health = col_factor(levels = c("1", "2", "3", "4",
            "5")), day_out = col_factor(levels = c("0", "1", "2", "3")), night_out = col_factor(levels = c("0",
            "1", "2", "3")), alcohol_or_narcotics = col_factor(levels = c("0", "1",
            "2", "3")), mentor = col_factor(levels = c("0", "1")), mentor_meetings = col_factor(levels = c("0",
            "1", "2", "3")), `Attendance Waiver Granted: 1 = Yes, 0 = No` = col_factor(levels = c("0",
            "1")), GRADE = col_factor(levels = c("A", "B", "C", "D", "E"))), locale = locale())

View(student_performance_dataset)
```

# Create a subset of the data

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
evaluation_per_group_per_gender <- student_performance_dataset %>%
    mutate(`Student's Gender` = ifelse(gender == 1, "Male", "Female")) %>%
    select(class_group, gender, `Student's Gender`, `Average Level of Learning Attained Rating`) %>%
    filter(!is.na(`Average Level of Learning Attained Rating`)) %>%
    group_by(class_group, `Student's Gender`) %>%
    summarise(average_learning_rating = mean(`Average Level of Learning Attained Rating`,
        na.rm = TRUE)) %>%
    arrange(class_group, `Student's Gender`) %>%
    arrange(desc(average_learning_rating), .by_group = TRUE)
```

    ## `summarise()` has grouped output by 'class_group'. You can override using the
    ## `.groups` argument.

``` r
# Print the resulting data frame print(evaluation_per_group_per_gender)

# If you want a tabular output in the RStudio viewer, you can use:
View(evaluation_per_group_per_gender)
```

# Decorated tabular output

``` r
library(dplyr)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
evaluation_per_group_per_gender %>%
    rename(class_group = class_group) %>%
    rename(`Average Level of Learning Attained Rating` = average_learning_rating) %>%
    select(class_group, `Student's Gender`, `Average Level of Learning Attained Rating`) %>%
    mutate(`Average Level of Learning Attained Rating` = cell_spec(`Average Level of Learning Attained Rating`,
        color = ifelse(`Average Level of Learning Attained Rating` >= 70, "green",
            "red"))) %>%
    kable("html", escape = FALSE, align = "c", caption = "Course Evaluation Rating per Group and per Gender") %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), full_width = FALSE) %>%
    column_spec(3, background = "#E2B6CE")
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Course Evaluation Rating per Group and per Gender
</caption>
<thead>
<tr>
<th style="text-align:center;">
class_group
</th>
<th style="text-align:center;">
Student’s Gender
</th>
<th style="text-align:center;">
Average Level of Learning Attained Rating
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
A
</td>
<td style="text-align:center;">
Female
</td>
<td style="text-align:center;background-color: #E2B6CE !important;">
<span style="     color: red !important;">4.6</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
A
</td>
<td style="text-align:center;">
Male
</td>
<td style="text-align:center;background-color: #E2B6CE !important;">
<span style="     color: red !important;">4.11538461538461</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
B
</td>
<td style="text-align:center;">
Female
</td>
<td style="text-align:center;background-color: #E2B6CE !important;">
<span style="     color: red !important;">4.19230769230769</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
B
</td>
<td style="text-align:center;">
Male
</td>
<td style="text-align:center;background-color: #E2B6CE !important;">
<span style="     color: red !important;">4.15217391304348</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
C
</td>
<td style="text-align:center;">
Female
</td>
<td style="text-align:center;background-color: #E2B6CE !important;">
<span style="     color: red !important;">3.95</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
C
</td>
<td style="text-align:center;">
Male
</td>
<td style="text-align:center;background-color: #E2B6CE !important;">
<span style="     color: red !important;">3.85714285714286</span>
</td>
</tr>
</tbody>
</table>

# Decorated visual bar chart

``` r
library(ggplot2)
evaluation_per_group_per_gender %>%
    ggplot() + geom_bar(aes(x = class_group, y = average_learning_rating, fill = `Student's Gender`),
    stat = "identity", position = "dodge") + expand_limits(y = 0) + purple_grey_theme() +
    scale_fill_manual(values = purple_grey_colours_4) + ggtitle("Course Evaluation Learning Rating per Group and per Gender") +
    labs(x = "Class Group", y = "Average Rating")
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%208%20Code%20Chunk-1.png)<!-- -->

# Data Cleansing for Qualitative Data Contractions

``` r
#function for removing spectical characters
remove_special_characters <- function(doc) {
  gsub("[^a-zA-Z]+", " ", doc, ignore.case = TRUE)
}
#function for contractions 
expand_contractions <- function(doc) {
  doc <- gsub("It's", "it is", doc, ignore.case = TRUE)
  doc <- gsub("I'm", "I am", doc, ignore.case = TRUE)
  doc <- gsub("Don't", "do not", doc, ignore.case = TRUE)
  doc <- gsub("I've", "I have", doc, ignore.case = TRUE)
  doc <- gsub("Isn't", "is not", doc, ignore.case = TRUE)
  doc <- gsub("We're", " we are", doc, ignore.case = TRUE)
  doc <- gsub("That's", " that is", doc, ignore.case = TRUE)
  doc <- gsub("you're", "you are", doc, ignore.case = TRUE)
  doc <- gsub("he's", "he is", doc, ignore.case = TRUE)
  doc <- gsub("she's", "she is", doc, ignore.case = TRUE)
  doc <- gsub("they're", "they are", doc, ignore.case = TRUE)
  doc <- gsub("I'll", "I will", doc, ignore.case = TRUE)
  doc <- gsub("you'll", "you will", doc, ignore.case = TRUE)
  doc <- gsub("he'll", "he will", doc, ignore.case = TRUE)
  doc <- gsub("she'll", "she will", doc, ignore.case = TRUE)
  doc <- gsub("it'll", "it will", doc, ignore.case = TRUE)
  doc <- gsub("we'll", "we will", doc, ignore.case = TRUE)
  doc <- gsub("they'll", "they will", doc, ignore.case = TRUE)
  doc <- gsub("won't", "will not", doc, ignore.case = TRUE)
  doc <- gsub("can't", "cannot", doc, ignore.case = TRUE)
  doc <- gsub("n't", " not", doc, ignore.case = TRUE)
  doc <- gsub("it s", " it is", doc, ignore.case = TRUE)
  doc <- gsub("its", " it is", doc, ignore.case = TRUE)
  doc <- gsub("It’s", " it is", doc, ignore.case = TRUE)
  return(doc)
}
#Evaluation likes
evaluation_likes <- student_performance_dataset %>%
  mutate(`Student's Gender` =
           ifelse(gender == 1, "Male", "Female")) %>%
  rename(`Class Group` = class_group) %>%
  rename(Likes = `D - 1. \nWrite two things you like about the teaching and learning in this unit so far.`) %>% # nolint
  select(`Class Group`,
         `Student's Gender`, `Average Level of Learning Attained Rating`,
         Likes) %>%
  filter(!is.na(`Average Level of Learning Attained Rating`)) %>%
  arrange(`Class Group`)

#expanding contractions
evaluation_likes$Likes <- sapply(evaluation_likes$Likes, expand_contractions) # nolint

#removing special characters
evaluation_likes$Likes <- sapply(evaluation_likes$Likes, remove_special_characters) # nolint

# Convert to lowercase
evaluation_likes <- evaluation_likes %>%
  mutate(Likes = tolower(Likes))  

#save as csv and load the dataset
write.csv(evaluation_likes,
          file = "data/likes_cleansed.csv",
          row.names = FALSE)
likes_cleansed_dataset <- read.csv("data/likes_cleansed.csv")

View(evaluation_likes)
```

# Stemming/Lemmatization

``` r
library(textstem)
```

    ## Loading required package: koRpus.lang.en

    ## Loading required package: koRpus

    ## Loading required package: sylly

    ## For information on available language packages for 'koRpus', run
    ## 
    ##   available.koRpus.lang()
    ## 
    ## and see ?install.koRpus.lang()

    ## 
    ## Attaching package: 'koRpus'

    ## The following object is masked from 'package:readr':
    ## 
    ##     tokenize

``` r
lemma_likes <- make_lemma_dictionary(evaluation_likes$Likes, engine = "lexicon")

View(lemma_likes)
```

# Stopword Removal, Short Word Removal, and Censorship

``` r
# Load necessary libraries
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr) 

# Assuming you have a dataset named evaluation_likes with a column named Likes

# Define a list of undesirable words to censor
undesirable_words <- c("wow", "lol", "none", "na")

# Load the stop words dataset (if not already loaded)
data("stop_words")

# Perform text preprocessing
evaluation_likes_filtered <- evaluation_likes %>%
  unnest_tokens(word, Likes) %>%
  anti_join(stop_words, by = "word") %>%  # Remove stopwords
  filter(!word %in% undesirable_words) %>%  # Censor undesirable words
  filter(nchar(word) > 3) %>%  # Remove short words
  distinct() %>%  # Remove duplicate words
  rename(`Likes (tokenized)` = word)  # Rename the column

# Save the filtered data as a CSV file
write.csv(evaluation_likes_filtered,
          file = "data/likes_filtered.csv",
          row.names = FALSE)

# Sample data
View(evaluation_likes_filtered)
```

# Word Count

## Word count per gender

``` r
library(dplyr)
library(kableExtra)

word_count_per_gender_likes <- evaluation_likes_filtered %>%
    group_by(`Student's Gender`) %>%
    summarise(num_words = n()) %>%
    arrange(desc(num_words))

# Define a function to style the cells with background color
style_cells <- function(x) {
    cell_spec(x, color = "black", background = ifelse(x >= 10, "#E2B6CE", "#E4E0C8"))
}

word_count_per_gender_likes %>%
    mutate(num_words = style_cells(num_words)) %>%
    rename(`Number of Words` = num_words) %>%
    kable("html", escape = FALSE, align = "c", caption = "Number of Significant Words in Evaluation Likes 
                   per Gender: Minus contractions, special characters, 
                   stopwords, short words, and censored words.") %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), full_width = FALSE)
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Number of Significant Words in Evaluation Likes per Gender: Minus
contractions, special characters, stopwords, short words, and censored
words.
</caption>
<thead>
<tr>
<th style="text-align:center;">
Student’s Gender
</th>
<th style="text-align:center;">
Number of Words
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Male
</td>
<td style="text-align:center;">
<span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: #E2B6CE !important;">253</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
Female
</td>
<td style="text-align:center;">
<span style="     color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: #E2B6CE !important;">190</span>
</td>
</tr>
</tbody>
</table>

## Word count per group

``` r
library(dplyr)
library(kableExtra)

word_count_per_group <- evaluation_likes_filtered %>%
    group_by(`Class Group`) %>%
    summarise(num_words = n()) %>%
    arrange(desc(num_words))

# Create a function to format the cell with a colored bar
color_bar_cell <- function(x) {
    cell_spec(x, color = "white", background = (scales::col_numeric(palette = c("#E2B6CE",
        "#E2B6CE"), domain = NULL))(x))
}

word_count_per_group %>%
    mutate(`Number of Words` = color_bar_cell(num_words)) %>%
    select(-num_words) %>%
    kable("html", escape = FALSE, align = "c", caption = "Number of Significant Words in Evaluation Likes 
                   per Group: Minus contractions, special characters, 
                   stopwords, short words, and censored words.") %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), full_width = FALSE)
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Number of Significant Words in Evaluation Likes per Group: Minus
contractions, special characters, stopwords, short words, and censored
words.
</caption>
<thead>
<tr>
<th style="text-align:center;">
Class Group
</th>
<th style="text-align:center;">
Number of Words
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
C
</td>
<td style="text-align:center;">
<span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: #E2B6CE !important;">212</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
B
</td>
<td style="text-align:center;">
<span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: #E2B6CE !important;">147</span>
</td>
</tr>
<tr>
<td style="text-align:center;">
A
</td>
<td style="text-align:center;">
<span style="     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: #E2B6CE !important;">84</span>
</td>
</tr>
</tbody>
</table>

## Top 10 words per gender

``` r
popular_words <- evaluation_likes_filtered %>%
    group_by(`Student's Gender`) %>%
    count(`Likes (tokenized)`, `Student's Gender`, sort = TRUE) %>%
    slice(seq_len(10)) %>%
    ungroup() %>%
    arrange(`Student's Gender`, n) %>%
    mutate(row = row_number())

popular_words %>%
    ggplot(aes(row, n, fill = `Student's Gender`)) + geom_col(fill = purple_grey_colours_1) +
    purple_grey_theme() + labs(x = "Word in Course Evaluation", y = "Number of Times Used (Term Frequency)") +
    ggtitle("Most Frequently Used Words in Course Evaluation Likes per Gender") +
    facet_wrap(~`Student's Gender`, scales = "free") + scale_x_continuous(breaks = popular_words$row,
    labels = popular_words$`Likes (tokenized)`) + coord_flip()
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%2014%20Code%20Chunk-1.png)<!-- -->

## Top 10 words per group

``` r
popular_words <- evaluation_likes_filtered %>%
    group_by(`Class Group`) %>%
    count(`Likes (tokenized)`, `Class Group`, sort = TRUE) %>%
    slice(seq_len(10)) %>%
    ungroup() %>%
    arrange(`Class Group`, n) %>%
    mutate(row = row_number())

popular_words %>%
    ggplot(aes(row, n, fill = `Class Group`)) + geom_col(fill = purple_grey_colours_1) +
    purple_grey_theme() + labs(x = "Word in Course Evaluation", y = "Number of Times Used") +
    ggtitle("Most Frequently Used Words in Course Evaluation Likes per 
          Class Group") +
    facet_wrap(~`Class Group`, scales = "free") + scale_x_continuous(breaks = popular_words$row,
    labels = popular_words$`Likes (tokenized)`) + coord_flip()
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%2015%20Code%20Chunk-1.png)<!-- -->

# Word Cloud

``` r
library(dplyr)
library(wordcloud2)

evaluation_likes_filtered_cloud <- evaluation_likes_filtered %>% # nolint
  count(`Likes (tokenized)`, sort = TRUE)

wordcloud2(evaluation_likes_filtered_cloud, size = .6)
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%2016%20Code%20Chunk-1.png)<!-- -->

# Term Frequency - Inverse Document Frequency (TF-IDF)

## TF-IDF Score per Group

``` r
library(ggplot2) 
popular_tfidf_words_likes <- evaluation_likes_filtered %>% # nolint
  unnest_tokens(word, `Likes (tokenized)`) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Likes (tokenized)` = word) %>%
  select(`Class Group`, `Student's Gender`,
         `Average Level of Learning Attained Rating`, `Likes (tokenized)`) %>%
  count(`Class Group`, `Likes (tokenized)`, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(`Likes (tokenized)`, `Class Group`, n)

top_popular_tfidf_words <- popular_tfidf_words_likes %>%
  arrange(desc(tf_idf)) %>%
  mutate(`Likes (tokenized)` =
           factor(`Likes (tokenized)`,
                  levels = rev(unique(`Likes (tokenized)`)))) %>%
  group_by(`Class Group`) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Class Group`, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, fill = `Class Group`)) +
  geom_col(fill = purple_grey_colours_1) +
  purple_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "TF-IDF Score") +
  ggtitle("Important Words using TF-IDF by Chart Level") +
  ggtitle("Most Important Words by TF-IDF Score in Course Evaluation Likes per 
      Class Group") +
  facet_wrap(~`Class Group`, scales = "free") +
  scale_x_continuous(
                     breaks = top_popular_tfidf_words$row,
                     labels = top_popular_tfidf_words$`Likes (tokenized)`) +
  coord_flip()
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%2017%20Code%20Chunk-1.png)<!-- -->

## TF-IDF Score per Gender

``` r
popular_tfidf_words_likes <- evaluation_likes_filtered %>% # nolint
  unnest_tokens(word, `Likes (tokenized)`) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  rename(`Likes (tokenized)` = word) %>%
  select(`Class Group`, `Student's Gender`,
         `Average Level of Learning Attained Rating`, `Likes (tokenized)`) %>%
  count(`Class Group`, `Likes (tokenized)`, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(`Likes (tokenized)`, `Class Group`, n)

top_popular_tfidf_words <- popular_tfidf_words_likes %>%
  arrange(desc(tf_idf)) %>%
  mutate(`Likes (tokenized)` =
           factor(`Likes (tokenized)`,
                  levels = rev(unique(`Likes (tokenized)`)))) %>%
  group_by(`Class Group`) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(`Class Group`, tf_idf) %>%
  mutate(row = row_number())

top_popular_tfidf_words %>%
  ggplot(aes(x = row, tf_idf, fill = `Class Group`)) +
  geom_col(fill = purple_grey_colours_1) +
  purple_grey_theme() +
  labs(x = "Word in Course Evaluation", y = "TF-IDF Score") +
  ggtitle("Important Words using TF-IDF by Chart Level") +
  ggtitle("Most Important Words by TF-IDF Score in Course Evaluation Likes per 
      Class Group") +
  facet_wrap(~`Class Group`, scales = "free") +
  scale_x_continuous(
                     breaks = top_popular_tfidf_words$row,
                     labels = top_popular_tfidf_words$`Likes (tokenized)`) +
  coord_flip()
```

![](Lab-Submission-Markdown_files/figure-gfm/Your%2018%20Code%20Chunk-1.png)<!-- -->
