## Install Script Dependencies ----

# All packages needed for script
pkg_list = c("pdftools", "tm", "wordcloud","wordcloud2", "webshot", "htmlwidgets")
# Determine what packages are NOT installed already.
to_install_pkgs = pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
# Install the missing packages
if(length(to_install_pkgs)) {
    install.packages(to_install_pkgs, repos = "https://cloud.r-project.org")
}
# Load all packages
pkg_loaded = sapply(pkg_list, require, character.only = TRUE)


## Project Directory ----
proj_dir = file.path("imps-2019-wordcloud")

## Set data file location ----

imps_abstract_csv_file = file.path(proj_dir, "all-imps-2019-abstracts.csv")
imps_wordcloud_name = "imps-2019-wordcloud"
imps_meeting_location = "2019 International Meeting of the Psychometric Society\nCentro de Extensión at the Pontificia Universidad Católica de Chile\nSantiago, Chile | July 15-19, 2019"


## Download the IMPS 2018 Abstract Text ----

# # Set target URL
# imps_url = "https://www.psychometricsociety.org/sites/default/files/IMPS%202018%20Talk%20Abstracts.pdf"
# imps_abstract_pdf_file = file.path(
#     proj_dir, tolower(gsub(" ", "-", URLdecode(basename(imps_url))))
# )
#
# # Download the PDF
# download.file(imps_url, imps_abstract_pdf_file, mode = "wb")

## Construct Text Corpus ----

# Parse the PDF text into an R text vector
## txt = pdf_text(imps_abstract_pdf_file)

txt = read.csv(imps_abstract_csv_file)[[1]]

# Convert to a tm corpus
abstract_corpus = VCorpus(VectorSource(txt))

## Clean Text Corpus ----

clean_abstract_corpus = function(corpus){
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeWords, stopwords("en"))
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, removeNumbers)
    corpus = tm_map(corpus, stripWhitespace)
    corpus = tm_map(corpus, PlainTextDocument)
    corpus = tm_map(corpus, removeWords,
                    c("also", "use", "thus", "given", "well", "many",
                      "may", "via", "way", "paper", "can", "using", "used",
                      "shown", "apply", "provide", "will", "however",
                      "often",
                      "one", "two", "three", "four"))
    return(corpus)
}

abstract_corpus_cleaned = clean_abstract_corpus(abstract_corpus)

## Obtain Frequencies for Word Usage ----

dtm_abstract = TermDocumentMatrix(abstract_corpus_cleaned)

matrix_dtm = as.matrix(dtm_abstract)
sorted_matrix_dtm = sort(rowSums(matrix_dtm), decreasing = TRUE)
most_popular_words = data.frame(word = names(sorted_matrix_dtm),
                                freq = sorted_matrix_dtm)
head(most_popular_words, 10)


## Generate a Wordcloud based on popular words ----

set.seed(55531)

imps_wordcloud_export_loc =
    file.path(
        proj_dir,
        paste0(imps_wordcloud_name, "-wordcloud-base-r.pdf")
    )



# Write to a PDF file
pdf(imps_wordcloud_export_loc, 8, 11)
layout(matrix(c(1, 2), nrow = 2), heights = c(2, 3))
par(mar = rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, imps_meeting_location)
wordcloud(
    words = most_popular_words$word,
    freq = most_popular_words$freq,
    min.freq = 10,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
)
dev.off()


## Generate WordCloud in HTML and Translate to PDF ----

# Attempt wordcloud using HTML

imps_wordcloud_export_loc =
    file.path(
        proj_dir,
        paste0(imps_wordcloud_name, "-wordcloud2-pkg.pdf")
    )

# Check if phantomjs is installed, if not then install it.
if(is.null(webshot:::find_phantom()) ) {
    webshot::install_phantomjs()
}

subset_popular_words = most_popular_words[most_popular_words$freq > 30, ]

subset_popular_words_meeting = rbind(
    data.frame(word = c("2019 IMPS",
                        "Centro de Extensión",
                        "Pontificia Universidad",
                        "Católica de Chile",
                        "Santiago, Chile",
                        "July 15-19, 2019"),
               freq = c(536, 535, 534,
                        533, 532, 531),
               stringsAsFactors = FALSE),
    subset_popular_words
)

idx = subset_popular_words_meeting$freq > 300

subset_popular_words_meeting$freq[idx] =
    subset_popular_words_meeting$freq[idx] - 100

# Construct the graph
my_graph = wordcloud2(
    subset_popular_words_meeting,
    size = .7,
    backgroundColor = "black",
    color = "random-light",
    gridSize = 1,
    rotateRatio = 0.8,
    shape = "cloud"
)
my_graph

# Problematic to add a title...

# Save the graph as HTML
htmlwidgets::saveWidget(my_graph, "tmp.html", selfcontained = F)

# Save to a PDF
webshot::webshot(
    "tmp.html",
    imps_wordcloud_export_loc,
    delay = 5,
    vwidth = 800,
    vheight = 800
)

# Delete artifacts
file.remove("tmp.html")
unlink("tmp_files", recursive = TRUE)

## Generate with ggplot2 wordcloud ----


# gwordmap = ggwordcloud2(
#     most_popular_words[most_popular_words$freq > 20, ],
#              #backgroundColor = "black",
#              color = "random-light",
#              #gridSize = 1,
#              rotateRatio = 0.8,
#              shape = "diamond") +
#     labs(title = imps_meeting_location) +
#     theme(panel.background = element_rect(fill = "black"))

subset_popular_words = most_popular_words[most_popular_words$freq > 20, ]
gwordmap = ggplot(subset_popular_words,
                  aes(
                      label = word,
                      size = freq,
                      color = word
                  )) +
    geom_text_wordcloud_area(
        mapping = aes(angle = 45 * sample(
            -2:2,
            size = nrow(subset_popular_words),
            replace = TRUE,
            prob = c(1, 2, 6, 2, 1)
        )),
        eccentricity = .3,
        #rstep = 0.02,
        shape = "square"
    ) +
    scale_size_area(max_size = 25) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "black"),
          plot.title = element_text(color = "white", hjust = 0.5, vjust = -5, size = 20)) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    labs(title = imps_meeting_location)


ggsave(gwordmap, file = "ggwordcloud-demo.pdf",
       height = 10, width = 10)
