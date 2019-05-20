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

## Download the IMPS 2018 Abstract Text ----

# Set target URL
imps_url = "https://www.psychometricsociety.org/sites/default/files/IMPS%202018%20Talk%20Abstracts.pdf"
imps_abstract_pdf_file = file.path(
    proj_dir, tolower(gsub(" ", "-", URLdecode(basename(imps_url))))
)

# Download the PDF
download.file(imps_url, imps_abstract_pdf_file, mode = "wb")

## Construct Text Corpus ----

# Parse the PDF text into an R text vector
txt = pdf_text(imps_abstract_pdf_file)

# Convert to a tm corpus
abstract_corpus = VCorpus(VectorSource(txt))

## Clean Text Corpus ----

clean_abstract_corpus = function(corpus){
    corpus = tm_map(corpus, stripWhitespace)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, removeWords, stopwords("en"))
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
    paste0(tools::file_path_sans_ext(imps_abstract_pdf_file),
           "-wordcloud.pdf")

# Write to a PDF file
pdf(imps_wordcloud_export_loc, 8, 11)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "2018 International Meeting of the Psychometric Society\nColumbia University\nNew York, USA\nJuly 9-13, 2018")
wordcloud(
    words = most_popular_words$word,
    freq = most_popular_words$freq,
    min.freq = 10,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
)


## Generate WordCloud in HTML and Translate to PDF ----

# Attempt wordcloud using HTML

imps_wordcloud_export_loc =
    paste0(tools::file_path_sans_ext(imps_abstract_pdf_file),
           "-wordcloud-big.pdf")

# Install phantomjs
webshot::install_phantomjs()

# Construct the graph
my_graph = wordcloud2(
    most_popular_words[most_popular_words$freq > 20, ],
    size = 1.6,
    backgroundColor = "black",
    color = "random-light",
    gridSize = 1,
    rotateRatio = 0.8,
    shape = "diamond"
)

# Save the graph as HTML
htmlwidgets::saveWidget(my_graph, "tmp.html", selfcontained = F)

# Save to a PDF
webshot::webshot(
    "tmp.html",
    imps_wordcloud_export_loc,
    delay = 5,
    vwidth = 480,
    vheight = 480
)

# Delete artifacts
file.remove("tmp.html")
unlink("tmp_files", recursive = TRUE)
