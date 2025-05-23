% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BM25-class.R
\name{BM25}
\alias{BM25}
\title{BM25 Object}
\description{
Class to construct the BM25 search object
}
\examples{
corpus <- c(
  "The rabbit munched the orange carrot.",
  "The snake hugged the green lizard.",
  "The hedgehog impaled the orange orange.",
  "The squirrel buried the brown nut."
)
bm25 <- BM25$new(data = corpus, lang = "en",
                 metadata = data.frame(src = paste("file", 1:4)))
bm25$query("orange", max_n = 2)
bm25$query("orange")

## ------------------------------------------------
## Method `BM25$new`
## ------------------------------------------------

corpus <- c(
 "The rabbit munched the orange carrot.",
 "The snake hugged the green lizard.",
 "The hedgehog impaled the orange orange.",
 "The squirrel buried the brown nut."
)
bm25 <- BM25$new(data = corpus, lang = "en",
                 metadata = data.frame(src = paste("file", 1:4)))
bm25
bm25$get_data()

bm25$query("orange", max_n = 2)
bm25$query("orange", max_n = 3)
bm25$query("orange") # return all, same as max_n = Inf or NULL

## ------------------------------------------------
## Method `BM25$available_languages`
## ------------------------------------------------

BM25$new()$available_languages()

## ------------------------------------------------
## Method `BM25$get_data`
## ------------------------------------------------

BM25$new(data = letters, metadata = LETTERS)$get_data()

## ------------------------------------------------
## Method `BM25$get_lang`
## ------------------------------------------------

BM25$new()$get_lang()
BM25$new(lang = "en")$get_lang()
BM25$new(lang = "detect")$get_lang()

## ------------------------------------------------
## Method `BM25$print`
## ------------------------------------------------

BM25$new(data = letters, metadata = LETTERS)

## ------------------------------------------------
## Method `BM25$add_data`
## ------------------------------------------------

bm25 <- BM25$new()
bm25$add_data(letters, metadata = LETTERS)
bm25

## ------------------------------------------------
## Method `BM25$query`
## ------------------------------------------------

corpus <- c(
 "The rabbit munched the orange carrot.",
 "The snake hugged the green lizard.",
 "The hedgehog impaled the orange orange.",
 "The squirrel buried the brown nut."
)
bm25 <- BM25$new(data = corpus, lang = "en",
                 metadata = data.frame(src = paste("file", 1:4)))

bm25$query("orange", max_n = 2)
bm25$query("orange", max_n = 3)
bm25$query("orange", return_text = FALSE, return_metadata = FALSE)
bm25$query("orange", max_n = 3)
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BM25-new}{\code{BM25$new()}}
\item \href{#method-BM25-available_languages}{\code{BM25$available_languages()}}
\item \href{#method-BM25-get_data}{\code{BM25$get_data()}}
\item \href{#method-BM25-get_lang}{\code{BM25$get_lang()}}
\item \href{#method-BM25-print}{\code{BM25$print()}}
\item \href{#method-BM25-add_data}{\code{BM25$add_data()}}
\item \href{#method-BM25-query}{\code{BM25$query()}}
\item \href{#method-BM25-clone}{\code{BM25$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-new"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of a BM25 class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$new(data = NULL, lang = "detect", k1 = 1.2, b = 0.75, metadata = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{text data, a vector of strings. Note any preprocessing steps
(tolower, removing stopwords etc) need to have taken place before this!}

\item{\code{lang}}{language of the data, see self$available_languages(),
can also be "detect" to automatically detect the language,  default is "detect"}

\item{\code{k1}}{k1 parameter of BM25, default is 1.2}

\item{\code{b}}{b parameter of BM25, default is 0.75}

\item{\code{metadata}}{a data.frame with metadata for each document, default is NULL
must be a data.frame with the same number of rows containing arbitrary
metadata for each document, e.g. a file path or a URL}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
BM25 object
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{corpus <- c(
 "The rabbit munched the orange carrot.",
 "The snake hugged the green lizard.",
 "The hedgehog impaled the orange orange.",
 "The squirrel buried the brown nut."
)
bm25 <- BM25$new(data = corpus, lang = "en",
                 metadata = data.frame(src = paste("file", 1:4)))
bm25
bm25$get_data()

bm25$query("orange", max_n = 2)
bm25$query("orange", max_n = 3)
bm25$query("orange") # return all, same as max_n = Inf or NULL
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-available_languages"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-available_languages}{}}}
\subsection{Method \code{available_languages()}}{
Returns the available languages
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$available_languages()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a named character vector with language codes and their full names
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{BM25$new()$available_languages()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-get_data"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-get_data}{}}}
\subsection{Method \code{get_data()}}{
Returns the data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$get_data(add_metadata = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{add_metadata}}{whether to add metadata to the data, default is TRUE}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a data.frame with the data and metadata if available and selected
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{BM25$new(data = letters, metadata = LETTERS)$get_data()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-get_lang"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-get_lang}{}}}
\subsection{Method \code{get_lang()}}{
Returns the language used
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$get_lang()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a character string with the language code
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{BM25$new()$get_lang()
BM25$new(lang = "en")$get_lang()
BM25$new(lang = "detect")$get_lang()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-print"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-print}{}}}
\subsection{Method \code{print()}}{
Prints a BM25 object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$print(n = 5, nchar = 20)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{number of data to print, default is 5}

\item{\code{nchar}}{number of characters to print for each text, default is 20}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
the object invisible
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{BM25$new(data = letters, metadata = LETTERS)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-add_data"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-add_data}{}}}
\subsection{Method \code{add_data()}}{
Adds data to the BM25 object

This can be useful to add more data later on, note this will rebuild the engine.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$add_data(data, metadata = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{a vector of strings}

\item{\code{metadata}}{a data.frame with metadata for each document, default is NULL}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{bm25 <- BM25$new()
bm25$add_data(letters, metadata = LETTERS)
bm25
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-query"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-query}{}}}
\subsection{Method \code{query()}}{
Query the BM25 object for the N best matches
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$query(query, max_n = NULL, return_text = TRUE, return_metadata = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{query}}{the term to search for, note all preprocessing that was
applied to the text corpus initially needs to be already performed on
the term, e.g., tolower, removing stopwords etc}

\item{\code{max_n}}{the maximum number of results to return, default is all}

\item{\code{return_text}}{whether to return the text, default is TRUE}

\item{\code{return_metadata}}{whether to return metadata, default is TRUE}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a data.frame with the results
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{corpus <- c(
 "The rabbit munched the orange carrot.",
 "The snake hugged the green lizard.",
 "The hedgehog impaled the orange orange.",
 "The squirrel buried the brown nut."
)
bm25 <- BM25$new(data = corpus, lang = "en",
                 metadata = data.frame(src = paste("file", 1:4)))

bm25$query("orange", max_n = 2)
bm25$query("orange", max_n = 3)
bm25$query("orange", return_text = FALSE, return_metadata = FALSE)
bm25$query("orange", max_n = 3)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BM25-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BM25-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BM25$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
