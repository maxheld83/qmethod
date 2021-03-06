\name{make.cards}
\alias{make.cards}
\title{Q methodology: produce printable cards for Q study with ID and full item wording}
\description{
  Creates cards for administering a Q study.
  Full item wordings are printed on the front of business cards and item IDs on the back.
}
\usage{
  make.cards(
    q.set,
    study.language = NULL,
    paper.format = "AveryZweckformC32010.Rnw",
    img.dir = NULL,
    output.pdf = TRUE,
    manual.lookup = NULL,
    wording.font.size = NULL,
    file.name = "QCards",
    babel.language = NULL,
    show.handles = FALSE,
    duplex.double = FALSE
  )
}

\arguments{
  \item{q.set}{
    A matrix with handles as row names ("\code{q-is-great}", for example), languages (if applicable) in columns, as produced by \code{\link{build.q.set}}.
    If the below \code{img.dir} argument is specified for graphical stimulus, file names of images (without extension or path) must be in columns.
  }
  \item{study.language}{
  	A character vector of length 1.
    Must be one of the languages from the column names in the specified \code{q.set} (which will be the same as the respective Q concourse object).
  	Defaults to \code{NULL}, in which case the first column from \code{q.set} is selected.
  }
  \item{paper.format}{
  	A character vector of length 1, choosing among available templates of business card sheets.
  	Defaults to \code{"AveryZweckformC32010.Rnw"}.
    Must include file extension of the template.
  }
  \item{img.dir}{
    A directory of images, if item stimuli are images, rather than text.
    Filenames of images (without extension or path) must be in colums of the matrix supplied to the above argument \code{q.set}.
    Avoid special characters in filenames.
  }
  \item{output.pdf}{
  	Logical.
  	If \code{TRUE}, function invokes \code{knit2pdf} to create a PDF in the workspace.
  	If \code{FALSE}, function invokes \code{knit} to return only a .TEX in the workspace, may be preferable if no \href{http://www.latex-project.org}{LaTeX} installation is available on the used computer.
  	Defaults to \code{TRUE}.
  }
  \item{manual.lookup}{
  	A matrix with handles (same as in \code{\link{build.q.set}}, \code{\link{import.q.concourse}}) as row names, \code{study.language} as columns and arbitrary, \emph{unique} identifying strings in the cells, as also expected in \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
    If \code{study.language} is set to \code{NULL} (default), the first column in the provided matrix will be used.
    Notice that \code{manual.lookup} must always be a matrix, even if it has only one column.

		Defaults to \code{NULL} in which case items are automatically identified by full item hashes, as also detected by \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
  }
  \item{wording.font.size}{
    A character vector of length 1 to set the font size of the full item wording on the cards.
    Defaults to \code{NULL} in which case the default font size 12pt is used.
    Only \href{http://en.wikibooks.org/wiki/LaTeX/Fonts#Sizing_text}{standard LaTeX font sizes} are allowed, from \code{"\\tiny"} to \code{"\\Huge"}.
  }
  \item{file.name}{
    A character vector of length 1 to set the filename \emph{without file extension}.
    Defaults to \code{QCards}.
  }
  \item{babel.language}{
    A character vector of length 1 to set the babel language for appropriate hyphenation, special letters and other international support as provided by the \href{https://www.ctan.org/pkg/babel}{babel LaTeX package}.
    Only available babel options are permissible.
    Defaults to \code{NULL}, in which case babel is never called.
    Changing \code{babel.language} between function calls can occasionally leave inconsistent LaTeX temp files, which may trip up compilation.
    Please re-run the function once again or clean up temp files (in the working directory) in that case.
  }
  \item{show.handles}{
    Logical.
    If \code{TRUE}, meaningful \emph{handles} are used instead of \emph{IDs}, as may be useful for researcher-facing card sets.
    Defaults to \code{FALSE}, in which case the participant-facing \emph{IDs} are used (as per \code{manual.lookup}).
  }
  \item{duplex.double}{
    Logical.
    If \code{TRUE}, \emph{page} is included \emph{twice} in the output to allow duplex printing, where duplex printer driver settings do not allow for such a setting.
    Defaults to \code{FALSE}, in which case each page is included only \emph{once} (recommended), and printer driver settings have to be used for duplex copying.
  }
}
\details{

	Preparing cards with full items and IDs quickly becomes cumbersome if a study is done several times or if items change frequently.
	Participants require well-printed, well-designed cards for their sorting task, ideally on heavier paper.
	Cards should include shorthand, unique identifiers to simplify later data entry.

	This function prepares a properly typeset *.PDF (or *.TEX source), where items are printed on readily-available business card templates, from which individual cards can be easily broken out.

	The function prints the full item wording on the \emph{right} column of any page, and the identifier (ID) on the \emph{left} column.
	If templates are duplex printed with the same page on the front and back, and in proper orientation, the front of each card includes the full wording, and the back its unique identifier (ID).
	For duplex printing, every page has to be printed \emph{twice}, once on the front, once on the back.
	It is recommended to use printer driver duplex settings for this copying, where available.
	Many printer drivers, even if they offer duplex printing, do not allow you to copy the \emph{same} page on front and back.
	Use \code{duplex.double=TRUE} in this case to create output with duplicate pages.

	Identifiers (ID) entered manually or automatically hashed from full wordings are also expected in the import functions \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
  The automatic summary of full item wordings, technically known as \emph{hashing}, proceeds internally by passing the full item wording to the \code{\link[digest]{digest}} function of the package \pkg{digest} (with arguments set to

  \code{algo = crc32, serialize = FALSE}.)

	The function proceeds internally by preparing a dataframe with full item wordings and identifiers (ID), and then invokes a prepared \code{*.RNW} template included with this package, which in turn includes a \pkg{knitr} chunk, which in turn calls \pkg{xtable} to return a neatly layed-out multi-page table.

  If you are not familiar with the terminology of item \emph{handle}, \emph{ID} and \emph{wording} or the file structure expected for import functions, please read the respective sections in the documentation for \link{qmethod-package} first or consider the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.

  If \code{img.dir} is specified for image-based Q studies, the item \emph{file names} are passed on internally as valid LaTeX wrapped in \code{\\includegraphics{}} commands.
  Automatic item identifiers (ID) are based on image file names, not binary image content, and \emph{do not change if the image content changes}.
  Images are currently \emph{not} read into the workspace by R, but only handled by LaTeX.
  Images, though not item wording, is horizontally and vertically centered within cells.
  Images are scaled to fit into cells.
  For details, see the source of this function.
}

\value{
	Writes a PDF or its source TEX to the working directory ready for printout.
}

\note{
	Hashed identification has not been widely tested in Q studies and should be used with great care and only for extra convenience.
	When using hash identification, researchers should be careful to record the precise item wordings at the time of hashing for the printed Q cards, preferably with a version control system.
	Researchers should also record the complete Q sorts of participants in an \emph{unhashed} form, such as a picture of the completed sort in full wordings, in case problems with the hashing arise.

  When \code{output.pdf = TRUE}, the function will sometimes fail with the error message\cr \code{"Running 'texi2dvi' on ... failed"}.
  This is not a bug with the function, but simply indicates that the path to pdflatex is not available in the current R environment.
  To fix this issue, compile the resulting *.TEX manually, use RStudio or try \href{http://www.r-bloggers.com/building-r-packages-missing-path-to-pdflatex/}{this fix}.
  You can also report other suggestions \href{https://github.com/aiorazabala/qmethod/issues/110}{here}.

  This function does \emph{not} automatically scale the font size to fit the given card size.
  Instead, users will have to proceed by trial and error, using a \code{wording.font.size} that works for their longest item.
  The default value should work for most Q items.

	This function currently works for Avery Zweckform C32010 templates, designed in\cr \code{/cardtemplates/AveryZweckformC32010.Rnw} as well as simple 13-cm quadratic cards, two on each A4 paper in \cr \code{/cardtemplates/2x1a4.Rnw}.
	If you would like support for other templates, check out / chip in here: \url{https://github.com/aiorazabala/qmethod/issues/34}.
}

\author{Maximilian Held, Verena Kasztantowicz}

\seealso{
	\code{\link{build.q.set}},
	\code{\link{import.q.feedback}},
	\code{\link{import.q.sorts}},
	\code{\link{import.q.concourse}}
}

\examples{
## Make cards from importexample
data(importexample)
make.cards(importexample$q.set, output.pdf = FALSE)
}
