\name{quickColor}
\alias{quickColor}
\alias{quickColorByScale}
\alias{quickReference}
\title{Quick Colors for xterm}
\description{
\code{quickColor} wraps \code{crayon} and adds the ability to reference colors by xterm number, which can be especially useful if you're trying to match a specific color from xterm's palette (though do see the caveat for system colors in Details). And it only colors text in interactive mode, so it won't mess with your scripts.
}
\usage{
quickColor(
	txt,
	fg=NA,
	bg=NA,
	bold=FALSE
)

quickColorByScale(
	txt,
	x,
	lwr,
	upr,
	target="fg",
	fg=NA,
	bg=NA,
	clrLwr=2L,
	clrMid=11L,
	clrUpr=9L
)

quickReference()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{txt}{
  The text to format.
  }
  \item{fg, bg}{
  Static foreground and background color, respectively. These should be xterm numbers (see \code{\link{quickReference}}). Setting either to \code{NA} (the default) forgoes color for that element.
  }
  \item{bold}{
  Whether to bold as well as color.
  }
  \item{x}{
  The working value on the color scale (see Details).
  }
  \item{lwr, upr}{
  The upper and lower bounds of the color scale (see Details).
  }
  \item{target}{
  Whether to apply the color scale to the foreground (\code{"fg"}) (the default) or the background (\code{"bg"}) (see Details). Note that if \code{target="fg"}, then the \code{fg} argument must be \code{NA}. Similarly, \code{target="bg"} necessitates \code{is.na(bg)}.
  }
  \item{clrLwr, clrMid, clrUpr}{
  The three colors of the color scale, representing the lower, middle, and upper bounds, respectively (see Details). All three should be xterm colors (see \code{\link{quickReference}}) except for \code{clrMid}, which is allowed to be \code{NA}. If \code{clrMid} is \code{NA}, the scale has no middle bound. The defaults here are green for the lower bound, yellow for middle, and red for upper.
  }
}
\details{
The \code{quickColor} function applies quick styling using xterm color numbers.

The \code{quickColorByScale} function applies xterm color based on a scale. Perhaps the best parallel is \href{https://support.microsoft.com/en-us/office/use-conditional-formatting-to-highlight-information-fed60dfa-1d3f-4e13-9ecb-f1951ff89d7f}{Excel's conditional formatting} by scale, which colors values depending on where they fall on a scale. With \code{quickColorByScale}, you specify text (\code{txt}), scale boundaries (\code{lwr} and \code{upr}), a numeric value on the scale (\code{x}), whether to dynamically color the foreground or background (\code{target}), a static color for the other element (\code{fg} or \code{bg}), and boundary colors (\code{clrLwr}, \code{clrMid}, and \code{clrUpr}), and \code{quickColorByScale} returns back the text nicely colored.

The middle boundary \code{clrMid} is the color at the \emph{mean} of \code{clrLwr} and \code{clrUpr}; see Examples for how it works. Omitting \code{clrMid} reverts \code{quickColorByScale} to a simple scale between \code{clrLwr} and \code{clrUpr}.

Also note that setting \code{x} to a value outside \code{clrLwr:clrUpr} sets the dynamic color returned to either \code{clrLwr} or \code{clrUpr}, depending on which value \code{x} exceeds.

With \code{quickColor} and \code{quickColorByScale}, you might notice that xterm colors \code{0} to \code{15} don't always behave as expected. These are system-defined colors, and they're treated as static colors in this package. To see their definitions, call \code{quickReference()} and note the \emph{Definition} field.
}
\value{
The \code{quickColor} and \code{quickColorByScale} functions return ANSI-escaped strings that must be handled with either \code{\link[base]{cat}} or a function with similar functionality (see Examples). \code{quickReference} returns nothing.
}
\references{
\href{https://jonasjacek.github.io/colors/}{256 Colors - Cheat Sheet - Xterm, HEX, RGB, HSL}
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# quickColor ----

final <- c()
vals <- c(NA,0:15)
fgVals <- vals
bgVals <- vals
boldVals <- c(FALSE,TRUE)
for(fgv in fgVals)
	for(bgv in bgVals)
		for (bdv in boldVals){
			result <- quickColor(
				"a",
				fg=fgv,
				bg=bgv,
				bold=bdv
			)
			final <- c(final,result)
		}
cat(final,sep="")

# quickColorByScale ----

final <- c()
mn <- 1
mx <- 20
targets <- c("fg","bg")
vals <- c(NA,0:15)
for(tar in targets)
	for(val in vals){
		if(tar=="fg"){
			fgWorking <- NA
			bgWorking <- val
		}else{
			fgWorking <- val
			bgWorking <- NA
		}
		resultRow <- c()
		for(i in mn:mx){
			result <- quickColorByScale(
				"a",
				x=i,
				lwr=mn,
				upr=mx,
				target=tar,
				fg=fgWorking,
				bg=bgWorking
			)
			resultRow <- c(resultRow,result)
		}
		resultRow <- paste(resultRow,collapse="")
		final <- c(final,resultRow)
	}
cat(final,sep="\n")

# quickReference ----

quickReference()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
