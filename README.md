# Quick Color for xterm

![](https://img.shields.io/github/v/release/cadnza/quickColor) ![](https://img.shields.io/github/r-package/v/cadnza/quickColor)

Quick and convenient coloring for xterm-compatible terminals in R. :art:

## Installation

R console:

```
install.packages("devtools")
devtools::install_github("cadnza/quickColor")
```

## Use

R console:

```
?quickColor::quickColor
```

## Examples

Here we go:

```
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
```
