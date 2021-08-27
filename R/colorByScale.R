# Write function
colorByScale <- function(
	txt,
	x,
	lwr,
	upr,
	clrLwr=2L,
	clrMid=11L,
	clrUpr=9L
){

	# Set function to get component value based on arguments ----
	getCval <- function(componentLetter){
		if(x<=lwr){
			final <- clrs[clrs$xterm==clrLwr,componentLetter]
			return(final)
		}
		if(x>=upr){
			final <- clrs[clrs$xterm==clrUpr,componentLetter]
			return(final)
		}
		midpoint <- ((upr-lwr)/2)+lwr
		limsComp <- c()
		if(is.na(clrMid)){
			limsComp[1] <- clrs[clrs$xterm==clrLwr,componentLetter]
			limsComp[2] <- clrs[clrs$xterm==clrUpr,componentLetter]
			lwrAdj <- lwr
			uprAdj <- upr
		}else{
			if(x>midpoint){
				limsComp[1] <- clrs[clrs$xterm==clrMid,componentLetter]
				limsComp[2] <- clrs[clrs$xterm==clrUpr,componentLetter]
				lwrAdj <- lwr+(upr-lwr)/2
				uprAdj <- upr
			}else{
				limsComp[1] <- clrs[clrs$xterm==clrLwr,componentLetter]
				limsComp[2] <- clrs[clrs$xterm==clrMid,componentLetter]
				lwrAdj <- lwr
				uprAdj <- upr-(upr-lwr)/2
			}
		}
		final <- round(
			limsComp[1]+(limsComp[2]-limsComp[1])*((x-lwrAdj)/(uprAdj-lwrAdj))
		)
		return(final)
	}

	# Get component values ----
	finalRGB <- list()
	finalRGB$R <- getCval("R")
	finalRGB$G <- getCval("G")
	finalRGB$B <- getCval("B")

	# Convert to hex ----
	finalHex <- paste(
		c(
			"#",
			sapply(
				finalRGB,
				function(x){
					converted <- as.character(as.hexmode(x))
					if(nchar(converted)==1)
						converted <- paste0("0",converted)
					return(converted)
				}
			)
		),
		collapse=""
	)

	# Color result ----
	final <- crayon::make_style(finalHex)(txt)

	# Return ----
	return(final)

}
