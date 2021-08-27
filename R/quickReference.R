# Shows a color reference

sp <- function(x,v){
	targetWidth <- max(nchar(c(x,v)))+1
	nSpaces <- max(0,targetWidth-nchar(x))
	final <- paste0(x,strrep(" ",nSpaces))
	return(final)
}

quickReference <- function(){
	sampleHeader <- "Color"
	nSpacesForSample <- nchar(sampleHeader)
	sample <- strrep(" ",nSpacesForSample-1)
	altHeader <- "Alternate"
	header <- quickColor(
		paste0(
			.sp("#",clrs$xterm),
			.sp(sampleHeader,sample),
			.sp("Name",clrs$name),
			.sp(altHeader,clrs$name)
		),
		bold=TRUE
	)
	final <- c(
		header,
		sapply(
			1:nrow(clrs),
			function(i){
				altText <- .sp(ifelse(clrs$system[i],clrs$hex[i],""),c(clrs$hex,altHeader))
				paste0(
					quickColor(.sp(clrs$xterm[i],clrs$xterm),bold=TRUE),
					quickColor(.sp(sample,sample),bg=clrs$xterm[i])," ",
					quickColor(.sp(clrs$name[i],clrs$name),fg=clrs$xterm[i]),
					crayon::make_style(clrs$hex[i])(altText)
				)
			}
		)
	)
	cat(final,sep="\n")
}
