# Applies xterm color

quickColor <- function(
	txt,
	fg=NA,
	bg=NA,
	bold=FALSE
){

	# Validate colors ----
	msgColor <- "Please supply valid xterm color numbers."
	for(clrInst in c(fg,bg))
		if(!clrInst%in%c(NA,clrs$xterm))
			stop(msgColor)

	# Return unmodified text if not interactive ----
	if(!interactive())
		return(txt)

	# Apply styles ----
	final <- txt
	if(!is.na(fg))
		final <- crayon::make_style(clrs$hex[clrs$xterm==fg],colors=256)(final)
	if(!is.na(bg))
		final <- crayon::make_style(clrs$hex[clrs$xterm==bg],bg=TRUE,colors=256)(final)
	if(bold)
		final <- crayon::bold(final)

	# Return ----
	return(final)

}
