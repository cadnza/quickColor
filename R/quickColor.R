# Applies xterm color

quickColor <- function(txt,fg=NA,bg=NA,bold=FALSE){

	# Apply styles
	final <- txt
	if(!is.na(fg))
		final <- crayon::make_style(clrs$hex[clrs$xterm==fg])(final)
	if(!is.na(bg))
		final <- crayon::make_style(clrs$hex[clrs$xterm==bg],bg=TRUE)(final)
	if(!is.na(bold))
		final <- crayon::bold(final)

	# Return
	return(final)

}
