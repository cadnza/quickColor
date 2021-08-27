# Applies xterm color

quickColor <- function(x,fg=NA,bg=NA,bold=FALSE){
	final <- x
	if(!is.na(fg))
		final <- crayon::make_style(clrs$hex[clrs$xterm==fg])(final)
	if(!is.na(bg))
		final <- crayon::make_style(clrs$hex[clrs$xterm==bg],bg=TRUE)(final)
	if(!is.na(bold))
		final <- crayon::bold(final)
	return(final)
}
