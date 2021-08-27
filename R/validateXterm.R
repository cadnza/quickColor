# Validates xterm

validateXterm <- function()
	if(crayon::num_colors()!=256)
		stop(
			"quickColor only works in xterm-compatible terminal emulators with 256 colors.",
			call.=FALSE
		)
