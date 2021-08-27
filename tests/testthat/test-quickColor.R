test_that(
	"quickColor works",
	{
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
					cat(result)
				}
		expect_true(TRUE)
	}
)
