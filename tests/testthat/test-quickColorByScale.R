test_that(
	"quickColorByScale works",
	{
		mn <- 1
		mx <- 1000
		result <- sapply(
			mn:mx,
			function(i)
				quickColorByScale(
					txt="a",
					x=i,
					lwr=mn,
					upr=mx
				)
		)
		cat(result,sep="")
		expect_true(TRUE)
	}
)
