library(tidyverse)
library(foreign)


ANES_2008 <- rio::import("data/raw/anes2008_2009panelpor.zip", which = "anes2008_2009panel.por")%>%
	select(contains("der08"), #party ID 7 0 - 6 strong dem to strong rep
				 contains("der12"), #primary vote choice
				 contains("der15"), #gen elect turnout 0 not voted, 1 voted. -6/-2 missing/not asked
				 contains("der16"), #for whom did you vote?
				 ends_with("d1"), #will R vote in november 1 - yes, 2 - no, -6/-2 missing
				 ends_with("e2"), # does r like dem party 1 like, 2 dislike, 3 neither -7/-6/-2 missing
				 ends_with("e5"), # does r like or dislike republican party ("same coding as dems
				 contains("invitedate"),
				 contains("begti"),#time questionaire started
				 -contains("xw"),
				 -contains("me2"),
				 -contains("pre2"),
				 )%>%
	glimpse()
