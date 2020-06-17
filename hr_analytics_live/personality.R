# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# Case Study: Personality Insights (IBM Watson Personality Insights)
# source: https://hranalyticslive.netlify.app/16-personality.html

# make account with IBM
key = "aOWMNztQ_VVlz9fINhc3v67rtnJqcN6JuubQorAvhq"
url = "https://gateway.watsonplatform.net/personality-insights/api/v3/profile?version=2017-10-13"
uname="a4a4ea65-e8e7-492c-a95e-128f10fc5f"
pword="LuFm4BELs"

# library
library(httr)
library(janeaustenr)

# POST a request to IBM API
cr=POST(url, 
        authenticate(uname, pword), 
        content_type("text/plane;charset=utf-8"), 
        accept_json(), 
        body = paste(janeaustenr::emma, collapse = " "))


# no access - 403
status_code(cr)
