Progress report 2
================
Thea Rossman
2020-02-20

  - [Progress Report No. 2](#progress-report-no.-2)
  - [Challenge for the team???](#challenge-for-the-team)

## Progress Report No. 2

In the last progress report, I came in having reproduced a USAFacts
analysis of a simplistic dataset from the Federal Reserve describing the
concentration and distribution of wealth over time, including breaking
down asset and debt ownership by income percentile, wealth percentile,
race, and asset or liability category. Bill correctly pointed out that
there was not much else to dig into in this dataset, and that I should
look elsewhere for more data.

I turned my intentions toward the geographic distribution of wealth in
the United States. And the interesting thing is that this is REALLY HARD
TO ESTIMATE. This
[article](https://www.census.gov/content/dam/Census/library/working-papers/2017/demo/FY2016-129.pdf)
from the census tries to approximate wealth based on the ACS and SIPP.

(Side note: The metrics that I really care about are “wealth, land,
power, and economic opportunity” as a concept that measures multiple
dimensions of what it means to have agency and accumulation in this
historical moment… but that may be beyond the scope of this project?)

My intention was to recreate one analysis, but I fell down a rabbit
hole.

I started with a Washington Post analysis using ACS data of zipcodes, in
which they ranked each [zip
code](https://www.washingtonpost.com/sf/local/2013/11/09/washington-a-world-apart/?utm_term=.b0e79aef2e1b)
according to a metric of percentage of population with a college degree
and median income.

I can’t figure out how to directly use their data, since it’s organized
by zip code, but I can create a similar map divided by county, using
more recent ACS data. This, in itself, would make an interesting
challenge, I think.

Some notes on this:

It turns out that the data and analysis came from a book by Charles
Murray called *Coming Apart: The State of White America*. Charles Murray
is famous for using data and statistics in the service of white
supremacy and eugenics; as someone who has done some anti-fascist work,
I see his work get cited by white nationalists all the time. This book
makes a thinly veiled case for white racial solidarity, *using the same
statistics and measures that the Washington Post used to make a point
about the concentration of opportunity.* This is so interesting\!\!\!

For more on this book, see
[here](https://www.latimes.com/entertainment/la-xpm-2012-feb-12-la-ca-charles-murray-20120212-story.html),
[here](https://www.carolinajournal.com/opinion-article/murrays-coming-apart-offers-explanations-for-social-decline/),
[here](https://www.theatlantic.com/magazine/archive/2016/09/the-original-underclass/492731/),
[here](https://www.splcenter.org/fighting-hate/extremist-files/individual/charles-murray),
[here](https://www.vox.com/the-big-idea/2017/3/28/15078400/scientific-racism-murray-alt-right-black-muslim-culture-trump).
This also echoes of the popular conversation surrounding *Hillbilly
Elegy*… see, e.g.,
[here](https://workingclassstudiesjournal.files.wordpress.com/2018/12/jwcs-vol-3-issue-2-dec-2018-reed-1-1.pdf),
and
[here](https://www.thestayproject.net/blog-stay-in-the-news/stays-full-statement-re-jd-vance-at-the-2018-appalachian-studies-conference).

Things that the WaPo analysis misses, I think:

  - It doesn’t account for wealth. So, some follow-up questions: What
    does this show, and what doesn’t it show? Are there ways to
    approximate wealth?
  - It doesn’t account for the vast segregation that can occur within a
    zip code. What are the implications of this? What is
    under-represented? What are measures that others have used for this?

I would like to somehow merge the income and educaiton distribution with
other analyses.

Specifically, I’d like to use this as an opportunity to get really
familiar with (1) the ACS data, and (2) NCES (national center for
education statistics) data.

ACS: I looked through all variables in the ACS data to think through
what could add nuance to what types of wealth people have access to.
Especially for the wealth disproportionately held by the 1% (outlined in
my first progress report), there’s very little transparency or points of
entry for figuring out this distribution. But, some proxies include:
owner-occupied homes, owning real estate not occupied, mortgage, housing
cost as a percentage of income, and type of health insurance. So, I’m
interested in looking at these and seeing if they match up with the
data.

NCES: The NCES (the Stanford Education Data Archive could also be drawn
on here for measures of school segregation) have statistics about
enrollment and gifted programs in private and public schools. Some of
these could be used as proxies for thinking about (1) segregation that
occurs within a county vs. between FIPS codes, and (2) wealth and
community control (i.e., certain types of private school enrollment).

Another concept that I was following was reading some studies of white
flight in
[education](https://www.researchgate.net/publication/232833305_White_Flight_in_the_Context_of_Education_Evidence_from_South_Carolina),
which uses the NCES statistics, and in middle-class
[neighborhoods](https://www.sciencedirect.com/science/article/pii/S0049089X17305422).
There’s a version of this analysis that takes a closer look at ACS data
over time that I think would be SUPER INTERESTING.

## Challenge for the team???

  - Recreate something like the Washington Post analysis; this should be
    fairly simple.
  - Then, do a dive into the history and present of racist statistics.
    Read some critical articles that develop an analysis of Charles
    Murray’s work and how it uses data to paint a picture that is,
    perhaps more subtly than *The Bell Curve*, ultimately in the service
    of white racial supremacy. Think through the history of how data are
    used and abused to justify ideological projects. The history of
    statistics is rooted in eugenics; I think that a reckoning of this
    could make for an interesting challenge.
