Progress report 1
================
Thea Rossman
2020-02-03

  - [Summary of Progress so Far](#summary-of-progress-so-far)
  - [More detailed explanation of the
    data](#more-detailed-explanation-of-the-data)
  - [Issues](#issues)
  - [Next steps](#next-steps)

## Summary of Progress so Far

My project proposal was to recreate the USAFacts analysis of wealth
distribution in the United States, over time, since 1990. The data is
downloaded from the [Federal Reserve Distributional Financial
Accounts](https://www.federalreserve.gov/releases/z1/dataviz/dfa/). The
original analysis is
[here](https://usafacts.org/reports/facts-in-focus/how-wealth-distributed-in-america).

Since then, I have downloaded the datasets from the Fed into my data-raw
folder, and written scripts to transform them into a convenient format
(reformatting dates and following tidyverse conventions).

The available data are quarterly estimates of the distribution of U.S.
household wealth, beginning with the third quarter of 1989 and through
the most recent quarter. The measure includes both assets, liabilities,
and asset/liability type. There are multiple data files in the “raw
data” folder, each of which breaks down the above variables by a
different category (age, education, generation, income percentile, net
wealth percentile, race) and by a different method (total owned or share
of total). (See “more detailed explanation of the data”.)

As discussed in the meeting about my second project proposal, in
addition to exploring, understanding, and wrangling the data available,
I have performed initial data analysis to recreate the portion of the
USAFacts analysis that examines household wealth by income category. The
graphs are in eda mode, and are not particularly pretty right now, but
it’s easy to see that the distributions match the USAFacts analysis.
After doing this and figuring out the kinks, I feel confident that I can
relatively quickly recreate the remaining graphs.

I struggled to put pictures in here; the visualizaitons can be found in
my [eda
folder](https://github.com/dcl-2020-01/thea/tree/master/c01-own/eda),
and they can be easily compared against both the
[article](https://usafacts.org/reports/facts-in-focus/how-wealth-distributed-in-america)
and copies of the images shared in my data folder.

## More detailed explanation of the data

In the data tables, each row is an estimate of either the total (summed,
for .csv files labeled “levels”) or share (pct of total, for .csv files
labeled “shares”) net wealth of a particular group during a given
quarter of a given year. This is broken down as follows:

  - Net worth: Total assets less total liabilites

Assets:

  - Assets: Total nonfinancial and financial assets

Columns for total value of assets by category:

  - Real estate, owner occupied, at market value
  - Consumer durables, current cost basis (automobiles, trucks/motor
    vehicles, furniture, etc.)
  - Corporate equities and mutual fund shares, excluding equities and
    mutual fund shares owned through DC pensions (counted under pension
    entitlents)
  - Pension entitlements
  - Private Business: Proprietors’ equity in noncorporate business
    (Includes non-publicly traded businesses and real estate owned as
    landlords, renting to others)
  - Other Assets not accounted for in above categories

Liabilities:

  - Total liabilities held by the group

Columns for total (negative) value of liabilities (debts) by category:

  - Home mortgages
  - Consumer credit: includes student loan and vehicle loan balances, as
    well as other consumer loans
  - Other liabilities not falling into the above categories.

## Issues

  - The USAFacts graphs are adjusted to 2019 dollars. I have not made
    these adjustments.

  - Challenge, or exciting thing: Just regarding next steps. I feel
    confident in being able to both recreate the USAFacts analysis and
    take a similar analysis further (see “next steps”). I wonder, are
    there are other datasets that I should combine this with? Or, are
    there ways that I can merge the datasets in this group? Or, are
    there possible analyses, within these data or otherwise, that I’m
    missing?

## Next steps

  - Finish recreating the USAFacts visualizations and analyses,
    including analysis of distribution by wealth.
  - Make these graphs look presentation-quality\! Experiment with
    tweaking their visualizations.
  - Perform similar analysis based on racial categories and wealth
    shares. In particular, I’m interested in how different racial groups
    were impacted by the 2008 financial crisis.
  - A bit more EDA-ing to figure out if there’s anything else that
    sticks out to me.
  - A write-up of these analyses based on additional research; context
    and narrative for understanding data and patterns.
  - See issues above; depending on scope and the story I choose to
    narrate in this project, I may seek out additional data.
