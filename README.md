# Monetary Unification Theory Thingy: Mutt

An attempt by [dpp](https://twitter.com/dpp) to unify
a bunch of monetary theory.

## Modern Monetary Theory (MMT)

[MMT](https://stephaniekelton.com/) is a relatively new theory that takes a
"balance sheet" approach to money and separates out currency issuers (e.g.,
the US, UK, Japanese governments) from currency users.

The basic idea is that currency issuers can and must issue currency in
excess of tax receipts to ensure there's enough money in the economy. See:

<img src='images/flows.png'>

So, if the federal government runs a surplus, there will be economic contractions.
Thus, it's good policy to run deficits. The gating issue for deficits is inflation.
And the general theory is that as long as there's unemployment, the government needs
to run larger deficits to pump money into the economy.

## Simulating the Balance Sheet

The code in the `rust` directory simulates financial transactions
based on MMT's "Balance Sheet" approach.

Currently, the code will run transactions between two parties and yield the
balance sheet for all the parties.

### Code Enhancements

Over time, I am planning to make the following enhancements:

* Supporting time -- Labor and Materials gets replenished (e.g. 40 hours per week of labor)
* Support for constraints and friction -- How fast can a transaction happen? What happens when 
  the speed or cost (friction) of a transaction changes? This will be useful in seeing
  how technology changes in the 90s for faster/more reliable clearing ("I deposit a check,
  when are the funds actually available to me?")
* Supporting "marketplace" -- where pricing changes based on supply/demand. Between time, constraints,
  and a marketplace for pricing scarce resources, we should be able to simulate MMT's inflation.
* Increasing the number of actors -- rather than having "Labor" as a block, how about 100,000 labor participants?
* Support for nested transactions -- when I lease a car, there are many counter-parties to the transaction (the leasing company, the auto dealer, the manufacturer, etc.) 
* Scoring assets for externalities -- assets produce externalities. With a government balance sheet, it
  will be possible to score assets like roads or laws based on the externalities (positive and negative)
  that the assets generate.
* Locale -- where does a particular transaction take place? If actors are physically proximate, 
  how does that impact transactions? Why do Wall St, Silicon Valley, and Hollywood rely of physical proximity?


### Does simulation reflect reality

With a complex enough simulation system, it should be possible to feed in a decade of raw
data from the Fed and other entities and see the simulation yield the same results as the
real world.

### What's different about simulating stuff now?

The laptop I'm doing my coding on has 40GB of RAM and 8 CPU cores. It's possible to simulate
10,000,000+ actors where an actor may be a virtual person, company, or governmental entity.

It's entirely possible to build a small cluster of computers to simulate every single actor
in the US economy.

This allows for something like [Finite Element Analysis](https://en.wikipedia.org/wiki/Finite_element_method)
but for economics.

When I was studying economics as an undergrad in the 1980s, computers were just becoming powerful enough
to run complex [Econometric](https://en.wikipedia.org/wiki/Econometrics) equations. But these were statistical
models... and those models hid a lot of the underlying interactions.

We are at a point in computing where economic simulations can be far more granular.

## Mutt vs. MMT

So... why is Mutt different from MMT?

As far as I've been able to determine, MMT takes a macro view of dollar flows. It does not
look at time nor does it look at the externalities related to assets.

Mutt seeks to look at cash flow over time and across economic groups (based on assets and income).
Mutt further looks at externality scoring of assets (e.g., a college education). Based on cash flow,
balance sheets, and asset scoring, it should be possible to figure out what economic policies
make sense. For example, "is it better for society for government to pay for college educations
or for students to take out loans to go to college?"

Further, Mutt explicitly seeks to model transactional friction/costs, time, and scarcity.

Further, but stratifying economic actors by income and assets, it should be possible to see
the impact of different policies as targeted to specific economic buckets. For example,
if there were automatic stabilizers that ensured the lowest economic decile had at least 3%
of total income and 2% of total assets, how would that impact things.

It should also be possible to model 1970s type [Stagflation](https://en.wikipedia.org/wiki/Stagflation)
based on policies that asymmetrically impact the value of asset classes. It should also be
possible to model the current increasing housing, education, and medical costs while the rest of
common consumption costs are decreasing.

As a broader goal, Mutt seeks to create live (executable) models for exploring policy choices.
This should allow an understanding of what would happen if minimum wages is increased, single
payer healthcare was adopted, etc.

Oh... one more thing... most of economics was developed in and around supporting the lower
levels of [Maslov's Hierarchy](https://en.wikipedia.org/wiki/Maslow%27s_hierarchy_of_needs).
However, the percentage of the economy needed to support basic needs is low and continues to decrease.

As many (most?) humans have biological needs taken care of, what kind of value can we derive from
creativity? Is it possible to model non-monetary transactions? Is it possible to score
the creation of art?

How will economics deal with decreasing amounts of transactions related to physical goods
and an increasing number of transactions related to virtual goods (streaming media, online games, etc.)?

## Participating

So far, this is mostly a dpp gig. If you want to participate, please open a pull request
or create an issue and dpp will respond.

If this turns into something beyond some code-slinging, we can figure out the right kind of forums
to continue the conversation.

## License

The contents of this repository is licensed under an MIT license.