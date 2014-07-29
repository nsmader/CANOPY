CANOPY
======

CANOPY (first named for "Chicago Allocation to Neigbhorhood-Oriented Programs for Youth") is a algorithm intended to assist policy makers with complex decision-making problems. CANOPY was designed to assist city planning processes which feature:

* the need to "optimally" distribute some resource (e.g. dollars of public funding, or service centers) across space;
* a large number of candidate recipients of that resource (e.g. sites to be funded, or candidate locations);
* a complex objective (e.g. one that considers many factors, like multiple characteristics of local populations, both in terms of quantity and quality) that defines the notion of "optimality".

CANOPY uses a [simulated annealing](http://en.wikipedia.org/wiki/Simulated_annealing) algorithm adapted to optimization of spatial policy allocations. Given statement of an objective function, a given total resource, and characteristics of the candidate recipients, the algorithm identifies promising candidate allocations that can be offered to human decision makers for review, whether to inform an improved run of the algorithm (perhaps by adjusting the objective function), or for finishing by human logic that cannot be well represented in the algorithm. The advantages of using CANOPY for assisting in planning processes are that it:

1. saves time relative to a human process of deliberating over candidate allocations;
2. requires taking a concrete, transparent position on the objective of a policy;
3. works with more rapid and consistent logic from start to finish than humans can do; and
4. can easily incorporate more complexity in the consideration of many factors than humans can do.
 
## Project Development

This project is currently in beta form, applied to a simple motivating example. In this example, there is a number of seats to be allocated across [Chicago community areas](http://en.wikipedia.org/wiki/Community_areas_in_Chicago), where the value of *n* seats allocated to community *i* equals

<p align="center">
<img src="http://latex.codecogs.com/gif.latex?V_i(n)=\left\{\begin{matrix}
 \sqrt{n} & if\ i\ is\ odd \\ 
 0 & if\ i\ is\ even
\end{matrix} \right.", border="0"/>
</p>

which was chosen because the global optimum is easily deducible as an even distribution of total resources across all odd-numbered community areas (and which is simple to identify visually from mapped runs of the algorithm), and which creates an interesting view of the search path towards optimality given the initialized allocation of uniform distribution across community areas.
