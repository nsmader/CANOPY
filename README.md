CANOPY
======

## Overview

CANOPY (first named for "Computational Algorithm for Neighborhood-Optimized Programs for Youth") is a algorithm intended to assist policy makers with complex decision-making problems. CANOPY was designed to assist city planning processes which feature:

* the need to "optimally" distribute some resource (e.g. dollars of public funding, or service centers) across space;
* a large number of candidate recipients of that resource (e.g. sites to be funded, or candidate locations);
* a complex objective (e.g. one that considers many factors, like multiple characteristics of local populations, both in terms of quantity and quality) that defines the notion of "optimality".

This repository contains documentation of the technical methods that can be used to generate solutions to these complex problems, as well as code--currently still in development--that demonstrates implementations of these methods with realistic--but mock--data.

A presentation with technical details underlying CANOPY can be downloaded [here](https://github.com/nsmader/CANOPY/raw/master/write-ups/presentation/CANOPY%20Project%20-%20Overview%20Presentation.pptx).

## The Problem

When planning locations and offerings across regions and neighborhoods, major for‐profit retailers maximize profits and marketshare with more than rules of thumb. Their analytics incorporate knowledge about where people live, which are likely customers, and what their tastes are. By contrast, local governments and foundations may well be aware of providers and populations within a given human services domain like Head Start, youth anti‐violence programs, or job training, but lack information about how repositioning existing or new resources can more effectively reach larger, and higher priority populations. Without this information, planners are at risk of ineffective use of resources, overloading resources in some areas, overlooking key populations in others, and generally mismatching type of services to needs of local populations. These are deeper issues than maximizing revenue or brand; child development, public safety, and economic opportunity depend on efficient and effective allocation of these scarce resources.

## Analytical Approach

A comprehensive solution that is practically useful to city‐scale resource planners—which may include both city planners at public agencies, public procurement officials, as well as large grant makers—requires a sophisticated understanding of decision‐making by diverse populations targeted by a given human service; a straightforward, concrete means to predict how well a new configuration of resources would meet their goals; and a tool to help identify what types of configurations obtain the best results. CANOPY meets these needs by integrating three interrelated human and technical domains:

### Crystallization of planning objectives and operating constraints

Both planners and researchers meet regularly to concretely articulate considerations for service provision, including:

* **Goals** - such as the relative value for reaching--and differentially prioritizing--different populations, areas of the city, and engaging different types of services and providers; and
* **Constraints** - reflecting both practical considerations such as the planner's overall budget, maximum enrollment capacity of each provider, and political ones such as identifying proposed resource allocations that do not depart too drastically from the status quo in any given year.

### Analysis of service uptake

An analysis of demand for services identifies which families are likely to take up services, and what choices they would be likely to make if they did. Well‐developed econometric methods analyzing "discrete choice" outcomes make use of historical service enrollment patterns to identify how the probability of service uptake is related to all available data on individual/household circumstances, characteristics of service providers, and features of choice context (e.g. neighborhood safety, transportation networks, etc) across city neighborhoods.

### Optimization algorithms

Optimization algorithms provide recommendations for resource allocations that best meet the planner's goals and constraints, given an understanding of how their population of interest would likely take up those services. This involves a range of methods from the field of operations research, which make use of high performance computing implementations that are customized to the planner's specific decision-making structure, which may include decisions of where--and what type of--new programs should be developed, the amount of resources to position at each existing providers, or enhancements to existing offerings to enhance their draw and/or accessibility.

## Anticipated Outcomes

CANOPY's solutions are delivered with an interactive dashboard that allows planners to compare allocation recommendations and anticipated results based on various goal scenarios. Because every technical solution related to social policy requires human judgment, this dashboard is designed to provide a richly‐informed starting point, from which planners can add more nuanced considerations as they make final deliberations.

As a tool for decision-making, CANOPY expects to reduce slack in use of resources, and improve the ability to reach key constituent populations, even with no more than existing resources. In particular, CANOPY will most likely improve access for "donut hole" populations who can benefit from services, but who get overlooked in favor of disproportionate resource allocation to the most immediately recognized geographies of need.
