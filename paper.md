---
title: 'SLICK: An interactive application for communicating MSE results'
tags:
  - Python
  - astronomy
  - dynamics
  - galactic dynamics
  - milky way
authors:
  - name: Author 1
    orcid: 0000-0000-0000-0000
    equal-contrib: true
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
  - name: Author Without ORCID
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: 2
  - name: Author with no affiliation
    corresponding: true # (This is how to denote the corresponding author)
    affiliation: 3
affiliations:
 - name: Institution Name, Country
   index: 1
 - name: Institution Name, Country
   index: 2
 - name: Independent Researcher, Country
   index: 3
date: 13 August 2017

---

# Summary

# Introduction
Fisheries around the world, both international and domestic, are transitioning to management procedures (MPs) as a more effective and efficient way to secure and maintain sustainable fisheries. Most management procedures include four elements:  1) management objectives to set a vision for the stock and fishery; 2) a data collection plan; 3) a method to assess those data; and 4) a harvest control rule (HCR), the operational part of an MP that sets fishing opportunities based on the other MP components. Management objectives may include reference points, which are benchmarks for the target and danger zones of fishing levels and population size. These and other performance metrics help to quantify the achievement of management objectives. Assessment methods might include full or simplified stock assessments, or more direct measures of stock status such as combinations of fishery-dependent and/or fishery-independent indices of abundance. The former are classified as model-based MPs, whereas the latter are classified as empirical MPs. HCRs are pre-agreed, formulaic approaches to setting fishing opportunities, such as catch or effort limits, based on stock status. For example, an HCR might linearly decrease catch limits between the target and danger population sizes. Once the population size is estimated, the catch limit would be set without any discussion or political negotiation. In this way, MPs lead to more predictable and transparent management. HCRs can also include provisions to limit the change in catch from one management cycle to the next, thereby creating a more stable system, which is beneficial to both the fishing industry and seafood market.

Management strategy evaluation (MSE) is a simulation tool used to develop MPs. Operating models form the core of the MSE, each representing a potential reality across a range of futures for the stock and fishery. The operating models allow for MSE to account for much greater uncertainty than a traditional stock assessment, ranging from unknown illegal fishing levels to natural variability, including due to climate change, when testing MP performance. The goal is to identify an MP that will perform well under all or most of the potential scenarios. MSEs might have tens or even hundreds of operating models, with 100 or more simulations per operating model and numerous performance metrics for evaluating performance, creating extensive results for consideration by scientists, decision-makers, and other stakeholders. 

As a result of the extensive results produced and the newness of the approach, communications about MPs and MSE have been challenging. MSEs are sophisticated, technical models and even some stock assessment scientists may struggle to understand the approach and how to interpret the results. Inconsistent terminology across different management bodies further complicates the process. One of the key benefits of MSE and its multiple performance metrics is the ability to balance tradeoffs across a suite of often competing objectives (e.g., maximizing both catch and population abundance). However, clear dissemination of results and a solid understanding by all players is required to enable this balancing.

To foster clear and consistent presentation of results in a streamlined manner, we developed an interactive, online tool, named Slick. It is an open-access software, coded in R Shiny, that allows users to upload their MSE results directly to the app for sharing with their audiences. There are four dimensions presented in Slick, all of which can be customized to the individual MSEs:  candidate management procedures, operating models, performance metrics, and simulations. Users can enable and disable options for each axis to limit the results to their preferred alternatives.  A suite of twelve plots plus tables are coded into the app to present the results both graphically and numerically to cater to different learning styles. 


