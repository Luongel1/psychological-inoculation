# Psychological Inoculation

## Overview

This paper investigates the impact of psychological inoculation on reducing susceptibility to misinformation, focusing on key psychological variables such as analytical thinking, conspiracy belief acceptance, and receptivity to misinformation. Utilizing Bayesian regression models, the study demonstrates that inoculation treatments significantly increase analytical thinking scores (effect size: 0.19, 95% CI [0.15, 0.23]) while decreasing conspiracy belief acceptance (-0.11, 95% CI [-0.15, -0.07]) and receptivity to misinformation (-0.23, 95% CI [-0.27, -0.18]). The findings reveal that the inoculation treatment is most effective among younger age groups (25–34) and individuals with higher educational attainment, while political ideology moderates the treatment’s impact, with stronger effects observed among individuals with liberal leanings. These results underscore the potential of psychological inoculation as a scalable intervention to combat misinformation across diverse populations.

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from FiveThirtyEight.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains details about LLM chat interactions and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

Aspects of the abstract, title, and code such as the simulation script, cleaning script, testing script, and the Quarto paper were written with the help of chatGPT-4o and the entire chat history is available in other/llms/usage.txt.
