# Barnby etal 2023 D2D3 Modelling

<p>
    <a href="https://twitter.com/intent/follow?screen_name=joebarnby">
        <img src="https://badgen.net/badge/icon/twitter?icon=twitter&label"
            alt="follow on Twitter"></a>
    <a href="https://mastodon.online/@JoeBarnby">
        <img src="https://badgen.net/badge/icon/mastodon?icon=mastodon&label"
            alt="follow on Mastodon"></a>    
    <a>
        <img src="https://badgen.net/badge/Release/:V1.03/red?icon="
            alt="Release"></a>
    <a>
        <img src="https://badgen.net/badge/Checks/Success/green?icon="
            alt="Checks"></a>   
</p>
<p>  
    <a href="https://soccrlab.shinyapps.io/MentalStateInferenceModel/">
        <img src="https://badgen.net/badge/Interactive Model/Click Here/orange?icon="
            alt="Access the Model"></a>    
</p>

Repository for this paper: https://www.biorxiv.org/content/10.1101/2023.05.02.539031v2

Below is the graphical summary and data guide to navigate the repo.

## Graphical Abstract

<img src="GraphicalAbstractD2D3Modelling.png"  width="637" height="502">

## Model 

You can play around with and simulate outcomes from the model included in this paper [here](https://soccrlab.shinyapps.io/MentalStateInferenceModel/)

## Data

- data for Matlab fitting ([DRUG].csv), where DRUG = {HALO, LDOPA, PLAC}
- data to list order of responses from partially fair Dictator's (RandomisedSchedule.csv)
- data for participant responses (TaskData.csv)
- folder of simulated outcomes from model simulations (/SimulatedData/...)

## R

- Analysis script (ModellingAnalysis_GitHub.R)

## Matlab

- fitted real (/HBIFit/...) and simulated parameters (/HBIFit_Sim/...) following HBI processing
- individual subject level model fits for real (/Lap_Subj/...) and simulated (/Lap_Subj_Sim/...) parameters
- laplace fitted models parameters for real (/LaplaceFit/...) and simulated (/LaplaceFit_Sim/...) data
- Model files (/MatlabModels/...)
- Fitting scripts for real (/MasterFileMOBS1/...) and simulated (/MasterFileRECOVERYMOBS1/...) data
