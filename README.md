# Capstone: the Legal Implications of Machine Learning Algorithms on the US Mortgage Lending Market

## Study Brief
This repo contains part of coding materials I have used for the Capstone project. Two datasets have been used in the simulation: one is the HMDA dataset, and the another is the CoreLogic dataset. 
- HMDA dataset can be accessed through the link: https://www.consumerfinance.gov/data-research/hmda/historic-data/
- Due to intellectual property constraint, the CoreLogic dataset cannot be shared online

After merging two datasets, I have trained and tested the prediction performance of both logistic and random forest models. I found the random forest model outperformed the logistic model, and found no improvement with protected characteristics included in. The result suggests that a more advanced prediction algorithm can uncover hidden protected characteristics, increases disparity between groups, and raises a challenge to the current anti-discrimination legal rules. I also tested four popular legal suggestions by scholars, and found none of them can effectively solve the current problem. 


## Repository Structure

```
├── modules.py #model library
├── Data and models.ipynb # data processing and model building
├── Simulation_Legal_Critics.R # examine four legal remedies through simulations
├── HMDA # data dictionry for HMDA
├── CL_LLMDA_Data_Dictionary # data dictionary for CoreLogic
```
