# poly_dyn
Deffuant model for studying democracy perception in the European population

Project title: Democracy perception analysis in the European population: an empirical and computational study
Project owners: Luca Marconi, Federico Cecconi

Project aim: adapting and modifying the Deffuant model for studying democracy perception in the European population by means of empirical statistical data collected by the ESS survey.

Project description:

Topics
1. Social systems are one of the most common kinds of complex systems. 
2. Socio-cultural dynamics in social systems involve heterogeneous interactions and lead to different emerging behaviors. 
3. Perception and opinions related to politics are highly relevant to predict the evolution of political systems and society.

Problems 
1. Finding quantifiable data collections through official EU survey to have access to the broadest range of data to understand a phenomenon with relevant social impact and dimension. 
2. Extracting significant distributions, indexes and measures so as to generate a complete statistical description of the social phenomenon. 
3. Simulating the evolution of the perception dynamics by means of agent-based models.

Objectives 
1. Developing a statistical model describing democracy perception in the European population, based on real data from EU citizens. 
2. Elicitating key emerging behaviors in the opinion structure, related to aspects like trust or democracy instruments. 
3. Find an opinion dynamics model leading from descriptive statistics to replicable and realistic predictions of the democracy perception evolution.

Research questions
1. How much do people think that political representatives reflect the will of the people?
2. How much do people think that political representatives have the skills to manage and decide on the issues they are people spokesmen for?
3. How much do people think that the instruments of democracy brought significant advances in research and application of the will of the people?

Model structure
1. Data collected by the statistical survey
2. Initial opinion distribution from empiric behaviors
3. Opinion dynamics modelling with the Deffuant model
4. Model aim: understanding system’s dynamics, starting by initial empirical conditions

The Deffuant model
1. Opinion dynamics model and theory
2. Parameters: tolerance and influence capacity
3. Imitation processes by pairs of agents

Agents’ properties

Opinions:                                      
1. Openness             
2. InstTrust                    
3. DemTrust   

Segmentations:
1. Gender
2. Age class
3. Country area

Tolerance and influence capacity
1. weighted for each agent
2. depending on the agents’ properties
3. influenced by specific conditions on the agents

Opinions influence network
1. defined at the beginning of the simulation
2. taking in consideration the homophily among the agents’ segmentations
3. also topological rules are selected and applied in its formation

Deffuant’s dynamics
1. extension of the traditional model and dynamics
2. opinions affections cover all the possible range of combinations
3. single dynamics and cross dynamics are both considered and applied

