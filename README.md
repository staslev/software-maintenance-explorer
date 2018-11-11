### Intro

This repo is the source code for https://soft-evo.shinyapps.io/maintenance-activities/.

The software maintenance explorer tool is based on the paper:

> Levin, Stanislav, and Amiram Yehudai. "Boosting automatic commit classification into maintenance activities by utilizing source code changes." In Proceedings of the 13th International Conference on Predictive Models and Data Analytics in Software Engineering, pp. 97-106. ACM, 2017.

The software maintenance activity explorer tool is aimed to provide an intuitive visualization of 
software maintenance activities over time. It may be useful to project and team managers 
who seek to recognize inefficiencies and monitor the health of a software project and its 
corresponding source code repository. The Software Maintenance Activity Visualizer was built 
with Few's and Cleveland's principles in mind, 
which advocate for encoding data using visual cues such as variation in size, shape, color, etc'. 
We chose stacked bar diagrams to visualize data since they allow for an easy comparison both between 
maintenance activities within a given time frame (e.g., what maintenance activity dominated a given time frame), 
and between different time frames (e.g., which of the time frames had more maintenance of a given type).
In addition, the bar diagrams allow users to quickly detect anomalies such as peaks and deeps in one maintenance activity or 
another compared to past periods.

### Project Activity Visualization

The project activity visualization allows users to examine the volumes of the different maintenance activities over time, 
and can be sliced and diced according to a specified date range and an activity period 
(e.g., from date x until date y, in time frames of 28 days). 
The stacked bar plot allows for an easy comparison between the maintenance activity types, as well as trend detection.

### Developer Activity Visualization

The developer activity visualization is a segmentation of the data by a specific developer. 
Users can examine the data for a specific developer, adjusting the period of interest and date range. 
Developers identity can be determined by their name, email or both, a feature that can be useful 
when developers perform commits using different emails, e.g., when working on an open source project 
from both their private account and their cooperate account.

### Publicly Accessible Data

The software maintenance activity exlorer's about page provides an option to explore the data in-line, 
or download it in a CSV format for an offline analysis.
