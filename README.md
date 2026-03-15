# Delivery Robot Selection using Multi-Criteria Decision Analysis

## Author

Druvitha H K
MSc Student – University of Leeds

## Project Overview

This project evaluates seven autonomous delivery robot prototypes using a Multi-Criteria Decision Analysis (MCDA) approach.

The goal is to recommend the most suitable robots for a delivery trial in Leeds based on two different business strategies.

The analysis applies weighted scoring models to evaluate robot performance across operational and technological criteria.

## Technologies Used

* R Programming
* Data Visualization (ggplot2)
* Multi-Criteria Decision Analysis (MCDA)
* Data Normalization
* Business Decision Analytics

## Dataset

The dataset contains performance metrics for seven robot prototypes across several operational criteria:

* Carrying Capacity
* Battery Size
* Speed
* Mobility
* Aesthetic Design
* Cost per Unit
* Reliability

Each robot is evaluated across these criteria to support strategic decision making.

## Methodology

### 1. Data Understanding

Robot performance metrics and management priorities were analysed to understand key decision factors.

### 2. Data Preparation

All criteria were normalized to a 0–1 scale using Min-Max normalization to allow fair comparison across different measurement scales.

### 3. Multi-Criteria Decision Analysis

A Weighted Sum Model (WSM) was used to calculate performance scores for each robot prototype.

### Business Plan 1 – Operating at Scale

Focus on operational efficiency with criteria weights such as:

* Carrying capacity
* Cost per unit
* Battery size
* Reliability

### Business Plan 2 – Technology Licensing

Focus on intellectual property value using:

* Battery size
* Cost efficiency
* Reliability

### 4. Model Evaluation

Robots were ranked based on weighted scores under each strategic plan.

## Key Results

Business Plan 1 (Operational Efficiency)
Best robot: **Gamma**

Business Plan 2 (Technology Licensing)
Best robot: **Gamma**, followed by **Delta**

The results show that Gamma provides a balanced performance across cost, reliability and battery capacity.

## Business Recommendations

For the Leeds trial, the recommended robots are:

1. **Gamma** – Primary robot for both strategies
2. **Delta** – Secondary robot for technology-focused strategy

Testing both robots provides strategic insights for operational efficiency and intellectual property development.

## Skills Demonstrated

* Decision analytics
* Multi-criteria modelling
* Data normalization
* Data visualization
* Strategic business analysis

## License

MIT License
