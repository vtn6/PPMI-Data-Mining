PPMI-Data-Mining
================
Authors: Vincent Nguyen, Vania Sung, Jason Wang

Data Mining Project on the PPMI Dataset (Parkinson's Progression Initiative)

Used cluster analysis, PCA, and neuralnets, to achieve the following outcomes

1.  Created a classifier that could identify PD patients from healthy controls (HC) with 92.5% accuracy from DatSCAN imaging, and 99.3% accuracy if we include a few other tests. This could be used to quickly and accurately diagnose PD patients, which in turn may reduce costs related to diagnosis.
2.	Found that there may possibly be 5 different kinds of sub-populations among PD patients with different underlying biological processes. This could spur new directions in PD therapy research that targets sub-populations of PD patients.
3.	Could predict the progression of a PD patient’s motor function by a year based on his current state, which could be used to provide patients peace of mind about the future of their condition.	

Introduction
Parkinson’s Disease (PD) affects over 1 million Americans, with annual costs estimated at $15 billion (Johnson, Diener, Kaltenboeck, Birnbaum & Siderowf, 2013). The disease is predominantly marked by motor (e.g., tremors or rigidity) and non-motor symptoms (e.g., sleep disorders or compromised sense of smell). Disease modifying therapeutics that target the underlying disease process are still unavailable. PD biomarkers could accelerate PD therapeutic development in several ways, but this exploration examines in particular how biomarkers can be used to:

1.	Improve patient selection for clinical trials (Neural Nets + Clustering)
2.	Monitor and predict disease progression (PCA + Neural Nets)

Our report (ORIE4740-FinalProjectReport.docx) outlines how we employed data mining techniques to identify a number potential biomarkers, and demonstrate their effectiveness in addressing the two requirements above. In particular we:
1.	Created a classifier that could identify PD patients from healthy controls (HC) with 92.5% accuracy from DatSCAN imaging.
2.	Found that there might be 5 different kinds of sub-populations among PD patients.
3.	Could predict the progression of a PD patient’s motor function by a year based on his current state (how accurately we were able to do this is discussed in the “Predicting PD Progression by One Year” section).
Dataset
The Parkinson’s Progression Markers Initiative (PPMI) is a multi-center study that collects clinical and imaging data, and biological samples for use by scientists. The dataset contains information on 668 patients (216 HC -and 452 PD) and has been ongoing since 2010.

There are 4 main groups of dimensions in our dataset (four main groups of columns): 
1.	Motor: information that deals with a subject’s motor capacities, such as the rigidity of their extremities, their resting tremor amplitude, and even their extent of the slurs in their speech. 
2.	Non-Motor: examples include a subject’s sense of smell, the quality of their REM sleep, and their impulsive behaviors (sex, drugs, alcohol, shopping etc.)
3.	Cerebrospinal Fluid (CSF): A lumbar puncture a.k.a “spinal tap” allows us to obtain a sample of the CSF for each subject. This in turns provides us with the protein make-up of the CSF.
4.	Imaging data: data on the neuronal density in the striatum was available for each patient. This is the region of the brain that is thought to be responsible for small voluntary movements. In particular, the dataset reports the Striatal Binding Ratios (SBR) for each region in the striatum. The SBR is a measure of the density of dopaminergic neurons in each of striatal region.
