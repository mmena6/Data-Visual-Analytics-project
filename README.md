DESCRIPTION 

In this zip file, inside the CODE folder we can find 3 folders:
	
 Data: with all the original csv files, as well as the code in R to join them, we have uploaded the final datasets as well, and to pdf files with the explanations for the datasets for 2019 and 2023, which are further treated in the model code, since they are the ones we will use for our model predictions in the visualization
	
 Models: with the jupyter notebooks with our final versions of the RF and NN models. In those files we further modify the datasets to use them to train our models accordingly. There is also the html version of the notebooks.
	
 Visualizations: with the Tableau workbook containing the visualizations. The Tableau workbook contains 3 Tableau dashboards which can be viewed by selecting them using the bottom tabs. 

INSTALLATION 

For Data Cleaning, you can install R and R studio to run our code to clean the dataset and create the aggregated cleaned one for the visualizations. However, clean data is also provided. To run the models you need to have python (works on version 3.7) and jupyter notebook, and install the packages that are imported at the beginning of the notebook (numpy, pandas, matplotlib.pyplot, sklearn, TabPy_client). This can be done using pip. Our visualizations are created by using the desktop version of Tableau (works on version 2023.2.1) and TabPy (version 2.7.0) to connect to our jupyter notebook. TabPy can be installed with the pip command “pip install TabPy”. Currently, both TabPy and jupyter notebook need to be running on the local machine to interface. The jupyter notebook has code to connect to TabPy through the TabPy_client import and establish a connection commonly through http://localhost:9004/. In Tableau there is the option to connect python and http://localhost:9004/ through help>settings and performance> manage analytics extensions connections> python.


EXECUTION

Once you have installed all the previous software needed, you can run the DataCleaning.R code to get the final_dataset_14_23.csv file, which we use in the Tableau visualizations, though our used clean datasets are already provided. To begin the visualization, use the command prompt to launch TabPy with “tabpy” command and jupyter notebook with “jupyter notebook” command. You will need Python 3.8 and the latest version of pip for compatibility. Then, with the files analytic_data2019.csv and analytic_data2023.csv, you can run the provided jupyter notebooks to train the models and automatically establish a connection to the TabPy through local host. Note that only the random forest will be used for Tableau. All the transformations to the dataset are now ready. Next, launch the desktop version of Tableau and under the help menu option click help>settings and performance> manage analytics extensions connections> python and enter the local host port information (9004). Lastly, open the provided Tableau workbook and the visualization and prediction should be ready to use. 
