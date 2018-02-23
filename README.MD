Original data are from the Human Activity Recognition Using Smartphones Dataset (Version 1.0)
	Source Data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

=======================================================================================================================================

Gathering data
	Original data were downloazed (using R studio) then unzipped using mictosoft windows 10.  Further information on this dataset is available in the README file contained in the downloaded .zip file.
	Averaged testing and training data from this dataset were used (X_train.txt and X_test.txt)
	column labels were obtained from featrues.txt
	subject labels were obtained from subject_train.txt and subject_test.txt
	activity labels were obtained from y_train.txt and y_test.txt.  Descriptive labels were obtained from activity_labels.txt

Processing
	merged training and test sets using the cbind() function
	extracted all data containing means and standard deviation (mean_SD_data.txt)
	
generate descriptive variables
	subject labels:  labels from training and test data (numbers 1-6) were replaced with descriptive labels from activity_labels.txt
	subject labels:  numeric labels indicating subject number were changed by adding "subject" in front of the number

generatye simplified column names
	special characters were removed from column names

Generate summary table of mean and sd data
	averaged the data for each subject over each activity (6 trials for each activity) and placed in summaryMeans.txt
	variables and column names are the same as in the mean_SD_data.txt file

===================================================================
Files

mean_SD_data.txt
	 contains the mean and standard deviation information from the original dataset.

SummaryMeans.txt
	Contains the mean of each column (movement standard deviation and mean) for each subject and activity (6 trials)
	Column names and factor names are the same as for the mean_SD_data table

run_analysis.R
	r script used for the analysis

other files
	from the original dataset (see original readme below)

===================================================================

Factor variables
	Factor columns:
		subjects
			number of the subject performing the activity
				subject1 through subject30

		activity
			name of activity being performed during measurements (six trials per subject)
				WALKING
           			WALKING_UPSTAIRS
				WALKING_DOWNSTAIRS
				SITTING            
				STANDING         
				LAYING

	movement columns:
		contain mean and standard deviation data for each movement from original dataset.
			tBodyAccmeanX               
			tBodyAccmeanY                
			tBodyAccmeanZ                
			tGravityAccmeanX            
			tGravityAccmeanY             
			tGravityAccmeanZ             
			tBodyAccJerkmeanX           
			tBodyAccJerkmeanY            
			tBodyAccJerkmeanZ            
			tBodyGyromeanX              
			tBodyGyromeanY               
			tBodyGyromeanZ               
			tBodyGyroJerkmeanX          
			tBodyGyroJerkmeanY           
			tBodyGyroJerkmeanZ           
			tBodyAccMagmean             
			tGravityAccMagmean           
			tBodyAccJerkMagmean          
			tBodyGyroMagmean            
			tBodyGyroJerkMagmean         
			fBodyAccmeanX                
			fBodyAccmeanY              
			fBodyAccmeanZ                
			fBodyAccmeanFreqX            
			fBodyAccmeanFreqY          
			fBodyAccmeanFreqZ            
			fBodyAccJerkmeanX           
			fBodyAccJerkmeanY          
			fBodyAccJerkmeanZ            
			fBodyAccJerkmeanFreqX        
			fBodyAccJerkmeanFreqY      
			fBodyAccJerkmeanFreqZ
			fBodyGyromeanX
			fBodyGyromeanY              
			fBodyGyromeanZ
			fBodyGyromeanFreqX           
			fBodyGyromeanFreqY          
			fBodyGyromeanFreqZ           
			fBodyAccMagmean             
			fBodyAccMagmeanFreq         
			fBodyBodyAccJerkMagmean      
			fBodyBodyAccJerkMagmeanFreq  
			fBodyBodyGyroMagmean        
			fBodyBodyGyroMagmeanFreq     
			fBodyBodyGyroJerkMagmean     
			fBodyBodyGyroJerkMagmeanFreq
			tBodyAccstdX                 
			tBodyAccstdY                 
			tBodyAccstdZ                
			tGravityAccstdX             
			tGravityAccstdY              
			tGravityAccstdZ             
			tBodyAccJerkstdX            
			tBodyAccJerkstdY             
			tBodyAccJerkstdZ            
			tBodyGyrostdX                
			tBodyGyrostdY                
			tBodyGyrostdZ               
			tBodyGyroJerkstdX            
			tBodyGyroJerkstdY            
			tBodyGyroJerkstdZ           
			tBodyAccMagstd               
			tGravityAccMagstd            
			tBodyAccJerkMagstd          
			tBodyGyroMagstd              
			tBodyGyroJerkMagstd          
			fBodyAccstdX                
			fBodyAccstdY                 
			fBodyAccstdZ                 
			fBodyAccJerkstdX            
			fBodyAccJerkstdY             
			fBodyAccJerkstdZ             
			fBodyGyrostdX               
			fBodyGyrostdY                
			fBodyGyrostdZ                
			fBodyAccMagstd              
			fBodyBodyAccJerkMagstd     
			fBodyBodyGyroMagstd          
			fBodyBodyGyroJerkMagstd  



=========================================================================================================================================
Original ReadMe
=========================================================================================================================================

Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
