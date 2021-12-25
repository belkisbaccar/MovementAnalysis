# MovementAnalysis

### To test the app: https://3w3uyl-belkis-baccar.shinyapps.io/r_project/

## Main goal: 
Create a shiny interface allowing to :
*  load a file produced with physics suite
*  visualize it (choice of sensors …)
*  process it (see details)
*  visualize the results of the processing
*  Export the results of the processing function as a csv files with two additional colums to store labels (X,O,Noise) and group name (Last names of the group members separated with _)

## Data:
Install the android/ iOS physics toolbox suite app on your smartphone.  
Use the multiple recording function to record the sensors (Force-g, Linear Accelerometer, Gyroscope) and create 3 files each containing:
*  20 X-movements with 2s pause between each movement
*  20 0-movements with 2s pause between each movement
*  2 min of recording of your daily movements (phone in your pocket or bag for example)
  
## Details on the processing
  Create a function extract_feature_long(data,threeshold_mean,threeshold_sd,threeshold_t) that:
*    compute the norm of the acceleration in three dimension with the gravity removed gN=|gFx2+gFy2+gFz2−0.8|
*    compute the left rolling average gNm of gN over 10 measurements (see the RcppRoll package)
*    compute the left rolling standard deviation gNsd of gN over 10 measurements
*    filter the data too keep the points where gNm>threeshold_mean and gNsd>threeshold_sd
*    compute dti=timei−timei−1
*    create an id called pid for each group g of points where dti<threeshold_time ,∀i∈g
*    add back the removed point with a pid set to 0
  

  Create a function extract_feature_large(data,threeshold_mean,threeshold_sd,threeshold_t) that:
*    compute the norm of the acceleration in three dimension with the gravity removed gN=|gFx2+gFy2+gFz2−0.8|
*    compute the left rolling average gNm of gN over 10 measurements (see the RcppRoll package)
*    compute the left rolling standard deviation gNsd of gN over 10 measurements
*    filter the data too keep the points where gNm>threeshold_mean and gNsd>threeshold_sd
*    compute dti=timei−timei−1
*    create an id called pid for each group g of points where dti<threeshold_time ,∀i∈g
*    find all the groups of measurements with a duration between 1s and 3s.
*    removes the others groups of measurements
*    create a feature localtimei=timei−mini∈g(timei)
*    create a feature tbini=floor(localtimei∗2)
*    compute the mean for each group and each tbin of gFx,gFy,gFz,wx,wy,wz
*    transform the data to large format where each row is a group and each column a sensor mean over a time bin.

## Tools Used :
R


## Files & directories:
* Input: csv files containing X, O and noise movement to be analysed
* Output: csv files containing processed X, O and noise movement with extract_feature_long function and  extract_feature_large function
* rsconnect: shiny app deployement files
* app.R: shiny app code
