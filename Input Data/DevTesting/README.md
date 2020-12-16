# Development Testing Tool Data
This directory contains input data for the Development Testing Tool. 

### To roll the year forward, use the script: $MTOM_DIR/Code/DevTesting_adjustStartYr.R, which changes the following files:
- "DevTesting_IC.avg.xlsx"
- "DevTesting_IC.dry.xlsx"
- "DevTesting_IC.wet.xlsx"
- "DevTesting_Inflows.xlsx"
DevTesting_Diversions.xlsx should have enough data for the next few years and doesnt need to be updated.

### RiverSMART Changes
To finish making this change, the RiverSMART study will need to be edited by changing the RunRange event (i.e. Oct2019_RR).
- Change the 'Start Date:' to xxxx-10, where xxxx is the new start year.
- Will need to re-hide unnecessary scenarios in RiverSMART.

### Note:
- RS v.1.3 uses WY 2020 (2019-10 to 2020-09)
- RS v.1.4 uses WY 2021 (2020-10 to 2021-09)

### Change Log (added by R):
Adding year to input data. New files for is for running water year 2021 on 2020-09-21
