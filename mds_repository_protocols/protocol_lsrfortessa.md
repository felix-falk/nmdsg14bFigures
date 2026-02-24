# Protocol: LSRFortessa Operation
Author: Felix Falk
Latest revision: February 24th 2026
## Precautions
- Check if the LSRFortessa SOP has been updated (last revision: 2023-05-25)
- Never vortex cells or antibody conjugates
- Work in accordance to BLS2
- Please note that some chemicals like FACS clean is corrosive and refer to KLARA to know more about ingredients of all FACS reagents, necessary precautions and procedures in case of an accident. 
## Reagents
- MilliQ Water
- PBS [-]Mg2+ [-]Ca2+ (Gibco, 14190-094)
- CS&T Beads ([BD Biosciences, 656504](https://www.bdbiosciences.com/en-se/products/reagents/flow-cytometry-reagents/clinical-diagnostics/process-and-quality-controls/cs-t-beads.656504?tab=product_details))
- FACS Cleaning Solution ([BD Biosciences, 340345](https://www.bdbiosciences.com/en-se/products/instruments/flow-cytometers/research-cell-analyzers/accuri-c6-plus/bd-facs-cleaning-solution.340345?tab=product_details))
- FACS Detergent Solution (Replaces FACS Rinsing Solution) ([BD Biosciences, 660585](https://www.bdbiosciences.com/en-se/products/instruments/flow-cytometers/research-cell-sorters/accuri-c6-plus/bd-detergent-solution-concentrate.660585?tab=product_details))
- FACSFlow Sheath Fluid ([BD Biosciences, 342003](https://www.bdbiosciences.com/en-se/products/instruments/flow-cytometers/research-cell-analyzers/facscanto-ii/sheath-fluid.342003?tab=product_details))
# Protocol
## 1: Startup (Skip if the instrument is on standby)
1. Check fluidics (FACSFlow and waste).
2. Refill water (yellow lid flask) if necessary. 
3. Turn on the FACSFlow pump (green button, located below the instrument).
4. Turn on the PC. 
5. Turn on the LSRFortessa instrument (green button, right hand side).
6. Launch the Coherent Laser software and manually turn on the UV laser by pressing START and setting the power to 20 mW. 
7. Start Diva and log in (BDAdmin or group account). Check that the connection to the instrument is established (this takes some time). If not, restart the computer, instrument and FACSFlow system. 
8. Place the instrument on high flow rate and prime the instrument 3x with the sample injection port (SIP) arm open and no tube. Use a FACS tube with water to test if air is pushed out of the machine (bubbles should appear). 
## 2: Cleaning, Before Analysis
1. Run 3 ml water at high flow rate for 1 minute with the SIP arm open and 2 minutes with the SIP arm closed. Repeat the same with 3ml cleaning solution, rinsing solution and water again. Acquire data during the clean to check that there is minimal debris. 
## 3: CST Beads (Skip if not first user of the day)
1. Log in to Diva using the admin account, and navigate to the CST interface. 
2. Note the CST lot number in the Diva software. 
3. Run CST beads (1 drop of vortexed beads + 350 ul PBS) at medium flow rate. Ensure that the CST dilution is well vortexed beforehand. See separate SOP for details. Label CST tube "yyyy-mm-dd-initials" and put immediately the left over CST dilution in the 4C fridge. You can reuse the same CST dilution the day after. 
4. After CST, run water for 2 minutes on high flow rate. 
5. Report CST performance in the log book and add any relevant comments. 
## 4: Sample Run
1. Write the date, time, your name and group in the log book. 
2. Log in to Diva using your group account. 
3. Set up your experiment and samples in Diva, using the latest CST settings. Do not use the default experiment filename. 
4. On the left hand screen, use the global worksheet interface to put up histograms and graphs of your markers and gating strategy. 
5. Run your single stain (aka compensation) samples. Manually adjust PMT voltages if necessary. 
6. Run your samples. Set the instrument to "Pause" while exchanging samples. Ensure that samples are well mixed, so that cells pass through the instrument at an even rate, this ensures that the data is as good as possible. 
7. Export your data to a USB Drive immediately and delete the experiment from the Diva software. Any remaining files will be deleted. 
## 5: Cleaning, After Analysis
1. Immediately after completing the sample run, remove the tube and prime 2x on high flow rate (without tube). Run water at high flow rate for 1 minute with the SIP arm open and 4 minutes with the SIP arm closed. Repeat the same with Clean, Rinse and water again. 
2. In a new experiment / cleaning experiment, make an SSC-A vs FSC-A dot plot with a very broad P1 gate to cover almost the whole plot space, leaving out the bottom left corner. Run a tube of water for 1 minute on high flow rate and record, using the same FSC and SSC voltages that you used for your cells. It should be less than 10 events in the P1 gate. Then save the file as a pdf in the "Cleaning report" folder on the desktop and name it with your date, full name and group. 
3. If there are more than 10 events in the P1 gate, repeat the cleaning steps and create a new cleaning report. 
4. Export your data and delete the experiment from the Diva software. Any remaining files will be deleted. 
5.  If you are NOT the last user of the day (refer to the booking calendar), leave the instrument with a water tube, in Standby mode. 
6. Note in the log book the end time as per the computer clock. Report in the log book if you use the HTS or any other non-standard filter configuration. 
7. Clean the desk and instrument surfaces. 
8. If you are the last user of the day (refer to the booking calendar), proceed to shut down. 
## 6: Shut Down
1. Quit the Coherent connection software, Diva and turn off the PC. 
2. Turn off the instrument and FACSFlow supply system (located under the instrument).
## Changing FACSFlow / Waste
1. Please note that FACSFlow and waste should be changed at the same time, no matter which alarm is sounding. 
2. Turn off the FACSFlow supply system. Follow SOP for waste handling. Use PPE (safety goggles and mask), considering that the waste contains live human cells and Virkon. 
3. Add 200 g of Virkon into the full 20 l waste tank (1% Virkon final concentration) and place it on the waste trolley. 
4. Move the FACSFlow empty tank to the waste, if not empty pour leftover in the sink, write "waste", the date and add the label to the box. 
5. Place a new FACSFlow tank in the proper location. Make sure to turn ON the FACSFlow supply system and press Restart. 
6. Write in the logbook under notes if you have changed flow and waste. 

Table 1. Lasers and detectors of the LSRFortessa instrument. 
| **Laser** | **Detector** | **Fluorochrome** |
| --------- | ------------ | ---------------- |
| 355       | NA           | NA               |
| 405       | 450/50       | BV421            |
|           | 525/50       | BV510            |
|           | 780/60       | BV786            |
| 488       | 530/30       | FITC             |
| 561       | 670/30       | 7AAD             |
| 640       | 670/14       | APC              |
|           | 780/16       | APC-Cy7          |
## References
- "SOP LSRFortessa (last revision 230525)" by Belinda Pannagel and Narmadha Subramanian
- "[BD LSRFortessa Cell Analyzer User's Guide](https://www.bdbiosciences.com/en-se/products/instruments/flow-cytometers/research-cell-analyzers/bd-lsrfortessa)" by BD Biosciences
