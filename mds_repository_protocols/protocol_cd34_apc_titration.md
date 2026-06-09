# Protocol: CD34 Antibody Titration
Author: Felix Falk

Date: April 21st 2026

## Precautions
- This protocol assumes that FACS buffer has been prepared **before** this protocol starts
- Never vortex thawed cells or antibody conjugates
- Turn off lights in the workspace when working with fluorescent antibodies
## Reagents
- BM MNCs (1 vial, 10 million thawed cells, counted and resuspended in 1000 µl FACS buffer
- FcR block stock solution ([Miltenyi Biotec, 130-059-901](https://www.miltenyibiotec.com/SE-en/products/fcr-blocking-reagent-human.html#130-059-901))
- FBS ([Gibco, 1000500-064](https://www.fishersci.se/shop/products/gibco-fetal-bovine-serum-certified-heat-inactivated-us-origin-2/11533387))
- PBS [-]Mg2+ [-]Ca2+ ([Gibco, 14190-094](https://www.thermofisher.com/order/catalog/product/14190094))
- Trypan Blue, 0.4% ([Sigma Aldrich, T8154-20mL](https://www.sigmaaldrich.com/SE/en/product/sigma/t8154))
- 7-AAD Viability Staining Solution ([BioLegend, 420403](https://www.biolegend.com/en-us/products/7-aad-viability-staining-solution-1649))
- APC anti-human CD34 ([BD Biosciences, 555824](https://www.bdbiosciences.com/en-de/products/reagents/flow-cytometry-reagents/research-reagents/single-color-antibodies-ruo/apc-mouse-anti-human-cd34.555824?tab=product_details))
- FITC anti-human CD3 ([BioLegend, 300306](https://nordicbiosite.com/product/300306/FITC-antihuman-CD3))

Table 1. Each antibody to be titrated, and their respective co-stain and co-stain mix.

|       | **Target** | **Conjugate** | **Negative co-stain** | **Conjugate** | **Dilution** | **Co-stain mix** |
| ----- | ---------- | ------------- | --------------------- | ------------- | ------------ | ---------------- |
| HSPCs | CD34       | APC           | CD3                   | FITC          | 1:100        | 1                |

# Protocol
## 1: Calculate Number of Samples
We have _1_ antibody to be titrated at *5* concentrations. 1 x 5 = 5 titration samples. We have _2_ FMO samples (CD34, 7AAD), _2_ single-stain samples (CD34, 7AAD) and _1_ unstained sample, for a total of _10_ samples.
## 2: Prepare FACS Buffer and Microcentrifuge Tubes
1. In a BSL2 hood, prepare 50 ml FACS buffer (49 ml PBS, 1 ml FBS) on the day of the sort and store it in in 4°C (should already have been prepared in the thawing protocol).
2. In a BSL1 lab, prepare _12_ microcentrifuge tubes.
	1. 10 samples + 1 FcR block master mix + 1 co-stain master mix = **12**
	2. Pipet cooled FACS buffer into each of the microcentrifuge tubes according to Table 1, keep cool and put in the fridge (4°C).

Table 2. FACS buffer to be added to each microcentrifuge tube.

| **Vial**                    | **FACS buffer per vial (µl)** |
| --------------------------- | ----------------------------- |
| FcR block master mix (1x)   | 337.5                         |
| Co-stain #1 master mix (1x) | 125                           |
| Serial dilution 1 (1x)      | 20                            |
| Serial dilutions 2-5 (x4)   | 12.5                          |
| CD34 FMO (x1)               | 12.5                          |
| 7AAD FMO (x1)               | 10                            |
| Single stain (x2)           | 22.5                          |
| 7AAD master mix (1x)        | 499                           |
| Unstained sample (1x)       | 12.5                          |
## 3: Antibody Stock Solution Pipetting
1. Performed in BSL2, in hood with light switched off. Keep samples cool throughout.
	1. Note AB lot number.
	2. Spin down antibody stock tubes (CD34-APC and CD3-FITC) to clear aggregates (2000 g, 1 min, 4°C). Use start/select button to confirm the settings in the TCR3 microcentrifuge.
	3. Pipet 37.5 µl FcR block stock solution into 337.5 µl FACS buffer. In this protocol, a total of 12 samples need to be FcR blocked, therefore prepare FcR block solution for 15 samples. Total FcR block = 15 x 25 µl = 375 µl.
	4. Pipet 5 µl CD34-APC antibody stock solution into first serial dilution tube (Table 3).
	5. Pipet 2.5 µl CD3-FITC antibody stock solution into co-stain master mix (Table 4).
	6. Pipet antibodies from stock solutions into the FMO samples according to the FMO table (Table 5).
	7. Pipet single stain controls according to the single stain table (Table 6).
	8. Pipet 1 µl 7AAD solution into 499 µl FACS buffer, to create a 100 µg/µl dilution from the original 5 mg/µl stock.
	9. Gently pipette mix and maintain at 4°C. Avoid light exposure.
2. Move to BSL1, keep samples cool and shielded from light throughout.
	1. Pipet serial dilution tubes for the CD34-APC antibody, reusing the pipet tip between samples with the same antibody.
	2. Add co-stain mix (12.5 µl) to each CD34-APC serial dilution, FMO control and the unstained sample, for a total of 25 µl in each serial dilution and FMO control sample. Here you should use a new pipet tip in between each dilution.
3. Put serial dilutions and ABs in fridge.

Table 3. Serial dilution of each new lot to be titrated. (12.5 µl for each tube in 50 µl final volume, therefore 4x dilution.) The recommended dilution for both BD and BioLegend antibodies is 1:20. The sixth sample is the FMO sample for the CD34 antibody, containing 0 µl CD34 antibody.

| **Dilution no.** | **Final dilution** | **4x dilution** | **AB vol. (µl)**          | **FACS buffer vol. (µl)** |
| ---------------- | ------------------ | --------------- | ------------------------- | ------------------------- |
| 1                | 1:20               | 1:5             | 5 (from AB stock)         | 20                        |
| 2                | 1:40               | 1:10            | 12.5 (from previous dil.) | 12.5                      |
| 3                | 1:80               | 1:20            | 12.5 (from previous dil.) | 12.5                      |
| 4                | 1:160              | 1:40            | 12.5 (from previous dil.) | 12.5                      |
| 5                | 1:320              | 1:80            | 12.5 (from previous dil.) | 12.5                      |

Table 4. Co-stain mix #1 for samples with CD3-FITC co-stain. (12.5 µl per sample, 8 samples) Add 12.5 µl of co-stain mix to prepared serial dilution tubes. In this protocol, 8 samples need co-stain mix (5 serial dilutions + 2 FMO controls + 1 unstained sample).

| **Antibody** | **Final dilution** | **4x pre-dilution** | **Volume per sample (µl)** | **x10** | **Final volume (µl)** |
| ------------ | ------------------ | ------------------- | -------------------------- | ------- | --------------------- |
| CD3-FITC     | 1:100              | 1:25                | 0.25                       |         | 2.5 (from AB stock)   |
| FACS buffer  |                    |                     | 12.25                      |         | 122.5                 |
| **Total**    |                    |                     | **12.5**                   |         | **125**               |

Table 5. FMO samples. Pipet CD34 antibody from stock solution.

| **FMO**  | **AB vol., fom stock (µl)** | **FACS buffer vol. (µl)** |
| -------- | --------------------------- | ------------------------- |
| CD34-APC | 0                           | 12.5                      |
| 7AAD     | 2.5                         | 10                        |

Table 6. Single stain samples.

| **Single stain** | **AB vol., from stock (µl)** | **FACS buffer vol. (µl)** |
| ---------------- | ---------------------------- | ------------------------- |
| CD34-APC         | 2.5                          | 22.5                      |
| 7AAD             | 2.5                          | 22.5                      |
## 4: Fc Receptor Blocking
1. Take off 1 µl of cells for the unstained sample and put in the prepared unstained microcentrifuge tube with 24 µl of FACS buffer.
2. Centrifuge cells (5 min, 500g, 4°C).
3. Resuspend the cell pellet in 375 ul FcR block solution. Mix well and incubate for 5 minutes in the refrigerator (4°C).
4. Proceed to staining the cells according to the staining panel.
## 5: Staining with Fluorescent Antibodies
1. Mix antibody cocktails and FcR blocked cells (5 antibody serial dilutions, 2 FMO samples, 2 single stain samples) (25 µl cells + 25 Ab mix). Gently pipet to mix and incubate for 15 minutes at 4°C in the dark.
## 6: Cell Washing and Staining with 7AAD
1. Add 1 ml chilled FACS buffer to each of the stained cell vials.
2. Centrifuge (500 g, 5 min, 4°C) and carefully aspirate the supernatant.
3. (Repeat the wash one-two more times, for a total of three washes.)
	1. Collect a small volume of cell suspension and count cell using Trypan Blue after each wash. Evaluate the recovery rate after the wash before sorting (pick 5 samples at random and evaluate). Cells/ml = (cells per quad/4) x dilution factor x 10, 000
4. Resuspend the samples in 297 µl FACS buffer.
5. From the 100 µg/ul 7AAD dilution, add 3 µl each to the serial dilution, FMO and single stain samples before sort to create 1 µg/µl final dilutions.
6. Proceed to cell analysing with the LSRFortessa as quickly as possible to preserve cell viability.
## References
1. “230925 FACS staining eBioscience kit” by Itzel Medina Andrade
2. “Combined antibody titration panel” by Petter Woll and Vanessa Marrone
3. “KI_SOP_Antibody_protocol” by Ellen Markljung and Kari Högstrand
4. “CHIP_034_HSPC_mature_CHIP_ddPCR_210325_PSW” by Tetsuichi Yoshizato and Petter Woll
5. “[Molecular and cellular dynamics of measurable residual disease progression in myelodysplastic syndromes](https://www.biorxiv.org/content/10.1101/2025.10.17.682671v1)” by Yu-Hsiang Chen et al.

![[Skärmavbild 2026-04-21 kl. 10.46.29.png]]

![[Skärmavbild 2026-04-21 kl. 10.47.14.png]]

![[Skärmavbild 2026-04-21 kl. 10.47.31.png]]
