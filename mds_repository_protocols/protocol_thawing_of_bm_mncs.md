# Protocol: BM MNC Thawing

Author: Felix Falk

Version: February 20th 2026

# Precautions
- Never vortex cells
- Never use vacuum pump
- Keep cells on ice and try to reduce time that cells are at a high concentration

# Reagents
- DNAse I ([STEMCELL Technologies, 15171507](https://www.fishersci.se/shop/products/dnase-i-solution-1-mg-ml/15171507#?keyword=DNase%20I%20stemcell))
- dPBS [+]Ca2+, [+]Mg2+ ([Gibco, 14040133](https://www.thermofisher.com/order/catalog/product/14040133?SID=srch-srp-14040133))
- dPBS [-]Mg2+ [-]Ca2+ ([Gibco, 14190-094](https://www.thermofisher.com/order/catalog/product/14190094))
- FBS ([Gibco, 1000500-064](https://www.thermofisher.com/order/catalog/product/10500064))
- Trypan Blue, 0.4% ([Sigma Aldrich, T8154-20mL](https://www.sigmaaldrich.com/SE/en/product/sigma/t8154))

# Protocol
1. Turn on the 37°C water bath and fume hood, and prepare thawing media and FACS buffer 
	- 5 ml thawing media
		- 4 ml PBS [+]Ca2+, [+]Mg2+
		- 1 ml FBS
		- 500 μg DNase I (100 μg/ml)
	- 50 ml FACS buffer (Prepare on wet ice, store in 4°C)
		- 49 ml PBS [-]Mg2+ [-]Ca2+
		- 1 ml FBS
2. Transfer BM MNC vials from -140°C freezer to BSL2 lab, on dry ice
3. Quickly thaw vials in 37°C water bath
4. When ice crystals disappear, swab surface of vials with 70% ethanol
5. Slowly transfer thawed cells into a 50 ml conical tube using a wide-bore P1000 pipette tip. Do *not* mix by pipetting up and down. Rinse the cryovial with 1000 ul of thawing media and add drop-wise to the cells, one drop every 3-5 seconds, making sure to mix between each drop by gently swirling the tube. Continue adding more medium drop-wise, each drop every 5 seconds until final volume is 30 ml. Gently invert to mix.
6. Centrifuge tubes (200 g, 15 min, RT, acc 4, dec 1), prepare for antibody staining during centrifugation.
7. Carefully discard the supernatant, leaving a small amount of medium behind to ensure cell pellet is not disturbed.
8. Gently resuspend cell pellet in 1000 ul FACS buffer using a P200 pipette.
9. Dissolve 10 ul cells in 90 ul Trypan blue, and assess cell number and % viability with a hemocytometer. Cells/ml = Cells per large square * 10 (dilution factor) * 10 000 
10. In biobank database, note which samples have been removed from the biobank, the cell number and % viability
11. Proceed immediately to FACS and CITE-seq staining protocol

# References
1. “[Cell Thawing Protocols for Single Cell Assays, CG000447, Rev B](https://www.10xgenomics.com/support/universal-three-prime-gene-expression/documentation/steps/sample-prep/cell-thawing-protocols-for-single-cell-assays)” by 10x Genomics
2. “KI_SOP_Freezing and thawing human and mouse cells” by Kari Högstrand and Ellen Markljung
3. “[CHIP_034_HSPC_mature_CHIP_ddPCR_210325_PSW](https://www.nature.com/articles/s41588-025-02405-w)” by Tetsuichi Yoshizato and Petter Woll
4. "[Coordinated immune networks in leukemia bone marrow microenvironments distinguish response to cellular therapy](https://www.science.org/doi/10.1126/sciimmunol.adr0782?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed)" by Katie Maurer et al.
