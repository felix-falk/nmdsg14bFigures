# Create MAF file for NMDS14B part 1 data, based on NGS file

# Load libraries
library(readxl)
library(dplyr)
library(tidyverse)

# Set working directory
setwd("~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Dokument/mds_project")

# Import datasets
ngs_p1 <- read_excel("NMDS14B_p1_data/NGS_Uppsala_2024-01-17.xlsx")
general_info_p1_joel <- read_excel("NMDS14B_p1_data/mds_data_20240930.xlsx")
sct_p1 <- read_excel("NMDS14B_p1_data/SCT_parameters_2026_02_20.xlsx")
ngs_p2 <- read_excel("NMDS14B_p2_data/NGS_lista_NMDSG14B2.xlsx")
general_info_p2 <- read_excel("NMDS14B_p2_data/NMDS14B-Inkl-screen-EoS.xlsx")

# Add Entrez_gene_id column to ngs_p1, based on gene_symbol
ngs_p1 <- ngs_p1 %>% mutate(gene_id = case_when(
  gene_symbol == "ASXL1" ~ 171023,
  gene_symbol == "ATRX" ~ 546,
  gene_symbol == "BCOR" ~ 54880,
  gene_symbol == "BCORL1" ~ 63035,
  gene_symbol == "BRAF" ~ 673,
  gene_symbol == "CALR" ~ 811,
  gene_symbol == "CBL" ~ 867,
  gene_symbol == "CEBPA" ~ 1050,
  gene_symbol == "complex" ~ NA,
  gene_symbol == "CREBBP" ~ 1387,
  gene_symbol == "CSF3R" ~ 1441,
  gene_symbol == "CUX1" ~ 1523,
  gene_symbol == "DDX41" ~ 51428,
  gene_symbol == "DNMT3A" ~ 1788,
  gene_symbol == "ETV6" ~ 2120,
  gene_symbol == "EZH2" ~ 2146,
  gene_symbol == "FBXW7" ~ 55294,
  gene_symbol == "FLT3" ~ 2322,
  gene_symbol == "GATA2" ~ 2624,
  gene_symbol == "GNAS" ~ 2778,
  gene_symbol == "IDH1" ~ 3417,
  gene_symbol == "IDH2" ~ 3418,
  gene_symbol == "IKZF1" ~ 10320,
  gene_symbol == "JAK2" ~ 3717,
  gene_symbol == "KDM6A" ~ 7403,
  gene_symbol == "KIT" ~ 3815,
  gene_symbol == "KMT2A" ~ 4297,
  gene_symbol == "KRAS" ~ 3845,
  gene_symbol == "MPL" ~ 4352,
  gene_symbol == "NF1" ~ 4763,
  gene_symbol == "No pathogenic variant detected" ~ NA,
  gene_symbol == "No variants" ~ NA,
  gene_symbol == "NOTCH1" ~ 4851,
  gene_symbol == "NPM1" ~ 4869,
  gene_symbol == "NRAS" ~ 4893,
  gene_symbol == "PDGFRA" ~ 5156,
  gene_symbol == "PHF6" ~ 84295,
  gene_symbol == "PPM1D" ~ 8493,
  gene_symbol == "PTPN1" ~ 5770,
  gene_symbol == "PTPN11" ~ 5781,
  gene_symbol == "RAD21" ~ 5885,
  gene_symbol == "RUNX1" ~ 861,
  gene_symbol == "SAMD9L" ~ 219285,
  gene_symbol == "SETBP1" ~ 26040,
  gene_symbol == "SETBP1 not in panel" ~ 26040,
  gene_symbol == "SETBP1 variant not in panel" ~ 26040,
  gene_symbol == "SF3B1" ~ 23451,
  gene_symbol == "SMC1A" ~ 8243,
  gene_symbol == "SMC3" ~ 9126,
  gene_symbol == "SRSF2" ~ 6427,
  gene_symbol == "STAG2" ~ 10735,
  gene_symbol == "TET2" ~ 54790,
  gene_symbol == "TP53" ~ 7157,
  gene_symbol == "U2AF1" ~ 7307,
  gene_symbol == "WT1" ~ 7490,
  gene_symbol == "ZRSR2" ~ 8233,
  gene_symbol == NA ~ NA,
))

# Change column names for ngs_p2
names(ngs_p2)[names(ngs_p2) == "Gen"] <- "gene_id"
names(ngs_p2)[names(ngs_p2) == "Studienummer"] <- "patno"

ngs_p1 <- ngs_p1 %>% mutate(chr = case_when(
  chromosome == 1 ~ "chr1",
  chromosome == 2 ~ "chr2",
  chromosome == 3 ~ "chr3",
  chromosome == 4 ~ "chr4",
  chromosome == 5 ~ "chr5",
  chromosome == 6 ~ "chr6",
  chromosome == 7 ~ "chr7",
  chromosome == 8 ~ "chr8",
  chromosome == 9 ~ "chr9",
  chromosome == 10 ~ "chr10",
  chromosome == 11 ~ "chr11",
  chromosome == 12 ~ "chr12",
  chromosome == 13 ~ "chr13",
  chromosome == 14 ~ "chr14",
  chromosome == 15 ~ "chr15",
  chromosome == 16 ~ "chr16",
  chromosome == 17 ~ "chr17",
  chromosome == 18 ~ "chr18",
  chromosome == 19 ~ "chr19",
  chromosome == 20 ~ "chr20",
  chromosome == 21 ~ "chr21",
  chromosome == 22 ~ "chr22",
  chromosome == "X" ~ "chrX",
  chromosome == "Y" ~ "chrY"
))

# Gene to chromosome mapping
gene_chr <- c(
  SMC3="chr10", BCOR="chrX", STAG2="chrX", ASXL1="chr20", EZH2="chr7",
  RUNX1="chr21", CBL="chr11", ZRSR2="chrX", TP53="chr17", SETBP1="chr18",
  DNMT3A="chr2", SF3B1="chr2", GATA2="chr3", PTPRF="chr1", DICER1="chr14",
  IDH1="chr2", ETV6="chr12", CEBPA="chr19", EP300="chr22", U2AF2="chr19",
  TET2="chr4", KDM6A="chrX", SH2B3="chr12", U2AF1="chr21", ATRX="chrX",
  MPL="chr1", CSF3R="chr1", SRSF2="chr17", SRCAP="chr16", NF1="chr17",
  IDH2="chr15", PHF6="chrX", CUX1="chr7", JAK2="chr9", NOTCH1="chr9",
  SMC1A="chrX", KIT="chr4", PTPN11="chr12", NRAS="chr1", DDX41="chr5",
  NT5C2="chr10", KRAS="chr12", SF3A1="chr22", `C7orf55-LUC7L2`="chr7",
  PHIP="chr6", MYB="chr6", FLT3="chr13", ETNK1="chr12", IKZF1="chr7",
  RIT1="chr1", WT1="chr11", KMT2D="chr12", BRAF="chr7", BCORL1="chrX",
  ABL1="chr9", NPM1="chr5", KMT2A="chr11", GNB1="chr1", SETPB1="chr18",
  PPM1D="chr17", ANKRD26="chr10", `FLT3-TKD`="chr13", `FLT3-ITD`="chr13",
  XPO1="chr2", JARID2="chr6", ARHGEF10="chr8", NXF1="chr11", ASXL2="chr2",
  NFE2="chr12", LUC7L2="chr7", STAT5B="chr17", STAG1="chr3", JAK1="chr1",
  CREBBP="chr16", PLCG2="chr16", BRCC3="chrX", ZBTB33="chrX", RAD21="chr8",
  CDKN2C="chr1", CSF2RB="chr22", ASXL="chr20", CDKN2A="chr9", CBLB="chr3",
  PRPF8="chr17", CALR="chr19", UBTF="chr17", `UBTF-TD`="chr17", MYD88="chr3"
)

# Add chromosome column
ngs_p2 <- ngs_p2 %>%
  mutate(chr = gene_chr[gene_id])

# Optional: handle missing/NA genes
ngs_p2$chr[is.na(ngs_p2$chr)] <- NA

classify_maf_variant <- function(hgvsp, hgvsc = NA) {
  
  result <- rep(NA_character_, length(hgvsp))
  
  # Ensure character
  hgvsp <- as.character(hgvsp)
  hgvsc <- as.character(hgvsc)
  
  # --- Protein-based classifications ---
  
  # Nonsense (stop gained)
  result[grepl("\\*$", hgvsp)] <- "Nonsense_Mutation"
  
  # Nonstop (stop lost)
  result[grepl("\\*[^$]", hgvsp)] <- "Nonstop_Mutation"
  
  # Frameshift
  fs_idx <- grepl("fs", hgvsp)
  result[fs_idx & grepl("del", hgvsp)] <- "Frame_Shift_Del"
  result[fs_idx & grepl("ins", hgvsp)] <- "Frame_Shift_Ins"
  result[fs_idx & is.na(result)] <- "Frame_Shift_Ins"  # fallback
  
  # In-frame indels
  result[grepl("del", hgvsp) & !grepl("fs", hgvsp)] <- "In_Frame_Del"
  result[grepl("ins", hgvsp) & !grepl("fs", hgvsp)] <- "In_Frame_Ins"
  
  # Missense vs Silent
  missense_idx <- grepl("^p\\.[A-Z][0-9]+[A-Z]$", hgvsp)
  if (any(missense_idx, na.rm = TRUE)) {
    aa_from <- substr(hgvsp[missense_idx], 3, 3)
    aa_to <- sub(".*([A-Z])$", "\\1", hgvsp[missense_idx])
    
    silent <- aa_from == aa_to
    
    result[missense_idx][silent] <- "Silent"
    result[missense_idx][!silent] <- "Missense_Mutation"
  }
  
  # Translation start site
  result[grepl("^p\\.M1[?A-Z*]", hgvsp)] <- "Translation_Start_Site"
  
  # --- cDNA-based classifications (fallback / complementary) ---
  
  # Splice site
  splice_idx <- grepl("[+-][0-9]+", hgvsc)
  result[is.na(result) & splice_idx] <- "Splice_Site"
  
  # UTRs
  result[is.na(result) & grepl("^c\\.-", hgvsc)] <- "5'UTR"
  result[is.na(result) & grepl("\\*\\d+", hgvsc)] <- "3'UTR"
  
  # Intronic
  result[is.na(result) & grepl("[+-][0-9]+", hgvsc)] <- "Intron"
  
  # RNA / non-coding (very rough fallback)
  result[is.na(result) & grepl("^n\\.", hgvsc)] <- "RNA"
  
  # Default fallback
  result[is.na(result)] <- "Targeted_Region"
  
  return(result)
}

# Add type_of_mutation column to ngs_p1
ngs_p1 <- ngs_p1 %>% mutate(type_of_mutation = classify_maf_variant(protein_variant))

# Add type_of_mutation column to ngs_p2
ngs_p2 <- ngs_p2 %>% mutate(type_of_mutation = classify_maf_variant(Proteinförändring))

# Function to determine variant_type
classify_variant_type <- function(hgvsp, hgvsc = NA) {
  
  result <- rep(NA_character_, length(hgvsp))
  
  # Ensure character
  hgvsp <- as.character(hgvsp)
  hgvsc <- as.character(hgvsc)
  
  # --- Protein-based rules ---
  
  # Deletions
  result[grepl("del", hgvsp)] <- "DEL"
  
  # Insertions
  result[grepl("ins", hgvsp)] <- "INS"
  
  # Frameshift (try to refine)
  fs_idx <- grepl("fs", hgvsp)
  result[fs_idx & grepl("del", hgvsp)] <- "DEL"
  result[fs_idx & grepl("ins", hgvsp)] <- "INS"
  result[fs_idx & is.na(result)] <- "INS"  # fallback (common convention)
  
  # Simple substitutions (missense, nonsense, silent)
  snp_idx <- grepl("^p\\.[A-Z][0-9]+[A-Z*]$", hgvsp)
  result[snp_idx] <- "SNP"
  
  # --- cDNA fallback (important!) ---
  
  # Deletion at DNA level
  result[is.na(result) & grepl("del", hgvsc)] <- "DEL"
  
  # Insertion at DNA level
  result[is.na(result) & grepl("ins", hgvsc)] <- "INS"
  
  # Substitution at DNA level
  result[is.na(result) & grepl(">", hgvsc)] <- "SNP"
  
  # Final fallback
  result[is.na(result)] <- "SNP"
  
  return(result)
}

# Add type_of_variant column to ngs_p1
ngs_p1 <- ngs_p1 %>% mutate(type_of_variant = classify_variant_type(protein_variant))

# Add type_of_variant column to ngs_p2
ngs_p2 <- ngs_p2 %>% mutate(type_of_variant = classify_variant_type(Proteinförändring))

# Change Study number to study_number in general_info_p1_joel
general_info_p1_joel <- general_info_p1_joel %>% 
  rename(study_number = Study_nr)

# Create outcome columns in general_info_p1_joel and general_info_p2
general_info_p1_joel <- general_info_p1_joel %>% 
  mutate(outcome = ifelse(cens_CR == "censor", "remission", cens_CR))
general_info_p2 <- general_info_p2 %>%
  mutate(
    outcome = ifelse(
      eosreason == "2 years post HCT" | is.na(eosreason),
      "remission",
      "censor"
    )
  )

# Create TP53 column in general_info_p1_joel
general_info_p1_joel <- general_info_p1_joel %>%
  mutate(TP53_boolean = ifelse(Pres_dx_TP53 == 1 | Pres_incl_TP53 == 1, TRUE, FALSE))

# Create aGVHD_3_or_higher column in sct_p1
sct_p1 <- sct_p1 %>% 
  mutate(aGVHD_3_or_higher = ifelse(as.numeric(max_grade_a_gvhd) >= 3, TRUE, FALSE))

# Create cGVHD_severe column in sct_p1
sct_p1 <- sct_p1 %>% 
  mutate(cGVHD_severe = ifelse(c_gvhd_grade == "Severe", TRUE, FALSE))

# Add outcome column from general_info_p1_joel to ngs_p1, based on study number
ngs_p1 <- ngs_p1 %>%
  left_join(general_info_p1_joel %>% select(study_number, outcome), by = "study_number")
ngs_p1 <- ngs_p1 %>%
  left_join(general_info_p1_joel %>% select(study_number, TP53_boolean), by = "study_number")
general_info_p2 <- general_info_p2 %>% mutate(patno = as.character(patno))
ngs_p2 <- ngs_p2 %>%
  left_join(general_info_p2 %>% select(patno, outcome), by = "patno")

# Add aGVHD_3_or_higher and cGVHD_severe columns from general_info_p1_joel to ngs_p1, based on study number
ngs_p1 <- ngs_p1 %>%
  left_join(sct_p1 %>% select(study_number, aGVHD_3_or_higher), by = "study_number")
ngs_p1 <- ngs_p1 %>%
  left_join(sct_p1 %>% select(study_number, cGVHD_severe), by = "study_number")

# Gene -> HGNC ID mapping
gene_hugo <- c(
  SMC3="10649",
  BCOR="20893",
  STAG2="11355",
  ASXL1="18391",
  EZH2="3527",
  RUNX1="10471",
  CBL="1541",
  ZRSR2="24307",
  TP53="11998",
  SETBP1="15573",
  DNMT3A="2978",
  SF3B1="10768",
  GATA2="4171",
  PTPRF="9671",
  DICER1="17098",
  IDH1="5382",
  ETV6="3495",
  CEBPA="1833",
  EP300="3373",
  U2AF2="12505",
  TET2="25941",
  KDM6A="12637",
  SH2B3="29610",
  U2AF1="12450",
  ATRX="886",
  MPL="7217",
  CSF3R="2439",
  SRSF2="10772",
  SRCAP="11297",
  NF1="7765",
  IDH2="5383",
  PHF6="18039",
  CUX1="2557",
  JAK2="6192",
  NOTCH1="7881",
  SMC1A="11111",
  KIT="6342",
  PTPN11="9644",
  NRAS="7989",
  DDX41="18647",
  NT5C2="8026",
  KRAS="6407",
  SF3A1="10765",
  `C7orf55-LUC7L2`="30426",
  PHIP="18010",
  MYB="7545",
  FLT3="3765",
  ETNK1="28509",
  IKZF1="13176",
  RIT1="10017",
  WT1="12796",
  KMT2D="7133",
  BRAF="1097",
  BCORL1="25654",
  ABL1="76",
  NPM1="7910",
  KMT2A="7132",
  GNB1="4396",
  SETPB1="15573",
  PPM1D="9277",
  ANKRD26="24057",
  `FLT3-TKD`="3765",
  `FLT3-ITD`="3765",
  XPO1="12825",
  JARID2="6197",
  ARHGEF10="21008",
  NXF1="8048",
  ASXL2="20894",
  NFE2="7776",
  LUC7L2="20315",
  STAT5B="11368",
  STAG1="11356",
  JAK1="6190",
  CREBBP="2348",
  PLCG2="9076",
  BRCC3="17393",
  ZBTB33="29430",
  RAD21="9811",
  CDKN2C="1789",
  CSF2RB="2440",
  ASXL="18391",
  CDKN2A="1787",
  CBLB="1542",
  PRPF8="17340",
  CALR="1455",
  UBTF="12514",
  `UBTF-TD`="12514",
  MYD88="7562"
)

# Add HGNC ID column to ngs_p2
ngs_p2 <- ngs_p2 %>%
  mutate(
    hugo_id = gene_hugo[gene_id]
  )

# Add Hugo_Symbol column to ngs_p1
nmds14b_p1_maf <- ngs_p1 %>% 
  mutate(Hugo_Symbol = gene_symbol,
         Entrez_Gene_Id = gene_id,
         Center = NA,
         NCBI_Build = "GRCh38", # Use GRCh38 as the genome build
         Chromosome = chr,
         Start_Position = NA,
         End_Position = NA,
         Strand = NA,
         Variant_Classification = type_of_mutation, 
         Variant_Type = type_of_variant,
         Reference_Allele = NA,
         Tumor_Seq_Allele1 = NA,
         Tumor_Seq_Allele2 = NA,
         dbSNP_RS = NA,
         dbSNP_Val_Status = NA,
         Tumor_Sample_Barcode = study_number, # Use study_number as sample ID
         Matched_Norm_Sample_Barcode = NA,
         Match_Norm_Seq_Allele1 = NA,
         Match_Norm_Seq_Allele2 = NA,
         Tumor_Validation_Allele1 = NA,
         Tumor_Validation_Allele2 = NA,
         Match_Norm_Validation_Allele1 = NA,
         Match_Norm_Validation_Allele2 = NA,
         Verification_Status = NA,
         Validation_Status = NA,
         Mutation_Status = NA,
         Sequencing_Phase = NA,
         Sequence_Source = NA,
         Validation_Method = NA,
         Score = NA,
         BAM_File = NA,
         Sequencer = NA,
         Tumor_Sample_UUID = NA,
         Matched_Norm_Sample_UUID = NA,
         HGVSc = NA,
         HGVSp = NA,
         HGVSp_Short = NA,
         Transcript_ID = NA,
         Exon_Number = NA,
         t_depth = NA,
         t_ref_count = NA,
         t_alt_count = NA,
         n_depth = NA,
         n_ref_count = NA,
         n_alt_count = NA,
         all_effects = NA,
         Allele = NA,
         Gene = NA,
         Feature = NA,
         Feature_type = NA,
         One_Consequence = NA,
         Consequence = NA,
         cDNA_position = NA,
         CDS_position = NA,
         Protein_position = NA,
         Amino_acids = NA,
         Codons = NA,
         Existing_variation = NA,
         ALLELE_NUM = NA,
         DISTANCE = NA,
         TRANSCRIPT_STRAND = NA,
         SYMBOL = NA,
         SYMBOL_SOURCE = NA,
                                    HGNC_ID = NA,
                                    BIOTYPE = NA,
                                    CANONICAL = NA,
                                    CCDS = NA,
                                    ENSP = NA,
                                    SWISSPROT = NA,
                                    TREMBL = NA,
                                    UNIPARC = NA,
                                    RefSeq = NA,
                                    SIFT = NA,
                                    PolyPhen = NA,
                                    EXON = NA, 
                                    INTRON = NA,
                                    DOMAINS = NA, 
                                    GMAF = NA, 
                                    AFR_MAF = NA, 
                                    AMR_MAF = NA, 
                                    ASN_MAF = NA, 
                                    EAS_MAF = NA, 
                                    EUR_MAF = NA, 
                                    SAS_MAF = NA, 
                                    AA_MAF = NA, 
                                    EA_MAF = NA,
                                    CLIN_SIG = NA,
                                    SOMATIC = NA, 
                                    PUBMED = NA, 
                                    MOTIF_NAME = NA, 
                                    MOTIF_POS = NA, 
                                    HIGH_INF_POS = NA, 
                                    MOTIF_SCORE_CHANGE = NA, 
                                    IMPACT = NA,
                                    PICK = NA,
                                    VARIANT_CLASS = NA,
                                    TSL = NA,
                                    HGVS_OFFSET = NA,
                                    PHENO = NA,
                                    MINIMISED = NA,
                                    ExAC_AF = NA,
                                    ExAC_AF_Adj = NA,
                                    ExAC_AF_AFR = NA,
                                    ExAC_AF_AMR = NA,
                                    ExAC_AF_EAS = NA,
                                    ExAC_AF_FIN = NA,
                                    ExAC_AF_NFE = NA,
                                    ExAC_AF_OTH = NA,
                                    ExAC_AF_SAS = NA,
                                    GENE_PHENO = NA,
                                    FILTER = NA,
                                    CONTEXT = NA,
                                    src_vcf_id = NA,
                                    tumor_bam_uuid = NA,
                                    normal_bam_uuid = NA,
                                    case_id = NA,
                                    GDC_FILTER = NA,
                                    COSMIC = NA,
                                    MC3_Overlap = NA,
                                    GDC_Validation_Status = NA,
                                    GDC_Valid_Somatic = NA,
                                    vcf_region = NA,
                                    vcf_info = NA,
                                    vcf_format = NA,
                                    vcf_tumor_gt = NA,
                                    vcf_normal_gt = NA,
         tumor_VAF = allele_fraction_percent,
         Protein_Change = protein_variant, 
         Outcome = outcome, 
         aGVHD_3_or_Higher = aGVHD_3_or_higher,
         cGVHD_Severe = cGVHD_severe,
         TP53 = TP53_boolean) %>%
  select(!c(study_number, 
            sampling_date, 
            sample_type, 
            adjTP53_multihit, 
            comment, 
            chromosome, 
            position, 
            gene_symbol, 
            transcript_id, 
            transcript_variant, 
            protein_variant, 
            read_depth, 
            allele_fraction_percent, 
            assessment, 
            twist_trusight, 
            include_in_publication, 
            to_include_1_yes_0_no,
            gene_id,
            chr, 
            type_of_mutation,
            type_of_variant,
            outcome,
            aGVHD_3_or_higher,
            cGVHD_severe,
            TP53_boolean))
  
# Add Hugo_Symbol column to ngs_p1
nmds14b_p2_maf <- ngs_p2 %>% 
  mutate(Hugo_Symbol = gene_id,
         Entrez_Gene_Id = gene_id,
         Center = NA,
         NCBI_Build = "GRCh38", # Use GRCh38 as the genome build
         Chromosome = chr,
         Start_Position = NA,
         End_Position = NA,
         Strand = NA,
         Variant_Classification = type_of_mutation,
         Variant_Type = type_of_variant,
         Reference_Allele = NA,
         Tumor_Seq_Allele1 = NA,
         Tumor_Seq_Allele2 = NA,
         dbSNP_RS = NA,
         dbSNP_Val_Status = NA,
         Tumor_Sample_Barcode = patno, # Use study_number as sample ID
         Matched_Norm_Sample_Barcode = NA,
         Match_Norm_Seq_Allele1 = NA,
         Match_Norm_Seq_Allele2 = NA,
         Tumor_Validation_Allele1 = NA,
         Tumor_Validation_Allele2 = NA,
         Match_Norm_Validation_Allele1 = NA,
         Match_Norm_Validation_Allele2 = NA,
         Verification_Status = NA,
         Validation_Status = NA,
         Mutation_Status = NA,
         Sequencing_Phase = NA,
         Sequence_Source = NA,
         Validation_Method = NA,
         Score = NA,
         BAM_File = NA,
         Sequencer = NA,
         Tumor_Sample_UUID = NA,
         Matched_Norm_Sample_UUID = NA,
         HGVSc = NA,
         HGVSp = NA,
         HGVSp_Short = NA,
         Transcript_ID = NA,
         Exon_Number = NA,
         t_depth = NA,
         t_ref_count = NA,
         t_alt_count = NA,
         n_depth = NA,
         n_ref_count = NA,
         n_alt_count = NA,
         all_effects = NA,
         Allele = NA,
         Gene = NA,
         Feature = NA,
         Feature_type = NA,
         One_Consequence = NA,
         Consequence = NA,
         cDNA_position = NA,
         CDS_position = NA,
         Protein_position = NA,
         Amino_acids = NA,
         Codons = NA,
         Existing_variation = NA,
         ALLELE_NUM = NA,
         DISTANCE = NA,
         TRANSCRIPT_STRAND = NA,
         SYMBOL = NA,
         SYMBOL_SOURCE = NA,
         HGNC_ID = NA,
         BIOTYPE = NA,
         CANONICAL = NA,
         CCDS = NA,
         ENSP = NA,
         SWISSPROT = NA,
         TREMBL = NA,
         UNIPARC = NA,
         RefSeq = NA,
         SIFT = NA,
         PolyPhen = NA,
         EXON = NA, 
         INTRON = NA,
         DOMAINS = NA, 
         GMAF = NA, 
         AFR_MAF = NA, 
         AMR_MAF = NA, 
         ASN_MAF = NA, 
         EAS_MAF = NA, 
         EUR_MAF = NA, 
         SAS_MAF = NA, 
         AA_MAF = NA, 
         EA_MAF = NA,
         CLIN_SIG = NA,
         SOMATIC = NA, 
         PUBMED = NA, 
         MOTIF_NAME = NA, 
         MOTIF_POS = NA, 
         HIGH_INF_POS = NA, 
         MOTIF_SCORE_CHANGE = NA, 
         IMPACT = NA,
         PICK = NA,
         VARIANT_CLASS = NA,
         TSL = NA,
         HGVS_OFFSET = NA,
         PHENO = NA,
         MINIMISED = NA,
         ExAC_AF = NA,
         ExAC_AF_Adj = NA,
         ExAC_AF_AFR = NA,
         ExAC_AF_AMR = NA,
         ExAC_AF_EAS = NA,
         ExAC_AF_FIN = NA,
         ExAC_AF_NFE = NA,
         ExAC_AF_OTH = NA,
         ExAC_AF_SAS = NA,
         GENE_PHENO = NA,
         FILTER = NA,
         CONTEXT = NA,
         src_vcf_id = NA,
         tumor_bam_uuid = NA,
         normal_bam_uuid = NA,
         case_id = NA,
         GDC_FILTER = NA,
         COSMIC = NA,
         MC3_Overlap = NA,
         GDC_Validation_Status = NA,
         GDC_Valid_Somatic = NA,
         vcf_region = NA,
         vcf_info = NA,
         vcf_format = NA,
         vcf_tumor_gt = NA,
         vcf_normal_gt = NA,
         tumor_VAF = `VAF (%)`,
         Protein_Change = NA, 
         Outcome = outcome) %>%
  select(!c(ScreeningID, 
            patno, 
            Remissnummer, 
            `Provtagning (Diagnos/Inklusion/Relaps/Uppföljning)`, 
            gene_id, 
            Transkriptnummer, 
            `cDNA förändring`, 
            Proteinförändring, 
            `VAF (%)`, 
            `Bedömning (VUS/Likely Pat/Pat)`, 
            `ddPCR design (assay ID)`, 
            `NGS (Stockholm/Uppsala/Extern)`, 
            Kommentar, 
            ...14, 
            ...15, 
            chr, 
            outcome,
            hugo_id,
            type_of_mutation,
            type_of_variant))

# Export nmds14b_p1_maf and nmds14b_p2_maf as tab separated .maf files
write.table(nmds14b_p1_maf, file='nmds14b_p1_maf.maf', sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
write.table(nmds14b_p2_maf, file='nmds14b_p2_maf.maf', sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
