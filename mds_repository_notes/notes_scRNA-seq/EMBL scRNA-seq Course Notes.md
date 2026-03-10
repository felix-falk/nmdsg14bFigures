# Day 1
- Open invitation to visit EMBL-EBI in Hinxton (contact Piv)
- Data can be submitted to EMBL-EBI, they will help you out with doing so
- Index = tells you the sample
- Cell barcode = tells you the cell
- UMI = tells you the original molecule, helps to correct for amplification biases and distinguish between RNA abundance differences due to either PCR bias or actual difference in the original cell
- Dry lab overview
	1. Map reads to genome
	2. Demultiplex samples using indices
	3. Quality checks
	4. Prepare data for analysis
	5. Find the most variable (informative) genes
		- Note: may have to keep these genes to help with genotyping?
	6. Dimensionality reduction
	7. Identify clusters of similar cells
	8. Plot and interpret results
- Challenges in single cell data analysis / Sources of ambiguity
	- Batch variation
	- Sampling variation
	- Cell size bias
	- Poor RNA capture efficiency
	- Variation in read depth
	- Doublets
	- Housekeeping genes / Background noise
	- Variation between individuals
	- Variation within a cell type
- For detecting genetic variants, you need a lot of reads: sequence 3-5x deeper than normal
- When comparing conditions, you need replicates for both conditions
- For detecting cell-cell interactions, both cell types need to be present in your data
- For detecting gene regulatory networks, you need to sequence deeper to capture lowly expressed transcription factors
- The pilot should evaluate how much washing we can / should do
	- How many cells do we lose in each wash?
	- How does viability change between washes?
- Techniques to avoid confounded experiments
	1. Pool several patient samples, then use genetic variants to post-hoc assign cells back to genetically unique donor: you need genotyping to know which was which
	2. Use antibodies with barcodes for hashing
	3. Preserve cells to be processed together at a later date
- Balanced batch effects
	- Difference across replicates < Difference across conditions
	- Adding hashing and pooling greatly reduces batch effects
# Day 2
- Start with asking: what is the research question?
	- What data is needed to answer the question?
	- Algorithms / Tools will give you answers, but they are not necessarily true: validation is key
- Normalizing data
	- Batch effects create false clusters / DE genes
	- Deconvoluting technical and biological differences between samples in not easy or straightforward
		- Therefore, most methods do not remove batch effects outright, but attempt to model them, to give an estimate of how confident we can be in our conclusion
	- Biological noise sources
		- Cell cycle
			- Regress out
			- All differences associated will also be removed
			- May be interesting to keep in some cases, such as cancer
		- Genetic background
		- Age
		- Circadian rhythm
		- Cell stress
			- Cell type / condition dependent
			- Difficult to regress
			- Typically, exclude affected cells / genes
		- Individual variation
			- Is often kept in, such as healthy vs diseased patients
			- Solved by doing multiple replicates and patients
	- Solutions
		- Regress / Correct / Normalize
		- Exclude affected cells
		- Include batch effects as covariate (most common solution)
		- Ignore and hope that biology of interest has a stronger signal
		- Imputations
			- Not recommended! Will skew data
- Clustering vs Pseudotime
	- Do not publish both, describe either clusters OR pseudotime in your data set
- Clustering assumptions
	- Clusters are roughly the same size
		- Will lead to arbitrary splitting of large clusters
	- Clusters are of similar density
		- Sparse clusters are split up into several clusters
		- Common with immune cells
	- Clusters have a fixed resolutions
- Adjust clustering parameters to achieve different outcomes
- To figure out if clusters are real, check: 
	1. Robustness of clusters when changing parameters
	2. Differentially expressed genes between clusters
	3. Known marker genes, annotations
	4. Quality statistics
		1. Silhouette index
	5. Consistency of clusters across replicates / reference data
		1. Using eg. scmap
	6. Validation experiments
		1. "Looking good" is NOT a good validation method
- Pseudotime assumptions
	- All cells exist on a smooth continuum
	- The continuum has a line / curve / tree shape
- RNA velocity looks at spliced (older) vs unspliced (newer) transcripts
- Visualisation
	- Dimensional reduction
	- UMAP, tSNE
	- PCA: preserves distances between points
	- UMAP: overall data structure
	- tSNE: Distinct clusters
	- Use all on your data to better understand it
- Differential Expression
	- Assumptions
		- Changes are linear
		- Noise follows a specific distribution (negative binomial)
			- The distribution changes after batch correction, and may become normally distributed
	- Nonparameteric tests
		- Assumes changes are monotonic
		- Independent of data distribution
		- Do NOT account for confounders, unlike linear models
		- Wilcoxon is generally the best nonparamteric statistical test! 
	- DE across multiple batches, conditions
		- Subset data to single cell type, then perform DE test
			- General linear model
# Day 3
# Day 4
# Day 5