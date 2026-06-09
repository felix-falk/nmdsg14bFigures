# Day 1
# Day 2
## Creating a repository in GitHub
- Always create a public repository, except in special cases.
- Always add a README file. 
- Put a list of file names in the .gitignore file, which are ignored when cloning
- Add a license, common ones are GNU GPL 2.0 or MIT license.
## Branches
- Use the *main* or *master* branch for the code that works and is tested, then create a branch when adding a new feature / developing your code, known as a feature branch. 
git branch: see which current branch you are on, marked with a *
git branch dev: add the dev branch
git checkout dev: move to the dev branch
git diff: show changes made since the last commit
- Never forget to pull! Do it at the beginning of each day.
git status: run this often
- Commit often! 
- HEAD indicates the commit where you currently are
git pull = git fetch + git merge
- Fetch the changes from remote to local repo. 
- Fork: your own copy of another repository. 
- Issues: 
# Day 3
## Environments
- A configured space where you have access to a set of tools (editors, compilers, libraries)
- To prevent "it works on my machine" problems
- Example 1: in the shell. 
module package: manages module for shell. 
- Scripts = recipe
- Executable = microwave meal
shebang: tell the OS which executable you want to use for this particular file
- Conda: language agnostic environment manager. 
.yaml: yet another markup language. Format often used for configuration files. 
environment.yaml: summarizes current environment parameters in a file
- Choose to either leave packages without specific version indicated. 
	- It is good to put in specific versions, to make the code future-proof. 
	- Loosely versioned: specify the version range of the package. 
	- mrb env export > environment.yml
condaforge: most common packages
bioconda: many bio-specific packages. 
## Reproducible Environments
- Pixi: next gen package dependency and environment manager for the conda ecosystem. 
- Do not include: 
	- Hardcoded paths
	- Credentials and passwords (secrets)
	- Configurations for local machine
- renv: R-specific environment manager. 
- uv: Python-specific environment manager, preferred / recommended over pip nowadays. 
- Indentation in yaml file should be two spaces. 
- mrb env create -f environment.yml
![[Skärmavbild 2026-05-28 kl. 10.36.04.png]]
![[Skärmavbild 2026-05-28 kl. 10.45.26.png]]
![[Skärmavbild 2026-05-28 kl. 10.46.33.png]]

- Should be explained in the repository: 
	- How to create conda environment?
	- How to run the code? 
	- Explain the output, and where you can find it. 
	- How to recreate your result. 
- Only have ONE .yml file! 

![[Skärmavbild 2026-05-28 kl. 11.14.07.png]]

# Day 4
## Virtual Machine (VM)
- Computer simulated within another computer
	- Useful when running OS-specific software on another OS
- Use VM images, which is a complete representation of the VM
	- ISO, OVA, OVF, VMDK, etc.
- Hypervisor; software that interfaces with multiple VMs
	- VirtualBox, VMWare, Hyper-V, Parallels
- Installation
	1. Install a hypervisor (VirtualBox or VMWare are recommended)
	2. FInd and download an image file
	3. Load image in Hypervisor program
	4. Run a setup in the VM
![[Skärmavbild 2026-06-04 kl. 09.31.00.png]]
## Software Containers
- Container
	- Self-contained: packages an app, its configuration and its dependencies all in one standardized unit
	- Distributable: can be put on any system and run
	- Lightweight: much smaller than a VM
- Container engines
	- Provide user interface and orchestration of multiple containers
	- Containers: Docker, Podman, Apptainer
	- Container registries: Docker Hub, GitHub Container Registry, Harbour
## Command Line Arguments
- You should not have to go into the scripts and make changes to them to make them work
![[Skärmavbild 2026-06-04 kl. 10.17.17.png]]

![[Skärmavbild 2026-06-04 kl. 10.19.57.png]]

![[Skärmavbild 2026-06-04 kl. 10.28.37.png]]

# Day 5
# Group Project