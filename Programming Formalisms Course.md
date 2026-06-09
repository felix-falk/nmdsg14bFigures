# Course / Project Introduction
- Richél
- Evidence-based best practices? 
	- Avoid nesting for-loops / if-statements
- Teaching methods: cold calls. Let everyone think about the question for 15 seconds, then call a random name to give an answer. 
- Pace: let everyone do an exercise, and signal somehow when they are done, using a ZOOM emote for example. Move on when 2/3 are done. 
- GitHub: sharing of code. 
- Repository: Dedicated folder. 
- GitHub Issue: To-do item. 
- Markdown: A mark-up language that describes what text should look like. 
- Markdown file: a .md file that uses the markdown language. 
- Rendered file: a markdown file that has been "rendered". 
# Design of a research project
- Start with a research question / hypothesis. 
- Design experiments to test the hypothesis. 
- Evaluate the results to accept or reject the hypothesis. 
- "Registered Report"
	- Starting point for scientists
	- A document created before starting your research project, explaining the hypotheses, methodology and expected results. 
	- The document is published together with the final research project. 
- The script vs package
	- Its more formal and user friendly
## Software development life cycle
- Lars Eklund
- The 5 phases of software development life cycle
- Verification and validation
	- Validation: is the program fulfilling the needs of the users? 
	- Quality: decide on the level of quality beforehand. 
	- Requirement: what a software needs to achieve and how. 
	- Software requirement specification: "well-formed requirement"
- Phase 1: Requirement gathering and analysis
- Project brief: write down a short summary of the software, its purpose and aims. 
	- Create an user story: write down how the user uses the software. 
## Risk
- "The likelihood of a negative event"
- [Link](https://uppmax.github.io/programming_formalisms/sessions/project_start/analysis_design/Risk/)
- Business risk: direct requirement
- Technical risk: derived requirement
- Risk: probability of event X severity of event. 
# Day 2
## Software Development Life Cycle
- [Link](https://uppmax.github.io/programming_formalisms/sessions/lifecycle/)
- A software development life cycle is a method for software development that involves different phases, and iteration across several cycles
- Scrum: a type of software development life cycle
- Agile: 
## Version Control
- [Link](https://uppmax.github.io/programming_formalisms/sessions/version_control/)
- Version control: record keeping of changes to your software. 
- Version control system: software that allows you to perform version control, eg. GitHub. 
- Commit: adding your version of the code to the version control system. 
- Commit hash: number in the version control system. 
- When should you commit: every couple of minutes. 
- Allows you to see the history of your files, undo mistakes and work together. 
## Integrated Development Environment
- eg. VScode
- 

## Merge conflicts
- When the version control system is asked to merge contradictory commits. 
- Commit early and often, and limit the amount of characters per line to avoid conflicts. 

## Design introduction
- [Link](https://uppmax.github.io/programming_formalisms/sessions/design_introduction/#exercise-2-our-first-setup)

## Function design
- [Link](https://uppmax.github.io/programming_formalisms/sessions/function_design/)
- A function should be brief and only do one thing
- Decompose programs into functions, to make the problem simpler
## Development introduction
- [Link]([https://uppmax.github.io/programming_formalisms/sessions/development_introduction/](https://uppmax.github.io/programming_formalisms/sessions/development_introduction/))
- The biggest source of errors is: mistmatch between what you assume that the code does, or what the code actually does. Even if an output is produced
## Assert
- [Link](https://uppmax.github.io/programming_formalisms/sessions/assert/)
- Assert: a very commonly used function. 
- Use it to make all assumptions explicit. 
- If your code fails, and the error message is "assert failed", congrats, you are a great programmer!
# Day 3
## Test-driven development
- [Link](https://uppmax.github.io/programming_formalisms/sessions/tdd/)
- A systematic way to grow code. 
- Red phase: write a test that fails, using assert. 
	- Example tests: 
		- assert is_zero.__doc__ Does the function have documentation?
		- asssert is_zero Does the funciton exist? 
		- assert is_zero(0) == True
- Green phase: rework the code to make the test pass. 
- Blue / Refactor phase: cleanup the code and push it. 
- Rules: only write code to resolve the error, no more. 
- Delete tests that don't break the code, keep tests that do break the code. 
- Use try / exception in your code!!!
	- has_thrown == False
# Day 4
## Branches
- The main branch: code that always work
- Develop branch: branch being developed
- Other / feature branch: named after developer or topic, specific to one topic / feature
## Continuous Integration
- [Link](https://uppmax.github.io/programming_formalisms/sessions/continuous_integration/)
- Use GitHub actions, which are tests that are run on the code. 
## Modules
- [Link](https://uppmax.github.io/programming_formalisms/sessions/modularity/)
- A file containing code
- Code coverage: 100% of code should be tested (Richél purist viewpoint)
- 
