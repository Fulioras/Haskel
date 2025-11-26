# fp-2025 - AUGUSTINAS BIČKAITIS

# DOMAIN - TASK MANAGER

Can be viewed [here](https://vipo.github.io/fp-2025/)

Main commannds
- 'add' - adds a new task or a subtask to a task
- 'remove' - removes a task/subtask
- 'complete' - marks a task as complete (can recusively mark as completed on tasks and subtasks)
- 'list' - list all completed and uncompleted tasks
- 'report' - shows task list metrics



Command examples:
	newTask 'T1' 'clean the bathroom'
	AddSubtask 'T1.1' 'clean the sink' 'T1'
	AddSubtask 'T1.1.1' 'clean the faucet' 'T1.1'
	Complete 'T1.1' true   --recursive
	ListTasks
	ReportProgress
	DumpExamples

BNF form

<command> ::= <task>             
            | "dump examples"      

<task> ::= "addTask" <string> <string>						   

		 | "addSubtask" <string> <string> <string>     

         | "removeTask" <string>									   

         | "list"   														   

         | "completeTask" <string>  <boolean>   						   

         | "reportProgress"

<string> ::= ([a-z] | [1-9])+

<boolean> ::= "true" | "false"

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`
