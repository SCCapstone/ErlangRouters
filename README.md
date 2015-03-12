#README: SCCapstone - ErlangRouters Project

Lasted edited: 2/18/2015  
Team members: Graham Booker, Paul Melton, Davis Alexander, George Akhvlediani, Joe McDowell

## What's in the Repo?

If you are reading this, then you should have downloaded the ErlangRouters Repository.  
Within this repository you should see:  
* docs       [folder]       - Files and resources that are not affiliated with the source code.
* src        [folder]       - The actual Erlang code for the project.
* .gitignore [text file]    - Sets git ignore properties
* LICENSE.md [md text file] - The license file. 
* Makefile   [text file]    - This is used to call vareity of commands using the source code
* README.md  [md text file] <- The file you are currently reading.  
* rebar.config [text file]  - Configuration for renar

## Executing .erl modules
We will assume you are using a linux-based operating system. Follow these steps in order to compile and run .erl files, or modules. 
* Step 1: Instal Erlang on the system you intend to use.

* Step 2: Open a terminal and navigate to */ErangRouters/src/

* Step 3: To initiate the Erlang console, simply type 'erl' {without the quotes}

* Step 4: From the Erlang console you can enter the command 'c(modulename).' {no quotes} 
to compile the corresponding .erl module file. Make sure you only enter the module name and include a period at the end, 
as so: 'c(listops).' {this command will compile the listops.erl file}

* Step 5: You can call any exported function from the compiled module by entering 'modulename:function(parameters),' {no quotes}. This command will call the function exported from the module with the specified parameters. Example: 'listops:max_value([1,2,2,18,0])' will return '18'. The module name is 'listops', and the exported function is 'max_value/1', the '/1' indicates the number of parameters 'max_value' must receive, which is just a single list in this case '[1,2,2,18,0]'. 
