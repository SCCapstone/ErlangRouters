#README: SCCapstone - ErlangRouters Project

Team members: Graham Brooker, Paul Melton, Davis Alexander, George Akhvlediani, Joe McDowell

## What's in the Repo?

Within the ErlangRouters repository, you should see:  
* docs       [directory]       - Files and resources that are not affiliated with the source code.
* src        [directory]       - The actual Erlang code for the project.
* .gitignore [text file]    - Sets git ignore properties
* LICENSE.md [md text file] - The license file. 
* Makefile   [text file]    - This is used to call vareity of commands using the source code
* README.md  [md text file] <- The file you are currently reading.  
* rebar.config [text file]  - Configuration for rebar


## Pre-Execution Information
Before running the simulation for the first time, take a look at the following links on our Github Wiki to gain a better understanding of the simulation, as well as the project as a whole:
* The overall architecture of the simulator is explained [here](https://github.com/SCCapstone/ErlangRouters/wiki/Simulator-Architecture).
* The overall project description, with some real-life examples and caveats, is [here](https://github.com/SCCapstone/ErlangRouters/wiki/Project-Description).
* The mathematical description of the simulation, including explainations of the algorithms used to load-balance, is [here](https://www.overleaf.com/read/tqndznjvbtfk).


## What You Need
You will need to download the following before running the simulator. We will assume you are using a Linux-based operating system.

Erlang Runtime Language (download [here](http://erlang.org/download.html))

Rebar:
* Download Rebar and set it up on your machine by typing in the following command:
```
$ wget https://raw.github.com/wiki/rebar/rebar/rebar && chmod u+x rebar
```
* Make Rebar available to somewhere on your shell's PATH (such as /usr/local/bin):
```
$ mv rebar <some-dir-on-PATH>
```

Mozilla Firefox (for the D3 visualization piece)


## Running the Simulator
We will assume you are using a Linux-based operating system. Follow these steps in order to compile and run the .erl modules that our simulator comprises of.

* Step 1: Open a terminal and move to the '../ErlangRouters/' directory that you have cloned locally from the SCCapstone/ErlangRouters Github page. This directory should contain the subdirectories and files listed above.

* Step 2: Type the following command to remove compiled bytecode, log files, etc. from any previous execution of the simulator.
```
$ make distclean 
```

* Step 3: Type the following command to compile the source code, make the directory '../ErlangRouters/ebin/', and place the bytecode that will be executed into the '../ErlangRouters/ebin/' directory.
```
$ make build
```

* Step 4: Move to the '../ErlangRouters/ebin' directory.

* Step 5: Run the simulation by typing in the following command. 
```
$ erl -noshell -s overseer main
```

* Step 6: Follow the prompts for user inputs and wait for the simulation to complete. The simulation is over when the terminal is no longer running the Erlang shell.

* Step 7: After the simulation is completed, you can now take a look at the before/after snapshot of the simulation by opening the file '../ErlangRouters/docs/visualization/D3/StackedBarChartDynamic.html' in Firefox or by running the following command from the '../ErlangRouters/' directory:
```
$ firefox /docs/visualization/D3/StackedBarChartDynamic.html
```
