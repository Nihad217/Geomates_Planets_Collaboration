# geomates
This is a clone of the 'geometry friends' competition that was run at IJCAI for several years. This 2D physical simulation game is intended to challenge autonomous agents in a multi-agent setting.

## Gameplay
Two players -- disc and rectangle -- jointly need to collect diamonds in a 2d platform environment. Players are controlled by one autonomous agent (or human). The disc player has the ability to jump up, while the rectangle can shape its shape by adjusting its height/width ratio. Both players can move left and right and are subject to gravity. Additionally, agents can send messages to the other agents (see below).

Levels are defined in levels.lisp, see there for how to design your own levels.

## Principle of Operation and Control
The game is implemented as a server that connects to agents and the GUI using TCP/IP sockets, sending scene information as s-expressions. The GUI is written in JavaScript and should run in any modern web browser.
For convenience of testing, you can connect to the server by telnet to control an agent. See documentation inside geomates.lisp for all the glory details.

Agents are controlled via key codes for a (left), d (right), w (jump, disc only), s (widen shape, rect only), w (grow shape, rect only). Additionally, m(...) can be used to send a message (...) which must be a valid s-expression (i.e., matching paranthesis) to the other agent. See the comments in geomates.lisp for all the glory details.

To make the game controllable by Telnet, telnet has to be switched into byte mode such that keypresses are send immediately. The game server will do so automatically, causing some protocol traffic the gameserver will however treat as control attempts. This is why telnet clients will receive lots of responses, although no key has been pressed.  

## Requirements
The game requires a 3.x version of the box2d library to be installed. As of 2025, 2.x versions are still widely shipped with package management systems. These are incompatible with 3.x versions and will not work! Therefore, [download the original](https://github.com/erincatto/box2d) repository and build the library yourself. In case your package manager provides a 3.x library, you may of course use that.

Additionally, [SBCL](https://sbcl.org) as LISP compiler is required. If you have ACT-R installed, you probably already have SBCL.

Alternatively, you may use the provided Dockerfile to run the game in a virtual environment (tested only on machines with x86 processor).

### Docker
Download [Docker](https://www.docker.com) and use the provided Dockerfile to build the game environment by <pre><code>docker build -t geomates:latest .</code></pre> (this may take a while, but is only required once). Then use <pre><code>docker run -p 8000:8000 -p 45678:45678 geomates:latest sbcl --script geomates.lisp</code></pre> to run the game. Note that you cannot provide own levels or change game parameters unless you modify the Docker container.

### Windows using MSVC
Follow the instructions for building below but use the Windows specific Makefile provided for building the library (thanks, Jendrik!). Note that you must adopt the path names inside the Makefile!

Note that Telnet is usually shipped with Windows, but it requires extra steps to activate it.

### Linux
Just follow the instructions for building below. In case you don't have clang installed, change the compiler to gcc in the Makefile.

### MacOS
Just follow the instructions for building below. Make sure to have developer tools installed. You can install telnet using [homebrew](https://brew.sh): <pre><code>brew install telnet</code></pre>

## Building
Only a single dynamic liberary needs to be build that wraps around box2d's static library. To do so, edit the Makefile to adjust the paths to where box2s include files and the static library can be found (box2d does not need to be installed system-wide).

## Running the game
```sbcl --script geomates.lisp```

Then, open viewer.html in a web browser and start your agents. Once both agents have connected, the game starts. It ends when all levels have been played. The list of levels is loaded from levels.lisp.

## Connecting with ACT-R

Assuming [ACT-R Sources](http://act-r.psy.cmu.edu/actr7.x/actr7.x.zip)  (actr7.x) are [compiled](http://act-r.psy.cmu.edu/actr7.x/QuickStart.txt) and in the same folder as the geomates folder run:

Start Geomates i.e. ``docker run -p 8000:8000 -p 45678:45678 geomates:latest sbcl --script geomates.lisp``

```
sbcl --load "actr7.x/load-act-r.lisp" --load "geomates/act-r-experiment.lisp" --eval '(load-act-r-model "geomates/model-dummy.lisp")'
```

to start the agent evaluate `(geomates-experiment)` in the ACT-R REPL to get into the ACT-R GUI Environemnt `(run-environment)`.


## Author and License 

The game is distributed as open source software as is. Author is Diedrich Wolter, address all requests to him. Geomates uses two Lisp packages provided under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) for open source software: [base64](https://github.com/massung/base64) and [sha1](https://github.com/massung/sha1).
