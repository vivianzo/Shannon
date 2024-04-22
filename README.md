# Shannon
Project created for Northeastern's Accelerated Fundamentals of Computer Science I class during the fall 2023 semester. 

# Logistic Information
This game is based on [Claude Shannon](https://en.wikipedia.org/wiki/Claude_Shannon), who is known as the "father of Information Technology". The game was made in racket using the [Universe Library](https://docs.racket-lang.org/teachpack/2htdpuniverse.html#(part._universe)) provided. The main objective of the game is to create a logic board with inputs and outputs as well as goals to win the game. 

There are 3 main pieces in the game, represented as cells: 
* **Conductor Cell**: Propogates the current based on adjacent cells (represented as c in the board)
* **Gates**: Three types: And, Or, Not - propagates the current based on the gate logic and the cells around it based on the indicated direction
* **Input/Output Cells**: Indicates where the games starts and ends

Upon parsing the game, the cells will propagate the charges through the board based on the goals set by the user. The goals show what the input and output should be and once one goal is reached, it is ticked off and the next goal is tested. Once all of the goals are reached the game will show the winning screen. 

# Demo
These are two examples of winnable boards
<p align="center">
  <img src="https://github.com/vivianzo/Shannon/blob/main/shannon%20WS4.gif" alt="Shannon demo WS4" height="400"/>
  <img src="https://github.com/vivianzo/Shannon/blob/main/shannon%20WS5.gif" alt="Shannon demo WS4" height="400"/>
</p>

# Extra Info
Since this project is made for a class, the source code can only be acessed upon request. 


