# Shannon
This project was developed as part of Northeastern's Accelerated Fundamentals of Computer Science I class during the fall semester of 2023.

# Logistical Information
The game was made in racket using the [Universe Library](https://docs.racket-lang.org/teachpack/2htdpuniverse.html#(part._universe)) provided. 

This game is based on [Claude Shannon](https://en.wikipedia.org/wiki/Claude_Shannon), who is known as the "Father of Information Technology". His study of boolean algebra and its applications in electrical engineering was showcased in this game. The main objective is to create a logic board with inputs and outputs as well as goals to win the game as there are cells within the grid that function similarly to an electrical circuit board. 

There are 4 main pieces in the game, represented as cells: 
* **Conductor Cell**: Propogates the current based on adjacent cells (represented as c in the board).
* **Gates**: Three types: And, Or, Not - propagates the current based on the gate logic and the cells around it based on the indicated direction.
* **Input/Output Cells**: Indicates where the game starts and ends.
* **Empty Cell**: These are the blank boxes, it is not possible for current to propagate to this area.

Upon parsing the game, the cells will propagate the charges through the board based on the goals set by the user. The goals show what the input and output should be and once one goal is reached, it is ticked off and the next goal is tested. Once all of the goals are reached the game will show the winning screen. 

# Demo 
<p align="center">
  <img src="https://github.com/vivianzo/Shannon/blob/main/shannon%20WS4.gif" alt="Shannon demo WS4" height="400"/>
</p>

<p align="center">
  <img src="https://github.com/vivianzo/Shannon/blob/main/shannon%20WS5.gif" alt="Shannon demo WS4" height="400"/>
</p>

# Extra Info
To learn more about this assignment, refer to the [original directions](https://github.com/vivianzo/Shannon/blob/main/Shannon%20Instructons.pdf). 


