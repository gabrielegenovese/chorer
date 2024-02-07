# Title
Chorer: a static analyser to generate Choreography Automata

## Abstract
One of the main features of Erlang is that it enables easy communication between processes.
Choreographies are a mathematical model to describe the message-passing communications among
two or more processes. Choreography Automata are a way to graphically present a Choreography
as a finite state automaton. This representation can be used both for program understanding
and to support automatic analysis of behavioral properties, such as deadlock freedom. This
talk will present Chorer, a static analysis prototype which takes as input an Erlang source
code and ganerates a Choreography Automaton which is an over-approximation of all its possible
interactions. The talk will focus on how Chorer works, how to interpret its output, and the
basic ideas underlying its construction.

## Talk objective: What will the audience learn from your talk?
The audience will learn what are choreographies and how Chorer allows one to exploit them for
understanding and analysis of Erlang program. 

## Target audience: Who would this talk appeal to?
Erlang programmers

### Which three words would best sum up your talk?
choreographies, static-analysis, concurrency


### Speaker tagline
Master student in Computer Science at University of Bologna

### Speaker biography
I'm a Master student in Computer science at Alma Mater Studiorum - University of Bologna.
I gradueted with a thesis focused on choreographies for Erlang, supervised by Prof. Ivan Lanese.
Into languages and distributed systems.
