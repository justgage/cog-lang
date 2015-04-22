<!--
# Ideas
- Easy for people to pick up!
- Built in tutorial
   - Error messages encourage you
- All in one package, easy install and run. (JVM? make that impossible?)
- Kids and people interested in learning more
- Auto formating for easier to read code
- []'s for blocks
- More English type words (Jessica approved)
   - "Box" for a variable
   - Action -> a function
- Smallest amount of syntax that's easy to understand how it's parsed
- Built in GUI
- Make writing to a file EASY
- Make string manipulation easy
   - "WORDS" auto breaks on spaces
   - "LETTERS" -> list of characters
   - EACH letters-of a DO: [...]
- Etc...

# Names?
- First lang (FL)
- Easy
- Make mistakes
- Have fun
- Game lang
- etc...

 START OF THE REAL THING!!!! --!>

# Senior Project proposal

Gage Peterson 

Wed Apr 22 2015


# Scripture

> Mormon 9:34
> 
> "34 But the Lord knoweth the things which we have written, and also that none other people knoweth our language; and because that none other people knoweth our language, therefore he hath prepared means for the interpretation thereof."

# Abstract

__The focus of this project is__ to learn how to parse and execute a programing language using a lexer, parser and abstract syntax tree which will help deepen my understanding of how programing languages go from code to execution. 

__The application focus is__ to make a simple, easy to use programing language for people first learning to program. 

# Background

## Definitions

_Abstract syntax tree (AST)_

An abstract syntax tree (AST) is a tree representation of the abstract syntactic structure of source code written in a programming language. 

_Executer_

This is the part that turns the AST into action. Action meaning making the computer do what the AST said to do based on the rules of the language. 

_Tokenizer_

Breaks a string of characters into "tokens" or the pieces of the programing language (if block, a function, etc...)

_Parser_

The part of the program that turns the tokens into an AST.

_Spec_

Stands for "requirements specification" which will be a document describing what my programing language is required to preform.


## Why this topic is of interest

Programing is a very useful skill but the barrier to entry is quite high for beginners. For this reason I propose to create a simple programing language with a visual representation of the internals. Providing a more visual way to see what is going on at each stage of the execution helps beginners find bugs as well as understand what the program is actually doing. This is interesting because many people, if taught to code, could solve many useful problems that otherwise would not of been possible for them.

## Prior work by others
- <http://sonic-pi.net> a music creation programing environment.
- [DrRacket](http://docs.racket-lang.org/drracket/index.html) programing with pictures. 
- [GameMaker](https://www.yoyogames.com/studio) a programing environment that I learned to program in. 
- Python's editor for windows
- [Scratch](http://scratch.mit.edu/) a Lego based programing language.
- Codeacademy / Codeschool (both online tutorials that take you step-by-step through programing)
- [Daisy the Dinosaur](https://itunes.apple.com/us/app/daisy-the-dinosaur/id490514278) an app for young children to learn to program on an I-pad.
- [Khan Academy Computer science courses](https://www.khanacademy.org/cs).
- [CODE.ORG](http://code.org/) has many basic interactive programing tutorials aimed at children.
- etc... etc...

## Prior work by you
- Brainstorming for the language syntax and other language ideas.
- Research in how various languages other languages work
- Research into the high level concepts of a compiler

# Description

The project consists of making a simple, easy to use programing language focused at people who are first learning to program. 

1. Preliminary research and proposal preparation
2. Research
3. Requirements specification
4. Design
5. Writing behavior driven tests.
6. Writing the Code to preform the given behaviors previously specified.

## What is success? 

The following is considered a "success":

- The code from the spec parses
- The code from the spec runs
- The code from the spec has the required output for any of the input examples from the spec

# Scope

## What is included
- A simple executable programing language which will have the following features
   - Variables which can hold values and be replaced with other values
   - Basic math operators including addition, subtraction, division and multiplication.
   - Lists
   <!-- 
   - Maps 
   --!>
   - A basic way to repeat blocks of code a number of times
   - Functions
      - Defining them
      - Running them
      - Returning values from them
      
- A simple view of the output in a separate screen
- A visual way to view the internals variables

## What is not
- Many language features including but not limited to:
   - Classes
   - Pointers
   - Very good error messages (hopefully it will though)
   - A compiler (it will only run the code)

# Tasks and Schedule

1. Preliminary research and proposal preparation
    - __Estimated Hours till completion:__ 8
    - __Start Date:__ Apr 20 
    - __Stop Date:__  Apr 30 

2. Research
    - __Estimated Hours till completion:__ 16
    - __Start Date:__ May 1 
    - __Stop Date:__ May 15 

3. Requirements Specification document
    - __Estimated Hours till completion:__ 31
    - __Start Date:__ May 16
    - __Stop Date:__  June 15

4. Design
    - __Estimated Hours till completion:__ 39
    - __Start Date:__ June 16
    - __Stop Date:__ July 16

5. Writing behavior driven tests.
    - __Estimated Hours till completion:__ 16
    - __Start Date:__ July 17
    - __Stop Date:__ July 31

6. Writing the code to preform the given behaviors previously specified.
    - __Estimated Hours till completion:__ 47
    - __Start Date:__ Aug 1
    - __Stop Date:__ Sep 12


__Total Hours:__ 156

# Deliverables

An executable that can interpret and execute the code written in a plain text file according to the spec.

# Applicability

This project helps me deepen my understanding of how the code that I write is actually doing something, what process it has to go through to reach execution. This would expand on many of the topics taught in CS 124 where we learned about the low level constructs of programing. Also there is a lot of knowledge I will be able to utilize from CS 238 when we talked about languages and grammar which have to be defined in any programing language. It is outside of the curriculum because there is no class that focuses on constructing programing languages.

# Required Resources with Costs

There is no requirements with costs.

# References

- <http://en.wikipedia.org/wiki/Abstract_syntax_tree>

Also all the things listed in "prior work by others"

