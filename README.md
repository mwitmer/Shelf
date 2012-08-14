# Shelf: A Prototype-Based Object System for Guile

## What is Shelf?

Shelf is a lightweight implementation of prototypes for Guile
Scheme. 

## What does it do?

Shelf lets you define simple objects containing key/value pairs. These
objects can use another object as a *prototype*. Objects have all the
properties of their prototype, plus whatever key/value pairs they
choose to add or override.

Shelf lets you create methods as object properties with familiar
'this' and 'super' bindings. Think Javascript but with the clean
syntax and power of Scheme.

Saving and loading with Shelf is easy---Shelf objects can be written
out as Scheme syntax and then run back through interpreter.

Shelf also lets you compile your objects to object code and then back
to Scheme whenever you like.

## Why did you write this?

I was writing a roguelike game that involved lots of monsters, items,
and map features that each have a lot of properties. I wanted to be
able to add new properties to objects without the trouble of modifying
class definitions all the time.

First I tried to write the game in C and add a little config language
with a Flex/Bison lexer/parser for defining and saving objects. That
worked all right but I got frustrated that I couldn't easily include
functions in it. I didn't want to write a whole programming language
from scratch!

I had some experience with Javascript and I liked first-class
functions and prototype object models. But I didn't like much anything
else about Javascript. There are lots of nice languages that do have
prototypes, like Self, Lua, and Tcl, but I decided it would be fun to
actually create a prototype system of my own instead.

With its powerful language-extending macros, Scheme is the perfect
language for that sort of thing. I picked Guile as my implementation
because I already had some experience writing scripts with it to extend
the musical score editor Lilypond.

## Future Ideas

Loading a really big Shelf object is slow. That's partly because
defining all of an object's properties inside of a closure created at
run-time seems to defeat many of Guile's attempts at optimization.
There is some exciting work going on right now to bring a faster VM
and JIT compilation to Guile. I think that will help speed Shelf up a
lot. I should probably look for some optimizations on my own, too.

Guile ships with a really neat and powerful object system called
GOOPS. It's *so* powerful, in fact, I'm fairly sure you can make it do
what Shelf does. But I wanted to do this from scratch. Maybe I'll look
for some ways to integrate the two in my next version.

Though this release contains several Guile-specific features, the
basic behavior of Shelf is Scheme-implementation independent. In other
words, you could pretty easily take out all the Guile stuff and port
this to another Scheme. The only extra features absolutely required
are hashtables and procedures with setters. I made a Chicken Scheme port
of Shelf a while back, though it's not very up to date at the moment.

Oh yeah, maybe some time I'll finish and release the game that
inspired all this.