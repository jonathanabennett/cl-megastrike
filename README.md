# Megastrike

### _Jonathan Bennett <doulos05@gmail.com>_

This is an implementation of Alpha Strike on the computer.

## Installation

This project depends on the McCLIM graphics library, and beast. These will be installed when you quickload the project in Common Lisp. All of these are known to work on MacOS (MacOS does require xQuartz to run as well, which must be configured the section below) and Linux. I lack a means of testing on Windows.

It also requires a functioning Common Lisp installation (I use [sbcl](http://www.sbcl.org "SBCL")) and [Quicklisp](https://www.quicklisp.org/beta/ "Quicklisp"). Please see the documentation on their websites to install them.

To run:

1. Clone this repository into `~/quicklisp/local-projects/megastrike`
2. Open Common lisp (run `sbcl`)
3. Run `(ql:quickload :megastrike)`
4. Run `(in-package :megastrike)`
5. Run `(main)` to launch the program.

## What works

Right now (v0.0.1), the game simulates a 1v1 duel between a Phoenix Hawk-1D and a Locust-1V (the locust is crazy). It displays the two units on the screen simply by their designator (PXH-1D and LCT-1V respectively) because in the introductory rules, facing is irrelevant.

The game has no concept (yet) of initiative order or phases, so those have to be handled by you. You can type Roll in the bottom to roll your initiative (type it twice for two rolls).

To move a unit, type Move, then a space, then click on the unit, then type a space, then click on the destination. If the unit doesn't move, add another space. If it still doesn't move, that's because it can't move that far. Try a closer hex.

To attack a unit, type Attack, then a space, then click on the attacker, then type a space, then click on the target. If you see a new `Command:` prompt, the attack was unsuccessful. Otherwise, you'll see damage on the unit's info card to the right.

There is no terrain, just a flat, featureless plain, though it is only 16x17. There is no "end game", when you're done, type quit and enter.

Feedback is welcome on the specific layout and how to make it look better, but I'll be focusing almost exclusively on rules and features to begin with.

## What's coming

### 0.1.0

This, the first "functional" release is a very primitive implementation of the rules, allowing a hot-seat style duel between two elements. The map will be a featureless plain. The pilots will all be skill 4. Facing will not be calculated. Critical hits will not function. Only standard attack types will work. No jumping will be allowed. Basically, you will be able to walk, shoot, and lose armor/structure.

### 0.1.1

Full implementation of introductory rules for maps and movement, and initiative. Allow multiple units per side.

### 0.1.2

Full implementation of introductory rules for attacks and damage (excluding physical attacks).

### 0.1.3

Implementation of physical attacks, heat, and all specials that are part of the Introductory rule set.

### 0.1.4

Full implementation of the introductory rule set for Alphastrike.

### 0.1.5

Draw unit images, add save games, and other cleanup before moving on to implement the Standard rules in the 0.2 series.

## How to Contribute

How you can help depends on how much you know Lisp.

### I'm a keeper of the parentheses, I wield them like the lightning bolts of Zeus.

Awesome! Your help is definitely welcome! This is the largest project I've ever written in Common Lisp, I feel like I barely know what I'm doing. We can chat about specifics but areas I know I need help with are:

1. GUI redesign. I've selected McCLIM because it is the one that "clicked" in my brain (AFAIK). But it looks ugly as sin because I don't know how to style anything.
2. Movement Algorithm. Movement will probably require some variation on Astar or a heat map. I'm an amateur, Astar is really hard for me. I'd love some help implementing it.
3. AI. Eventually, I want to have a bot you can play against. I know a tiny bit about how to implement that, but I'd welcome help.
4. Network. I'd love to have a way to play against someone else over the network. I realize this will require a client-server setup and likely some extensive rewriting (though I've tried to separate things out as best I can to make that easier), but that's all I know.

### I know this Lisp you speak of.

Fantastic, pull requests are welcome to help me with whatever catches your fancy. Particularly, I'd love for someone to help me with the Windows build. I'm able to test builds for MacOS and Linux on my computers but not Windows.

### Common lisp? No, I speak just fine. But I do know my Battletech!

There are a bunch of elements in the Battletech universe which will need to be created. The README in `data/units` explains the data format, just copy an existing file, rename it, and replace the data as appropriate. Also playtesting is greatly appreciated. Submit issues to Github and I'll do my best to address them, but please expect if it's a rules thing that I'm going to postpone implementation until I get to the point release covering that section of the rules.
