# alphastrike

### _Jonathan Bennett <doulos05@gmail.com>_

This is an implementation of Alpha Strike on the computer.

## Installation

This project depends on the McCLIM graphics library, CL-PPCRE, and random-uuid. These will be installed when you quickload the project. All of these are known to work on MacOS and Linux. I lack a means of testing on Windows.

It also requires a functioning Common Lisp installation and Quicklisp. Please see the doucmentation on their websites to install them.

To run:

1. Clone this repository into `~/quicklisp/local-projects/alphastrike`
2. Open Common lisp
3. Run `(ql:quickload :alphastrike)` followed by `(in-package :alphastrike)`
4. Run `(main)` to launch the program.

## What works

Right now (v0.0.1), almost nothing. It can display the stats for a unit (which must be set by modifying the alphastrike.lisp file directly). There is no way to modify the units (applying damage or critical hits). There is only a single map, and it doesn't reflect the terrain in the .board file being loaded. This is largely just a mockup demonstrating what I want the final version to look like.

Feedback is welcome on the specific layout and how to make it look better.

## What's coming

### 0.1.0

The first "functional" release will be a very primative implementation of the rules, allowing a hot-seat style duel between two elements. The map will be a featureless plain. The pilots will all be skill 4. Facing will not be calculated. Critical hits will not function. Only standard attack types will work. No jumping will be allowed. Basically, you will be able to walk, shoot, and lose armor/structure.

### 0.1.1

Full implementation of introductory rules for maps and movement. Allow multiple units per side.

### 0.1.2

Full implementation of introductory rules fro attacks and damage (excluding physical attacks).

### 0.1.3

Implementation of physical attacks, heat, and all specials that are part of the Introductory ruleset.

### 0.1.4

Full implementation of the introductory ruleset for Alphastrike.

### 0.1.5

Add unit images, save games, and other cleanup for moving on to implement the Standard Ruleset in the 0.2 series.

## How to Contribute

How you can help depends on how much you know Lisp.

### I'm a keeper of the parentheses, I wield them like the lightning bolts of Zeus.

Awesome! Your help is definitely welcome! This is the largest project I've ever written in Common Lisp, I feel like I barely know what I'm doing. We can chat about specifics but areas I know I need help with are:

1. GUI redesign. I've selected LTK because it is the easiest to set up on other people's computers (AFAIK). But it looks ugly as sin because I don't know how to style it.
2. Movement Algorithm. Movement will probably require some variation on A\*. I'm an amateur, A\* is really hard for me. I'd love some help implementing it.
3. AI. Eventually, I want to have a bot you can play against. I know a tiny bit about how to implement that, but I'd welcome help.
4. Network. I'd love to have a way to play against someone else over the network. I realize this will require a client-server setup and likely some extensive rewriting (though I've tried to separate things out as best I can to make that easier), but that's all I know.

### I know this Lisp you speak of.

Fantastic, pull requests are welcome to help me with whatever catches your fancy. Particularly, I'd love for someone to help me with the Windows build. I'm able to test builds for MacOS and Linux on my computers but not Windows.

### Common lisp? No, I speak just fine. But I do know my Battletech!

There are a bunch of elements in the Battletech universe which will need to be created. The README in `data/units` explains the data format, just copy an existing file, rename it, and replace the data as appropriate. Also playtesting once this thing is playable would be greatly appreciated.
