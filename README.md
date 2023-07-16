# Megastrike

### _Jonathan Bennett <doulos05@gmail.com>_

This is an implementation of Alpha Strike on the computer.

## Installation

This project depends on the McCLIM graphics library, and beast. These will be installed when you quickload the project in Common Lisp. All of these are known to work on MacOS (MacOS does require xQuartz to run as well, which must be configured the section below) and Linux. I lack a means of testing on Windows.

It also requires a functioning Common Lisp installation. The simplest way to have such an installation is to install Roswell, those instructions are below. If you prefer to manage your Common Lisp installation manually, then see the manual install.

### Roswell

[Roswell](https://github.com/roswell/roswell) is a way to manage Common Lisp systems (programs and libraries) on your computer. This page has either pre-built binaries or instructions for how to install it on your computer. Once you have installed Roswell, simply do the following:

1. Open a terminal and run `ros install sbcl`. This will download a good Common Lisp.
2. Run `ros install jonathanabennett/megastrike` This will download Megastrike and all its dependencies.
3. When you want to play, simply open a terminal and type `ros exec megastrike`. This should launch Megastrike on your computer to play.

### Installing Manually

This assumes you have a functional Common Lisp environment with Quicklisp installed.

To run:

1. Clone this repository somewhere ASDF can see it (`~/common-lisp` and `quicklisp/local-projects` are the two most common places).
2. Open Common lisp (run `sbcl`).
3. Run `(ql:quickload :megastrike)`.
4. Run `(megastrike:main)` to launch.

### MacOS

McCLIM, the Graphics library I am using, requires an X Windows system. This means you will need to install [xQuartz](https://www.xquartz.org). See their website for installation instructions. You need to open xQuartz before launching MegaStrike. If it does not open, please follow the following steps:

1. Open xQuartz
2. Go to the xQuartz preferences -> Security and Allow networked connections
3. Open xTerm in xQuartz and run `xhosts +localhost`
4. Restart xQuartz

## What works

Right now (v0.0.1), the game simulates a 2v2 duel between a Lyran Phoenix Hawk-1D and Longbow LGB-0W and a Draconis Combine Locust-1V and Marauder-3R. It displays the units on the screen simply by their designators with a background color matching their team because in the introductory rules, facing is irrelevant.

To play, first click "Roll Initiative" at the bottom. That will generate an initiative list, which shows up on the right side. Next, click "Next phase" until you get to the Movement phase (Phase advancing is not yet automatic if there are no actions to take, and there is a deployment phase in there).

Select a unit on the appropriate side by clicking on them, then click "Move" and then the hex you want to move to. If you cannot reach that hex, nothing will happen. Try again. If your unit moves, then click a unit on the other side and repeat. If a unit is standing still, click them, click move, and then click their hex. When everyone has moved, click "Next Phase"

The Combat phase works the same way. Click a unit on the correct side, click Attack, and then click your target. Check the "Quickstats" blocks on the lower right pane to see if you did damage. Once everyone has fired, click Next Phase twice.

Destroyed units will be removed automatically at the end of the end phase. Roll initiative for your new turn and play on!

There is no terrain, just a flat, featureless plain, though it is only 16x17. There is no "end game", when you're done, click the "Quit Game" button.

Feedback is welcome on the specific layout and how to make it look better, but I'll be focusing almost exclusively on rules and features to begin with.

## What's coming

### 0.1.0

This, the first "functional" release, is a very primitive implementation of the rules, allowing a hot-seat 2v2. The map is a featureless plain. The pilots will all be skill 4. Selecting new units requires programming. Facing will not be calculated. Critical hits will not be applied. Only standard attack types will work. Only walking and jumping will be allowed (nothing but Mechs). Basically, you will be able to move, shoot, and lose armor/structure until your mech dies.

### 0.1.1

Allow you to build your armies before battle. Allow you to deploy. Full implementation of introductory rules for maps and movement.

### 0.1.2

Full implementation of introductory rules for attacks and damage (excluding physical attacks).

### 0.1.3

Implementation of physical attacks, heat, and all specials that are part of the Introductory rule set.

### 0.1.4

Full implementation of the introductory rule set for Alphastrike.

### 0.1.5

Add save games and other cleanup before moving on to implement the Standard rules in the 0.2 series.

## How to Contribute

How you can help depends on how much you know Lisp.

### I'm a keeper of the parentheses, I wield them like the lightning bolts of Zeus.

Awesome! Your help is definitely welcome! This is the largest project I've ever written in Common Lisp and help would be appreciated. We can chat about specifics but areas I know I need help with are:

1. GUI redesign. I've selected McCLIM because it is the one that "clicked" in my brain (AFAIK). But it looks dated and I'm still learning how to do anything more than basic styling.
2. Movement Algorithm. Movement will probably require some variation on Astar or a heat map. I'm an amateur, Astar is really hard for me. I'd love some help implementing it.
3. AI. Eventually, I want to have a bot you can play against. I know a tiny bit about how to implement that, but I'd welcome help.
4. Network. I want this to eventually be like MegaMek, able to be played over the network.. I realize this will require a client-server setup and likely some extensive rewriting (though I've tried to separate things out as best I can to make that easier), but that's all I know.

### I know this Lisp you speak of.

Fantastic, pull requests are welcome to help me with whatever catches your fancy. Particularly, I'd love for someone to help me with the Windows build. I'm able to test builds for MacOS and Linux on my computers but not Windows.

### Common lisp? No, I speak just fine. But I do know my Battletech!

There are a bunch of elements (like... thousands) in the Battletech universe which will need to be created. The README in `data/units` explains the data format, just copy an existing file, rename it, and replace the data as appropriate. Also playtesting is greatly appreciated. Submit issues to Github and I'll do my best to address them, but please expect if it's a rules thing that I'm going to postpone implementation until I get to the point release covering that section of the rules.
