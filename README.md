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

This assumes you have a functional Common Lisp environment (I recommend [SBCL](http://www.sbcl.org)) with [Quicklisp](https://www.quicklisp.org/beta/) installed.

To run:

1. Clone this repository somewhere ASDF can see it (`~/common-lisp` and `~/quicklisp/local-projects` are the two most common places).
2. Open Common lisp (run `sbcl` from a terminal).
3. Run `(ql:quickload :megastrike)`.
4. Run `(megastrike:main)` to launch.

### MacOS

McCLIM, the Graphics library I am using, requires an X Windows system. This means you will need to install [xQuartz](https://www.xquartz.org). See their website for installation instructions. You should launch Megastrike _from an xQuartz terminal_, not a regular MacOS terminal. If it isn't working, try the steps below.

1. Open xQuartz
2. Go to the xQuartz preferences -> Security and Allow networked connections
3. Open xTerm in xQuartz and run `xhosts +localhost`
4. Restart xQuartz

## What works

Right now (v0.1.1), the game allows you to simulate combat between armies of any size, but those armies can only consist of Phoenix Hawk-1Ds, several variants of the Locust, Marauder-3Rs, and Longbow-0Ws. The combat happens on a featureless plain. It displays the units on the screen simply by their designators with a background color matching their team because in the introductory rules, facing is irrelevant.

## How to Play

### Lobby Screen

When you first launch the game, it will launch into a lobby screen. In this lobby screen, you will see 3 window spaces. These windows are the Unit Selection window, the Game Setup window, and the Army Unit List window.

![Lobby View](https://github.com/jonathanabennett/megastrike/blob/main/documentation/lobby.png)

#### Adding Armies

First, replace AFFS with the name of your army, then click the color you want your army to be. Once finished, click "New Army" and you should see it appear in the list above you.

**NOTE: The game currenly only supports 2 armies in a head to head, do not add more than two armies.**

Once you have added an army, you can select it by clicking on its name.

#### Adding Units

To add a unit, you need to have an army selected. Enter the pilot data and the starting hex number. Then click on the mek you want to assign to that pilot and click the Add Unit button. You should see the unit appear in the Army Unit List window to the bottom right.

You can add as many units as you want, but please be sure to enter a new starting hex number for each unit. Deploying during the deployment phase should come in the next release.

#### Setting Up the Map

Finally, initialize the map by entering the width and height of the map (the default is 16x17). Once you are happy with the map size, click "Update Map Size".

#### Ready to Play

If you have completed all of these steps, you will see the "Game Not Ready" button change to "Game Ready". Click it to begin playing.

### Initiative Phase

To roll initiative, click "Roll Initiative" at the bottom. That will generate an initiative list, which shows up on the right side. Click "Next phase" until you get to the Movement phase (Phase advancing is not yet automatic if there are no actions to take, and there is a deployment phase in there).

### Deployment Phase

The deployment phase is not yet implemented. Simply click "Next Phase"

### Movement Phase

Select a unit on the appropriate side by clicking on them, then click "Move" and then the hex you want to move to. If you cannot reach that hex, nothing will happen. Try again. If your unit moves, then click a unit on the other side and repeat. If a unit is standing still, click them, click move, and then click their hex. When everyone has moved, click "Next Phase"

### Combat Phase

The Combat phase works the same way. Click a unit on the correct side, click Attack, and then click your target. Check the "Quickstats" blocks on the lower right pane to see if you did damage. Once everyone has fired, click Next Phase.

### End Phase

Destroyed units will be removed automatically at the end of the end phase. Click "Next Phase" to start the next round.

### Next round

Roll initiative for your new turn and play on! Play continues until one side is destroyed. When you are finished, click "Quit Game" to exit.

## What's coming

### 0.1.0

This, the first "functional" release, is a very primitive implementation of the rules, allowing a hot-seat 2v2. The map is a featureless plain. The pilots will all be skill 4. Selecting new units requires programming. Facing will not be calculated. Critical hits will not be applied. Only standard attack types will work. Only walking and jumping will be allowed (nothing but Mechs). Basically, you will be able to move, shoot, and lose armor/structure until your mech dies.

### 1.1.0 This release

Allow you to build your armies before battle. Deployment happens via entering the locations (as the instructions above), expect this to change very quickly.

### 1.2.0

Full implementation of introductory rules for maps and movement.

### 1.3.0

Full implementation of introductory rules for attacks and damage (excluding physical attacks).

### 1.4.0

Implementation of physical attacks, heat, and all specials that are part of the Introductory rule set.

### 1.5.0

Full implementation of the introductory rule set for Alphastrike.

### 1.6.0

Add save games and other cleanup before moving on to implement the Standard rules for the 2.0 series.

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

# Credits

The images found in the data/images folder are used with permissino from Megamek [Megamek](https://github.com/MegaMek/megamek).

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

_MechWarrior, BattleMech, ‘Mech and AeroTech are registered trademarks of The Topps Company, Inc. Original BattleTech material Copyright by Catalyst Game Labs All Rights Reserved. Used without permission._
