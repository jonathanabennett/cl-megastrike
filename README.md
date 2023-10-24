# Megastrike

### _Jonathan Bennett <doulos05@gmail.com>_

This is an implementation of Alpha Strike on the computer.

## Installation

This project depends on GTK4. This must be installed on your system before you try to build this from source. All other dependencies will be installed for you via quicklisp. All of these are known to work on MacOS and Linux. I lack a means of testing on Windows.

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
3. Run `(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)`
4. Run `(ql:update-all-dists)` to ensure you are using the latest versions of the libraries.
3. Run `(ql:quickload :megastrike)`.
4. Run `(megastrike:megastrike)` to launch.

## What works

Right now (v1.3.0), the game allows you to simulate combat between armies of any size using units exported from Megamek via their AlphaStrike stat generator. While the game won't stop you from using them, note that *FLYING UNITS DO NOT FLY* and none of the rules for them have been implemented yet. The combat happens on a featureless plain.

## How to Play

### Lobby Screen

When you first launch the game, it will launch into a lobby screen. In this lobby screen, you will see 3 window spaces. These windows are the Unit Selection window, the Game Setup window, and the Army Unit List window.

![Lobby View](https://github.com/jonathanabennett/megastrike/blob/main/documentation/lobby.png)

#### Adding Armies

First, replace AFFS with the name of your army, then click the color you want your army to be. Once finished, click "New Army" and you should see it appear in the list above you.

**NOTE: The game currenly only supports 2 armies in a head to head, do not add more than two armies.**

Once you have added an army, you can select it by clicking on its name.

#### Adding Units

To add a unit, you need to have an army selected and a mek selected. Enter the pilot data and click the Add Unit button. You should see the unit appear in the Army Unit List window to the bottom left. You can add as many units as you want. Currently, there is no way to remove a unit added by mistake, be sure to click carefully.

#### Setting Up the Map

Finally, initialize the map by entering the width and height of the map. Once you are happy with the map size, click "Update Map Size".

#### Ready to Play

If you have completed all of these steps, you will see the "Game Not Ready" button change to "Game Ready". Click it to begin playing.

### Initiative Phase

Initiative should roll automatically. If it does not, click "Next Phase" to roll it.

### Deployment Phase

To deploy a unit, click on the unit in the list on the right, then click on the hex you want to deploy in. Right now, you can deploy anywhere, you will have to manually enforce deployment zones. When you are happy with that unit's deployment, click "Deploy". If you want to deploy a different unit, click "Undeploy". When everyone has moved, click "Next phase".

### Movement Phase

To move a unit, the unit you want to move and then the button for the movement type you want (Walk or Jump). The walk button is also the button for tracked, wheeled, or all of the other "basic" movement types. Then click a hex. If you cannot reach that hex, nothing will happen. If you can reach that hex, then a ghost will appear in that hex. When you're ready to move that unit, click "Move Unit". If a unit is standing still, click them, click "Stand Still", and then click "Move Unit". When everyone has moved, click "Next Phase".

### Combat Phase

The Combat phase works similarly. Click a unit on the right side, click their target, and then click "Attack". A popup will appear with the details on the attack. Click "Attack" to resolve the attack or click "Cancel" to select another target. Once everyone has fired, click Next Phase.

### End Phase

Destroyed units will be removed automatically at the end of the end phase. Click Next Phase to start the next round.

### Next round

Initiative for the next round should roll automatically! If it does not, click "Reroll Initiative" and "Next Phase". Play continues until one side is destroyed. When you are finished, click "Quit Game" to exit.

## What's coming

### 0.1.0 DONE

This, the first "functional" release, is a very primitive implementation of the rules, allowing a hot-seat 2v2. The map is a featureless plain. The pilots will all be skill 4. Selecting new units requires programming. Facing will not be calculated. Critical hits will not be applied. Only standard attack types will work. Only walking and jumping will be allowed (nothing but Mechs). Basically, you will be able to move, shoot, and lose armor/structure until your mech dies.

### 1.1.0 DONE

Allow you to build your armies before battle. Deployment happens via entering the locations (as the instructions above), expect this to change very quickly.

### 1.2.0 Previous Release

Deployment happens now by click on the map in the deployment phase. Automatic advancement of phases works sometimes, but it doesn't always detect that it should advance.

### 1.3.0 This Release

Rewrite everthing using GTK4 instead of McCLIM. This should allow for much simpler cross platform installation. This rewrite allowed me to also add the following features:

- An MUL listing all the units in MegaMek 0.49.14 (~8100)
- Movement Ghosts, so you can change your mind about your intended destination
- A confirmation menu for attacks in case you click the wrong target.

I also used the opportunity to rewrite the code to make it easier to add in new rules. Finally, I also bought the "new" Commanders Edition PDF to replace the original rulebook I bought ages ago in a Barnes and Noble. Based on that, I see that this Introductory/Standard/Advanced break down I've been using to structure this code is no longer actually how the rulebook is structured, meaning I'll need to reimagine the rest of the V1.x series. Expect an updated roadmap soon.

### 1.4.0

Full implementation of introductory rules for maps and movement.

### 1.5.0

Full implementation of introductory rules for attacks and damage (excluding physical attacks).

### 1.6.0

Implementation of physical attacks, heat, and all specials that are part of the Introductory rule set.

### 1.7.0

Full implementation of the introductory rule set for Alphastrike.

### 1.8.0

Add save games and other cleanup before moving on to implement the Standard rules for the 2.0 series.

## How to Contribute

How you can help depends on how much you know Lisp.

### I'm a keeper of the parentheses, I wield them like the lightning bolts of Zeus.

Awesome! Your help is definitely welcome! This is the largest project I've ever written in Common Lisp and help would be appreciated. We can chat about specifics but areas I know I need help with are:

1. GUI redesign. CL-GTK4 is challenging. I've made a lot of progress, but there are some frustrating bugs (which I'll be logging as issues in Github) that I could use some help with.
2. Movement Algorithm. Movement will probably require some variation on Astar or a heat map. I'm an amateur, Astar is really hard for me. I'd love some help implementing it.
3. AI. Eventually, I want to have a bot you can play against. I know a tiny bit about how to implement that, but I'd welcome help.
4. Network. I want this to eventually be like MegaMek, able to be played over the network.. I realize this will require a client-server setup and likely some extensive rewriting (though I've tried to separate things out as best I can to make that easier), but that's all I know.

### I know this Lisp you speak of.

Fantastic, pull requests are welcome to help me with whatever catches your fancy. Particularly, I'd love for someone to help me with the Windows build. I'm able to test builds for MacOS and Linux on my computers but not Windows.

### Common lisp? No, I speak just fine. But I do know my Battletech!

Playtesting is greatly appreciated. Submit issues to Github and I'll do my best to address them! Also places where I've got the rules wrong. I'm writing this specifically because I have nobody to play Alphastrike with, so my understanding of the rules is largely theoretical.

# Credits

Most of the files found in the data folder are used with permission from Megamek [Megamek](https://github.com/MegaMek/megamek).

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

_MechWarrior, BattleMech, ‘Mech and AeroTech are registered trademarks of The Topps Company, Inc. Original BattleTech material Copyright by Catalyst Game Labs All Rights Reserved. Used without permission._
