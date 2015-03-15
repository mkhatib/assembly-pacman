# Pacman-like Assembly Game
A computer organization course homework at Birzeit University to build a simple pacman-like game with Assembly 8086.

# How to run
## Mac OS X

Follow this [YouTube guide](https://www.youtube.com/watch?v=9EMs3x-LJvs) explaining how to use DOSBox to run TASM on your Mac OS X. The guy also links in the YouTube description to a zip file which I [re-uploaded for redundancy](https://drive.google.com/open?id=0B9qF9jxAyahXfjRqUkE3UWppN2xOWGdQamtiNEdZWnphT2VPcDk4dkgtemJVbkJwS3VpTTQ&authuser=0) in case the file goes away. That zipfile includes TASM.exe and TLINK.exe and some other debugger and utils files.

## Windows
Probably not much needed here other than downloading the TASM.

## Running the pacman.asm
```sh
$ tasm pacman.asm
$ tlink pacman.obj
$ pacman
```

# Screenshots

<p align="center"><img src="http://storage.googleapis.com/magical-unicorn/assembly-pacman/screenshots/1.png" width="700px" alt="Main Menu"></p>

<p align="center"><img src="http://storage.googleapis.com/magical-unicorn/assembly-pacman/screenshots/2.png" width="700px" alt="Playing the game"></p>

<p align="center"><img src="http://storage.googleapis.com/magical-unicorn/assembly-pacman/screenshots/3.png" alt="Winning the game"></p>

<p align="center"><img src="http://storage.googleapis.com/magical-unicorn/assembly-pacman/screenshots/4.png" width="700px" alt="Exiting the game"></p>
