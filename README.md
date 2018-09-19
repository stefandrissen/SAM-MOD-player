# SAM MOD player 

[![Build Status](https://travis-ci.org/stefandrissen/SAM-MOD-player.svg?branch=master)](https://travis-ci.org/stefandrissen/SAM-MOD-player)

## Structure

- loader
- burstplayer
- demo
- sequencer
- example

### loader

This is the initial front end. It provides a screen to select the output device:

- colour lookup table
- saa1099 soundchip
- samdac on port 1 or 2
- dac on port 1 or 2
- blue alpha sampler
- quazar soundcard

And the type of Amiga the mod was intended for:

- pal
- ntsc

Once these have been selected, the burst player code is generated.

The disk (sam or dos format) is then scanned for possible mod files, the drive can be changed or rescanned.
When a file is selected, it is loaded and the "demo" starts. 

### burstplayer

This generates the code for the burst player based on the output device:

- ports
- channels
- manual timing 
  
The burst player outputs the samples from the sample buffers to the output device.

The playback rate is 10,400 Hz, this is achieved by outputting sample data 208 times a frame, this is once every 1.5 scan line.
During the border area, timing is manual.  During the screen area line interrupts are set every 3 lines, the first output is on 
the line interrupt, the second is timed manually at 1.5 line, after which control is given back to the "demo" until the next line
interrupt occurs.

### demo

This shows information on the module being played via various screens:

- welcome
- help (f1)
- samples short (f2)
- samples detailed (f3)
- tracker (f4)
- supported effects (f5)
- sample debug info (f6)

### sequencer

This is the sequencer. It is called every frame. 
It reads the pattern and sample information and fills the sample playback buffers.

### example 

This is a simple example that replaces the "demo".

## compiling

I use Eclipse. Ant is used to invoke pyz80 for assembling.

"all" will assemble loader, demo, burstplayer and sequencer, the four object files are then combined by assembling modplayer which produces an autoboot modplayer.dsk image. SimCoupe is then started with the modplayer.dsk image.

The properties used to find python, pyz80 and SimCoupe are at the top of the build.xml file.
