# SAM MOD player

[![build](https://github.com/stefandrissen/SAM-MOD-player/actions/workflows/ant.yml/badge.svg)](https://github.com/stefandrissen/SAM-MOD-player/actions/workflows/ant.yml)

## Structure

- boot
- loader
- burstplayer
- demo
- tracker
- example

### boot

This autoboot file will load the loading screen, all code and then start the SAM MOD player.

### loader

This is the initial front end. It provides a screen to select the output device:

- [saa1099 sound chip](https://www.worldofsam.org/products/saa1099)
- [samdac](https://www.worldofsam.org/products/samdac)
- dac
- [blue alpha sampler](https://www.worldofsam.org/products/sound-sampler)
- [quazar soundcard](https://www.worldofsam.org/products/quazar-surround)
- colour lookup table

And the type of Amiga the mod was intended for:

- pal
- ntsc

Once these have been selected, the burst player code is generated.

The disk (or mass storage device) is then scanned for mod files, the drive can be changed or rescanned. Mass storage device records can be selected with cursor left or right, when holding shift the selection jumps 10 records, when holding symbol the selecting jumps 50 records.

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

- help (f1)
- samples short (f2)
- samples detailed (f3)
- tracker (f4) - initial
- supported effects (f5)
- sample debug info (f6)

### tracker

This is the tracker. It is called every frame.
It reads the pattern and sample information and fills the pointers for the sample playback buffers.

### example

This is a simple example that replaces the "demo".

## compiling

Ant is used to invoke [pyz80](https://github.com/simonowen/pyz80) for assembling, this can be executed from Eclipse or Visual Studio Code.

"all" will assemble loader, demo, burstplayer and tracker, the four object files are then assembling boot which produces an autoboot disk image.
SimCoupe is then started with the disk image.

The properties used to find python, pyz80 and [SimCoupe](https://www.simcoupe.org/) are at the top of the build.xml file.
