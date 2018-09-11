# SAM MOD player

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

# To do:

Things still to do in future versions of the SAM MOD player - just so that I don't forget them  :)                         
                                                                
1. fix SAA volume tables to use 3 bits - lowest bit is not audible.

2. megabyte support

3. compress patterns into own track structure a pattern now consists of four tracks, identical tracks are not put into memory twice                               
                                                                
4. do not keep the header info in memory, when loading grab the necessary info and then load over it                    
                                                                
5. allow compression (via quality reduction) on mods which are too large to fit into memory (remember 256k check!) 
if a mod is too large then only load 1 out of 2 sample bytes and transpose the pattern data down one octave.

Also double all portamento commands and possibly vibrato too.  Adjust boundary values....                            
                                                                
6. volume bars in tracker screen plus the option to use a number of different mode 4 screens with a scrolly or something like that.... (complaints about mode 2 look)      
- also a screen with graphs of volume and pitch             
                                                                
7. fix pitch commands when using different tempos.  Adjust for tempo factor (multply with parameter).                      
                                                                