; SAM MOD player

; (C) 2018 Stefan Drissen 

; easiest way to combine the various output files and auto-load them

	include "memory.i"

	dump page.loader,0    
	autoexec    
	mdat "../obj/loader"
	
	dump page.loader,8192	
	mdat "../obj/demo"
	
	dump page.sequencer,0	
	mdat "../obj/sequencer"
	
	dump page.create.burstplayer,0
	mdat "../obj/burstplayer"
	 	    
