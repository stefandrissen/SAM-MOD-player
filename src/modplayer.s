; SAM MOD player

; (C) 2018 Stefan Drissen 

; easiest way to combine the various output files and auto-load them

	dump 1,0    
	autoexec    
	mdat "../obj/loader"
	
	dump 1,8192	
	mdat "../obj/demo"
	
	dump 4,0	
	mdat "../obj/sequencer"
	
	dump 5,0
	mdat "../obj/burstplayer"
	 	    
