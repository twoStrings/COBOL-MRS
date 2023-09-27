      
	  *****************************************************************
      * FD TICKET INFORMATION DATASTORE
      * Alan, John, and Taryn
      * This file is used to store all the information that will need
      * to go to the ticketing system so they can sell tickets to our
      * lovely customers
      * 
      * The record length is 47 characters.
      * 
      * The file is Indexed Sequential 
      * The key field is SCREEN-NUMBER
      *****************************************************************
       FD MRS-TICKET-INFO
           RECORD CONTAINS 47 CHARACTERS.
       01  MRS-TICK-REC.
           05  MRS-TIC-MOVIE-NAME         PIC X(20).
           05  MRS-TIC-SHOW-TIME          PIC 9(20).
           05  MRS-TIC-SEATS-AVALIBLE     PIC 99 VALUE 40.
           05  MRS-TIC-RATING             PIC X(4).
           05  MRS-TIC-SCREEN-NUMBER      PIC 9.