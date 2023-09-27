      *****************************************************************
      * FD TICKET-INFO-DATASTORE
      * Alan, John, and Taryn
      * This file is used to store all the information that ticketing
      * will need for their subsystem. It is loaded by a batch program.
      * 
      * The record length is 266 characters.
      * 
      * The file is Indexed Sequential 
      * The key field is INFO-ID.
      *****************************************************************
       FD MRS-SCH-INFO-FILE
           RECORD CONTAINS 266 CHARACTERS.
       01 MRS-SCH-INFO-REC.
           05 MRS-SCH-MOVIE-ID                 PIC X(4).
           05 MRS-SCH-MOVIE-NAME               PIC X(20).
		   05 MRS-SCH-MOVIE-VENDOR             PIC X(15).
           05 MRS-SCH-SHOW-TIME                PIC X(20).
           05 MRS-SCH-SEATS                    PIC 99.
           05 MRS-SCH-RATING                   PIC X(4).
		   05 MRS-SCH-DESCRIPTION              PIC X(200).
           05 MRS-SCH-SCREEN-NUMBER            PIC 9.
      

