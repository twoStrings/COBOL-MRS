      *****************************************************************
      * FD SHOW DATASTORE
      * Alan, John, and Taryn
      * This file is used to store all the information for each
      * showing. It contains the date and time along with how many
      * seats are available and which screen it's shown on.
      * 
      * The record length is 30 characters.
      * 
      * The file is Indexed Sequential 
      * The key field is SHOW-ID
      *****************************************************************
       FD MRS-SHOW-FILE
           RECORD CONTAINS 30 CHARACTERS.
       01 MRS-SHOW-REC.
           05 MRS-SHOW-ID                      PIC 9(4).
           05 MRS-RENTAL-ID                    PIC X(7).
           05 MRS-SHOW-DATE                    PIC 9(8).
           05 MRS-SHOW-TIME                    PIC X(7).
           05 MRS-SEATS                        PIC 99.
           05 MRS-SCREEN-NUMBER                PIC 99.
      

