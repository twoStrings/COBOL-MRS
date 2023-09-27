      *****************************************************************
      * FD RENTAL DATASTORE
      * Alan, John, and Taryn
      * This file is used to store all the information for each
      * rental record. It contains the needed time frame in which 
      * the movie is rented along with costs, the journal number, and
      * various flags for programs.
      * 
      * The record length is 47 characters.
      * 
      * The file is Indexed Sequential 
      * The key field is RENT-ID
      *****************************************************************
       FD MRS-RENTAL-FILE
           RECORD CONTAINS 47 CHARACTERS.
       01 MRS-RENTAL-REC.
           05 MRS-RENT-ID                  PIC X(6).
           05 MRS-MOVIE-ID                 PIC 9(4).
           05 MRS-COPY-ID                  PIC 99.
           05 MRS-START-DATE               PIC X(8).
           05 MRS-END-DATE                 PIC X(8).
           05 MRS-SUBTOTAL                 PIC 9(5)V99.
           05 MRS-JOURNAL-NUMBER           PIC X(10).
           05 MRS-READY-TO-SCHEDULE-FLAG   PIC X.
           05 MRS-RETURN-FLAG              PIC X.

      

