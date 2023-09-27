      *****************************************************************
      * FD MOVIE DATASTORE
      * Alan, John, and Taryn
      * This file is used to store all the information for each
      * individual movie. Each record contains the information needed
      * from a movie. It is used by the MRS system.
      * 
      * The record length is 303 characters.
      * 
      * The file is Indexed Sequential 
      * The key field is MOVIE-NO
      *****************************************************************
       FD MRS-MOVIE-FILE
           RECORD CONTAINS 292 CHARACTERS.
       01 MRS-MOVIE-REC.
          05  MRS-MOVIE-KEY.
              10 MRS-VENDOR-NO                     PIC X(2).
              10 MRS-MOVIE-NO                      PIC X(4).
          05 MRS-MOVIE-NAME                        PIC X(20).
          05 MRS-PRODUCTION-CO                     PIC X(15).
          05 MRS-DIRECTORS                         PIC X(20).
          05 MRS-RATING                            PIC X(4).
          05 MRS-GENRE                             PIC X(20).
          05 MRS-DESCRIPTION                       PIC X(200).
          05 MRS-RENTAL-COST                       PIC S9(4)V99.
          05 MRS-ACTIVE-FLAG                       PIC X.
     