       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MRS-3200.
       AUTHOR.  JOHN BELLEK.
      *****************************************************************
      * This subprogram will have the user enter a movie id and then 
      * search the file to see if that movie is there.
      * If it is, it will then ask the user if they wish to delete the 
      * file.
      *  
      * 
      *****************************************************************

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
          CURSOR IS CRPT
          CRT STATUS IS SCR-STAT.

       FILE-CONTROL.
           SELECT MRS-SCH-INFO-FILE
               ASSIGN TO UT-SYS-MRS-SCH
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MRS-SCH-MOVIE-ID
               ALTERNATE KEY IS MRS-SCH-SCREEN-NUMBER.

           SELECT MRS-TICKET-INFO
               ASSIGN TO UT-SYS-MRS-TIC
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MRS-TIC-SCREEN-NUMBER.
          
       DATA DIVISION.
       FILE SECTION.
       COPY "CPYBOOKS/MRS-SCH-INFO.CPY".
       COPY "CPYBOOKS/MRS-TICKET-INFO.CPY".


       WORKING-STORAGE SECTION.
       COPY "CPYBOOKS/FUNCTION-KEYS.CPY".
       COPY "CPYBOOKS/DATETIME.CPY".

       01  WORKING-VARIABLES.
           05  WV-SCR-NUM            PIC 9.
           05  WV-SHOW-TIMES.
               10  SHOW-1            PIC 9999.
               10  SHOW-2            PIC 9999.
               10  SHOW-3            PIC 9999.
               10  SHOW-4            PIC 9999.
               10  SHOW-5            PIC 9999.
           05  WV-DESCRIPTION.
               10  WV-DES-1          PIC X(40).
               10  WV-DES-2          PIC X(40).
               10  WV-DES-3          PIC X(40).
               10  WV-DES-4          PIC X(40).
               10  WV-DES-5          PIC X(40).
           05  WV-CONFIRM            PIC X.
           05  WV-ENTER              PIC X.
           05  SCH-EOF               PIC X.
           05  TIC-EOF               PIC X.
           05  WV-AGAIN              PIC X.
           05  WV-COUNT              PIC 9 VALUE 0.

       01  WORKING-INPUT.
           05 WI-MOVIE-ID            PIC X(4).
           05 WI-MOVIE-NAME          PIC X(20).
		   05 WI-MOVIE-VENDOR        PIC X(15).
           05 WI-SHOW-TIME           PIC X(20).
           05 WI-SEATS               PIC 99.
           05 WI-RATING              PIC X(4).
		   05 WI-DESCRIPTION         PIC X(200).
           05 WI-SCREEN-NUMBER       PIC 9.

       01  WORKING-TICKET.
           05  WT-MOVIE-NAME         PIC X(20).
           05  WT-SHOW-TIME          PIC 9(20).
           05  WT-SEATS-AVALIBLE     PIC 99 VALUE 40.
           05  WT-RATING             PIC X(4).
           05  WT-SCREEN-NUMBER      PIC 9.

      * File path(s)
            05  UT-SYS-MRS-SCH       PIC X(50)
                                   VALUE "C:\COBOL\MRS-SCH-INDEX.dat".
           05  UT-SYS-MRS-TIC           PIC X(50)
                                   VALUE "C:\COBOL\MRS-TICKET-INFO.DAT".


       LINKAGE SECTION.
       01  LS-OPTION    PIC 9.

       
       SCREEN SECTION.
       01  MOVIE-SCHEDULE-DEL             BLANK SCREEN
                                            PROMPT
                                            AUTO
                                            REQUIRED
                                            BACKGROUND-COLOR 0
                                            FOREGROUND-COLOR 7.
           05  MV-TITLE-LINE.
               10  LINE 1 COL 1            VALUE "MRS320".
               10         COL 30           VALUE "MOVIE THEATER SYSTEM".
               10         COL 70           PIC Z9 FROM WS-MONTH.
               10         COL 72           VALUE "/".
               10         COL 73           PIC Z9 FROM WS-DAY.
               10         COL 75           VALUE "/".
               10         COL 76           PIC 9999 FROM WS-YEAR.

           05  SCHEDULE-DELETE-TITLE.
               10  LINE 2 COL 17
                   VALUE "MOVIE RENTALS AND SCHEDULING: ".
               10         COL 47 VALUE "MOVIE SCHEDULE DELETE".
               
           05  CHECK-ID.
               10  LINE 6 COL 17 VALUE "SCREEN NUMBER:".
               10  LINE 6 COL 32 PIC 9 TO WV-SCR-NUM REVERSE-VIDEO.

           05 SCH-FUNCTION.
             10  LINE 25   COL 1  VALUE "F1 = HELP     F3 = END     ".
             10            COL 27 VALUE " F4 = RETURN     F12 = CLEAR".

       01  SCHEDULE-DELETE-SHOW.
           05  DELETE-SCHEDULE.
               10  LINE 6 COL 21 VALUE "MOVIE ID:".
               10  LINE 7 COL 20  VALUE "MOVIE NAME:".
               10  LINE 8 COL 19  VALUE "VENDOR NAME:".
               10  LINE 9 COL 17  VALUE "SCREEN NUMBER:".
               10  LINE 10 COL 20 VALUE "SHOW TIMES:".
               10  LINE 11 COL 19 VALUE "SEAT NUMBER:".
               10  LINE 12 COL 24 VALUE "RATING:".
               10  LINE 13 COL 19 VALUE "DESCRIPTION:".

           05  DELETE-GET-DATA.
               10  LINE 6 COL 32 PIC 9999 FROM WI-MOVIE-ID.
               10  LINE 7 COL 32  PIC X(15) FROM WI-MOVIE-NAME.
               10  LINE 8 COL 32  PIC 9  FROM WI-MOVIE-VENDOR.
               10  LINE 9 COL 32  PIC 9 FROM WI-SCREEN-NUMBER.
               10  LINE 10 COL 32  PIC 9999 FROM SHOW-1. 
               10  LINE 11 COL 32  PIC 99 FROM WI-SEATS.
               10  LINE 12 COL 32 PIC X(4) FROM WI-RATING.
               10  LINE 13 COL 32 PIC X(40) FROM WV-DES-1.
               10  LINE 14 COL 32 PIC X(40) FROM WV-DES-2.
               10  LINE 15 COL 32 PIC X(40) FROM WV-DES-3.
               10  LINE 16 COL 32 PIC X(40) FROM WV-DES-4.
               10  LINE 17 COL 32 PIC X(40) FROM WV-DES-5.

           05  SCH-DELETE-CONFIRM.
               10  LINE 22 COL 15 VALUE "CONFIRM DELETE:  Y/N".
               10          COL 39 PIC X TO WV-CONFIRM REVERSE-VIDEO.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 24 COL 1 BLANK LINE.


       01  DELETE-MESSAGE-BOX.
           05  SCH-DEL-HELP                FOREGROUND-COLOR 3.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "ENTER Y IF YOU WOULD LIKE TO".
               10          COL 38 VALUE " DELETE THIS RECORD.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10 VALUE "ENTER N IF YOU WOULD NOT LIKE".
               10          COL 39 VALUE " TO DELETE THIS RECORD.".

           05  SCH-MOVIE-ID-HELP          FOREGROUND-COLOR 3.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "ENTER THE 4 DIGIT MOVIE CODE".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  SCH-DELETE-AGAIN            FOREGROUND-COLOR 7.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10 VALUE "WOULD YOU LIKE TO DELETE".
               10          COL 35 VALUE "ANOTHER RECORD:  Y/N".
               10          COL 60 PIC X TO WV-AGAIN REVERSE-VIDEO.

           05  DELETE-SUCCESS              FOREGROUND-COLOR 2.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "RECORD WAS SUCCESSFULLY ".
               10          COL 35 VALUE "DELETED.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  DELETE-ERROR                FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "RECORD WAS NOT DELETED.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  SCH-NOT-FOUND               FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "SCHEDULE WAS NOT FOUND".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  EMPTY-FILE                  FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "THE SCHEDULE FILE IS EMPTY".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.
           
       procedure division USING LS-OPTION.

           PERFORM 900-OPEN-FILES THRU 900-EXIT
           COPY "CPYBOOKS/ENABLE-KEYS.CPY".

           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME
           MOVE "Y" TO WV-AGAIN

           PERFORM 200-LOOP-DELETE THRU 200-EXIT
           UNTIL (WV-AGAIN = "N")

           PERFORM 910-CLOSE-FILES THRU 910-EXIT
           goback.

      * Loops until the user would not like to delete anymore records.
       200-LOOP-DELETE.
           DISPLAY MOVIE-SCHEDULE-DEL
           ACCEPT MOVIE-SCHEDULE-DEL

           if(F3 OR F4)
               MOVE "N" TO WV-AGAIN

           ELSE
               IF (F1)
                    DISPLAY SCH-MOVIE-ID-HELP
               ELSE
                   MOVE SPACES TO WV-CONFIRM
                   PERFORM 300-READ-FILE THRU 300-EXIT
                   IF (NOT F4 OR NOT F3)
                   DISPLAY SCH-DELETE-AGAIN
                   ACCEPT SCH-DELETE-AGAIN
                   end-if
                   if(WV-AGAIN = "Y")
                       MOVE "N" TO SCH-EOF
                       MOVE "N" TO TIC-EOF
                       MOVE ZEROS TO WV-COUNT
                   end-if
               END-IF
           end-if.
       200-EXIT.
           EXIT.

      * Read the schedule file until it finds the movie the user is
      * looking for or until the file is out of records
       300-READ-FILE.
           PERFORM UNTIL (SCH-EOF = "Y")
               MOVE WV-SCR-NUM TO MRS-SCH-SCREEN-NUMBER
               READ MRS-SCH-INFO-FILE KEY IS MRS-SCH-SCREEN-NUMBER
               INVALID KEY
                   PERFORM 305-SCH-INVALID THRU 305-EXIT
               NOT INVALID KEY
                   PERFORM 310-SCH-VALID THRU 310-EXIT
               end-read
           END-PERFORM.
       300-EXIT.
           exit.

      * At the end of the file it will display the the movie was not
      * found in the file
       305-SCH-INVALID.
           MOVE "Y" TO SCH-EOF
           if(WV-COUNT = 0)
               DISPLAY EMPTY-FILE
               ACCEPT EMPTY-FILE
           end-if
           if(WV-CONFIRM NOT EQUAL "Y" or
               WV-CONFIRM NOT EQUAL "N")
               DISPLAY SCH-NOT-FOUND
               ACCEPT SCH-NOT-FOUND
           END-IF.
       305-EXIT.

           exit.

      * Checks to see if the record read was the correct one for the 
      * user
       310-SCH-VALID.
           ADD 1 TO WV-COUNT
           MOVE MRS-SCH-INFO-REC TO WORKING-INPUT

           if(WV-SCR-NUM = WI-SCREEN-NUMBER)
               PERFORM 400-DELETE THRU 400-EXIT
           end-if.
       310-EXIT.
           exit.

      * Provides the data for the movie that the user would like to 
      * delete.
       400-DELETE.
           MOVE WI-DESCRIPTION TO WV-DESCRIPTION
           MOVE WI-SHOW-TIME TO WV-SHOW-TIMES

           PERFORM 475-FKEY-ENTERED THRU 475-EXIT
           UNTIL (WV-CONFIRM = "Y" OR
                  WV-CONFIRM = "N")

           IF (WV-CONFIRM = "Y")

               PERFORM 450-DELETE-TIC-REC THRU 450-EXIT
               UNTIL (TIC-EOF = "Y")

               DELETE MRS-SCH-INFO-FILE
               END-DELETE

               DISPLAY DELETE-SUCCESS
               ACCEPT DELETE-SUCCESS

               MOVE "Y" TO SCH-EOF
           else IF (WV-CONFIRM = "N" AND NOT F4)
               DISPLAY DELETE-ERROR
               ACCEPT DELETE-ERROR

               MOVE "Y" TO SCH-EOF
           ELSE
               MOVE "Y" TO SCH-EOF
           END-IF.
       400-EXIT.
           EXIT.

       450-DELETE-TIC-REC.
           MOVE WI-SCREEN-NUMBER TO MRS-TIC-SCREEN-NUMBER
           READ MRS-TICKET-INFO KEY IS MRS-TIC-SCREEN-NUMBER
           INVALID KEY
               MOVE "Y" TO TIC-EOF
           NOT INVALID KEY
               MOVE MRS-TICK-REC TO WORKING-TICKET
               if(WT-SCREEN-NUMBER = MRS-SCH-SCREEN-NUMBER)
                   DELETE MRS-TICKET-INFO
                   end-delete

                   MOVE "Y" TO TIC-EOF
               end-if
           END-READ.
       450-EXIT.
           EXIT.

      *LOOP INCASE THE USER ENTERS F1
       475-FKEY-ENTERED.
           DISPLAY SCHEDULE-DELETE-SHOW
           ACCEPT SCHEDULE-DELETE-SHOW

           if(F3)
               MOVE "N" TO WV-CONFIRM
               MOVE "N" TO WV-AGAIN
           end-if

           if(F4)
               MOVE "N" TO WV-CONFIRM
           end-if

           if(F1)
               DISPLAY SCH-DEL-HELP
           end-if.
       475-EXIT.
           exit.

      * Opens the files
       900-OPEN-FILES.
           OPEN I-O MRS-TICKET-INFO
                    MRS-SCH-INFO-FILE.
       900-EXIT.
           EXIT.

      * Closes the files
       910-CLOSE-FILES.
           CLOSE MRS-TICKET-INFO
                 MRS-SCH-INFO-FILE.
       910-EXIT.
           EXIT.
           
       end program MRS-3200.