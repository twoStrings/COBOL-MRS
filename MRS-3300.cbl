       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MRS-3300.
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


       DATA DIVISION.
       FILE SECTION.
       COPY "CPYBOOKS/MRS-SCH-INFO.CPY".

       WORKING-STORAGE SECTION.
       COPY "CPYBOOKS/FUNCTION-KEYS.CPY".
       COPY "CPYBOOKS/DATETIME.CPY".

       01  WORKING-VARIABLES.
           05  WV-SCR-NUM            PIC 9.
           05  WV-SHOW-TIMES.
               10  WV-SHOW-1         PIC 9(4).
               10  WV-SHOW-2         PIC 9(4).
               10  WV-SHOW-3         PIC 9(4).
               10  WV-SHOW-4         PIC 9(4).
               10  WV-SHOW-5         PIC 9(4).
           05  WV-DESCRIPTION.
               10  WV-DES-1          PIC X(40).
               10  WV-DES-2          PIC X(40).
               10  WV-DES-3          PIC X(40).
               10  WV-DES-4          PIC X(40).
               10  WV-DES-5          PIC X(40).
           05  WV-CONFIRM            PIC X.
           05  SCH-EOF               PIC X.
           05  WV-ENTER              PIC X.
           05  WV-AGAIN              PIC X.
           05  WV-HOLDING-TIME       PIC 9999.
           05  WV-HOLDING-SCREEN     PIC 9.
           05  WV-FUNC-PRESS         PIC X.

       01  WORKING-CHANGE.
           05  WC-MOVIE-ID           PIC XXXX.
           05  WC-MOVIE-NAME         PIC X(20).
           05  WC-MOVIE-VENDOR       PIC X(15).
           05  WC-SHOW-TIMES.
               10  WC-SHOW-1         PIC 9(4).
               10  WC-SHOW-2         PIC 9(4).
               10  WC-SHOW-3         PIC 9(4).
               10  WC-SHOW-4         PIC 9(4).
               10  WC-SHOW-5         PIC 9(4).
           05  WC-SEATS              PIC 99.
           05  WC-RATING             PIC XXXX.
           05  WC-DESC               PIC X(200).
           05  WC-NEW-SCREEN         PIC 99.

       01  WORKING-INPUT.
           05 WI-MOVIE-ID            PIC X(4).
           05 WI-MOVIE-NAME          PIC X(20).
		   05 WI-MOVIE-VENDOR        PIC X(15).
           05 WI-SHOW-TIME           PIC X(20).
           05 WI-SEATS               PIC 99.
           05 WI-RATING              PIC X(4).
		   05 WI-DESCRIPTION         PIC X(200).
           05 WI-SCREEN-NUMBER       PIC 9.

       01  WORKING-SCREEN-CHECK.
           05 WSC-MOVIE-ID            PIC X(4).
           05 WSC-MOVIE-NAME          PIC X(20).
		   05 WSC-MOVIE-VENDOR        PIC X(15).
           05 WSC-SHOW-TIME           PIC X(20).
           05 WSC-SEATS               PIC 99.
           05 WSC-RATING              PIC X(4).
		   05 WSC-DESCRIPTION         PIC X(200).
           05 WSC-SCREEN-NUMBER       PIC 9.

      * File path(s)
           05  UT-SYS-MRS-SCH         PIC X(50)
                                   VALUE "C:\COBOL\MRS-SCH-INDEX.dat".


       SCREEN SECTION.
       01  MOVIE-SCHEDULE-UP                BLANK SCREEN
                                            PROMPT
                                            AUTO
                                            REQUIRED
                                            BACKGROUND-COLOR 0
                                            FOREGROUND-COLOR 7.
           05  MV-TITLE-LINE.
               10  LINE 1 COL 1            VALUE "MRS330".
               10         COL 30           VALUE "MOVIE THEATER SYSTEM".
               10         COL 70           PIC Z9 FROM WS-MONTH.
               10         COL 72           VALUE "/".
               10         COL 73           PIC Z9 FROM WS-DAY.
               10         COL 75           VALUE "/".
               10         COL 76           PIC 9999 FROM WS-YEAR.

           05  SCHEDULE-ADD-TITLE.
               10  LINE 2 COL 17
                   VALUE "MOVIE RENTALS AND SCHEDULING: ".
               10         COL 47 VALUE "MOVIE SCHEDULE UPDATE".
   
           05  CHECK-ID.
               10  LINE 6 COL 17 VALUE "SCREEN NUMBER:".
               10  LINE 6 COL 32 PIC 9 TO WV-SCR-NUM REVERSE-VIDEO.

           05 SCH-FUNCTION.
             10  LINE 25   COL 1  VALUE "F1 = HELP     F3 = END     ".
             10            COL 27 VALUE " F4 = RETURN     F12 = CLEAR".

       01  SCHEDULE-UPDATE.
           05  UPDATE-SCHEDULE.
               10  LINE 6 COL 21 VALUE "MOVIE ID:".
               10  LINE 7 COL 20  VALUE "MOVIE NAME:".
               10  LINE 8 COL 19  VALUE "VENDOR NAME:".
               10  LINE 9 COL 17  VALUE "SCREEN NUMBER:".
               10  LINE 10 COL 20 VALUE "SHOW TIMES:".
               10  LINE 11 COL 19 VALUE "SEAT NUMBER:".
               10  LINE 12 COL 24 VALUE "RATING:".
               10  LINE 13 COL 19 VALUE "DESCRIPTION:".

           05  UPDATE-GET-DATA.
               10  LINE 6 COL 32 PIC 9999 FROM WI-MOVIE-ID.
               10  LINE 7 COL 32  PIC X(15) FROM WI-MOVIE-NAME.
               10  LINE 8 COL 32  PIC 9  FROM WI-MOVIE-VENDOR.
               10  LINE 9 COL 32  PIC 9
                                   USING WI-SCREEN-NUMBER REVERSE-VIDEO.
               10  LINE 10 COL 32  PIC 9999
                                    USING WV-SHOW-1 REVERSE-VIDEO. 
               10  LINE 11 COL 32  PIC 99 FROM WI-SEATS.
               10  LINE 12 COL 32 PIC X(4) FROM WI-RATING.
               10  LINE 13 COL 32 PIC X(40) FROM WV-DES-1.
               10  LINE 14 COL 32 PIC X(40) FROM WV-DES-2.
               10  LINE 15 COL 32 PIC X(40) FROM WV-DES-3.
               10  LINE 16 COL 32 PIC X(40) FROM WV-DES-4.
               10  LINE 17 COL 32 PIC X(40) FROM WV-DES-5.

           05  SCH-UPDATE-CONFIRM.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 15 VALUE "CONFIRM UPDATE:  Y/N".
               10          COL 39 PIC X TO WV-CONFIRM REVERSE-VIDEO.
           05  SCH-UPDATE-HELPFUL-TIP.
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10
                    VALUE "HIT TAB TO MOVE TO THE NEXT FIELD"
                    FOREGROUND-COLOR 3.

       01  MESSAGE-BOX.
           05  UPDATE-SUCCESS           FOREGROUND-COLOR 2.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10 VALUE "UPDATE SUCCESSFUL!".
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  UPDATE-FAILED            FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10 VALUE "DATA WAS NOT UPDATED".
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  MOVIE-UPDATE-AGAIN       FOREGROUND-COLOR 7.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "WOULD YOU LIKE TO UPDATE".
               10          COL 35 VALUE "ANOTHER RECORD:  Y/N".
               10          COL 60 PIC X TO WV-AGAIN REVERSE-VIDEO.
               10  LINE 24 COL 1 BLANK LINE.

           05  MOVIE-NOT-FOUND          FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "MOVIE WAS NOT FOUND IN THE ".
               10          COL 37 VALUE "SCHEDULE".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  UPDATE-HELP             FOREGROUND-COLOR 3.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "ENTER A SCREEN NUMBER 1-6".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  UPDATE-DATA-HELP             FOREGROUND-COLOR 3.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "YOU CAN CHANGE THE SCREEN AND".
               10          COL 39 VALUE " TIME.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  SAME-SCREEN                  FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "A MOVIE IS ALREADY SCHEDULED".
               10          COL 38 VALUE " FOR THIS SCREEN.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.


       procedure division.

           OPEN I-O MRS-SCH-INFO-FILE.
           COPY "CPYBOOKS/ENABLE-KEYS.CPY".

           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME
           MOVE SPACES TO WV-AGAIN
           PERFORM 200-LOOP-UPDATE THRU 200-EXIT
           UNTIL (WV-AGAIN = "N" OR F3 OR F4)
           

           CLOSE MRS-SCH-INFO-FILE.
           goback.

       200-LOOP-UPDATE.
           MOVE "N" TO WV-FUNC-PRESS

           DISPLAY MOVIE-SCHEDULE-UP
           ACCEPT MOVIE-SCHEDULE-UP

           IF (F3 OR F4)
               MOVE "N" TO WV-AGAIN
           ELSE IF (F1)
                    DISPLAY UPDATE-HELP
                    ACCEPT UPDATE-HELP
           else
               PERFORM 300-READ-FILE THRU 300-EXIT
               if(WV-FUNC-PRESS = "N")
                   DISPLAY MOVIE-UPDATE-AGAIN
                   ACCEPT MOVIE-UPDATE-AGAIN

                   if(WV-AGAIN = "Y")
                       CLOSE MRS-SCH-INFO-FILE
                       OPEN I-O MRS-SCH-INFO-FILE
                       MOVE "N" TO SCH-EOF
                   end-if
               end-if
           END-IF.
       200-EXIT.
           exit.

       300-READ-FILE.
           PERFORM UNTIL (SCH-EOF = "Y")
               MOVE WV-SCR-NUM TO MRS-SCH-SCREEN-NUMBER
               READ MRS-SCH-INFO-FILE KEY IS MRS-SCH-SCREEN-NUMBER
               INVALID KEY
                   DISPLAY MOVIE-NOT-FOUND
                   ACCEPT MOVIE-NOT-FOUND

                   MOVE "Y" TO SCH-EOF
               NOT INVALID KEY
                   PERFORM 400-UPDATE THRU 400-EXIT
               end-read
           END-PERFORM.
       300-EXIT.
           exit.

       
       400-UPDATE.
           MOVE MRS-SCH-INFO-REC TO WORKING-INPUT
           MOVE WI-DESCRIPTION TO WV-DESCRIPTION
           MOVE WI-SHOW-TIME TO WV-SHOW-TIMES
           MOVE WV-SHOW-1 TO WV-HOLDING-TIME
           MOVE WI-SCREEN-NUMBER TO WV-HOLDING-SCREEN

           PERFORM 450-ACCEPT-UPDATE THRU 450-EXIT
           UNTIL (WV-CONFIRM = "Y" OR
                  WV-CONFIRM = "N")

           if(WV-CONFIRM = "Y")
               PERFORM 500-NEW-INFO THRU 500-EXIT

               PERFORM 600-REWRITE-FILE THRU 600-EXIT

               DISPLAY UPDATE-SUCCESS
               ACCEPT UPDATE-SUCCESS
           ELSE
               if(WV-CONFIRM = "N" AND WV-FUNC-PRESS = "N")
                   DISPLAY UPDATE-FAILED
                   ACCEPT UPDATE-FAILED
               end-IF
           end-if.
       400-EXIT.
           EXIT.

       450-ACCEPT-UPDATE.
           DISPLAY SCHEDULE-UPDATE
           ACCEPT SCHEDULE-UPDATE

           IF (F1)
               DISPLAY UPDATE-DATA-HELP
               ACCEPT UPDATE-DATA-HELP

               DISPLAY SCH-UPDATE-HELPFUL-TIP
           END-IF

           IF (F4 OR F3)
               MOVE "N" TO WV-CONFIRM
               MOVE "Y" TO WV-FUNC-PRESS
               MOVE "N" TO WV-AGAIN
           END-IF

           if(F12)
               MOVE WV-SCR-NUM TO MRS-SCH-SCREEN-NUMBER
               READ MRS-SCH-INFO-FILE KEY IS MRS-SCH-SCREEN-NUMBER
               INVALID KEY 

               NOT INVALID KEY
                   MOVE MRS-SCH-INFO-REC TO WORKING-INPUT
               END-READ
           end-if

           PERFORM 475-SCREEN-CHECK THRU 475-EXIT.
       450-EXIT.
           exit.

       475-SCREEN-CHECK.
           MOVE "N" TO SCH-EOF
           CLOSE MRS-SCH-INFO-FILE
           OPEN I-O MRS-SCH-INFO-FILE
           PERFORM UNTIL SCH-EOF = "Y"
           READ MRS-SCH-INFO-FILE NEXT RECORD INTO WORKING-SCREEN-CHECK
               AT END
                   MOVE "Y" TO SCH-EOF
               NOT AT END
                   if(WI-SCREEN-NUMBER = WSC-SCREEN-NUMBER)
                       MOVE "N" TO WV-CONFIRM
                       MOVE "Y" TO SCH-EOF

                       DISPLAY SAME-SCREEN
                       ACCEPT SAME-SCREEN
                   end-iF
           END-READ
           END-PERFORM.
       475-EXIT.
           exit.
           
       500-NEW-INFO.
           if(WI-SCREEN-NUMBER NOT EQUAL WV-HOLDING-SCREEN)
               MOVE WI-SCREEN-NUMBER TO WC-NEW-SCREEN
           end-if

           if(WV-SHOW-1 NOT EQUAL WV-HOLDING-TIME)
               PERFORM 700-CALCULATE-TIME THRU 700-EXIT
           end-if.
       500-EXIT.
           EXIT.

       600-REWRITE-FILE.
           PERFORM 750-MOVE-VARS THRU 750-EXIT

           REWRITE MRS-SCH-INFO-REC FROM WORKING-CHANGE
           END-REWRITE.
       600-EXIT.
           EXIT.

         
      * Calculates and stores the times for all of the showing of a 
      * movie for one day
       700-CALCULATE-TIME.
           MOVE WV-SHOW-1 TO WC-SHOW-1

           ADD 300 TO WC-SHOW-1 GIVING WC-SHOW-2
           ADD 300 TO WC-SHOW-2 GIVING WC-SHOW-3
           ADD 300 TO WC-SHOW-3 GIVING WC-SHOW-4
           ADD 300 TO WC-SHOW-4 GIVING WC-SHOW-5

           MOVE WC-SHOW-TIMES TO MRS-SCH-SHOW-TIME.
       700-EXIT.
           exit.

       750-MOVE-VARS.
           MOVE WI-MOVIE-ID TO WC-MOVIE-ID
           MOVE WI-MOVIE-NAME TO WC-MOVIE-NAME
           MOVE WI-MOVIE-VENDOR TO WC-MOVIE-VENDOR
           MOVE WI-RATING TO WC-RATING
           MOVE WI-DESCRIPTION TO WC-DESC.
       750-EXIT.
           EXIT.

       end program MRS-3300.