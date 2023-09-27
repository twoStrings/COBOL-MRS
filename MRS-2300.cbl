       IDENTIFICATION DIVISION.
       program-id. MRS_2300.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT MRS-MOVIE-FILE
                   ASSIGN TO UT-SYS-MSTERFILE
                   ORGANIZATION IS INDEXED
                   ACCESS IS DYNAMIC
                   RECORD KEY IS MRS-MOVIE-KEY.

       data division.
       FILE SECTION.
       COPY "./CPYBOOKS/MRS-MOVIE.CPY".


       working-storage section.
       COPY "./CPYBOOKS/FUNCTION-KEYS.CPY".
   

       01 WS-FILENAMES.
         05 UT-SYS-MSTERFILE PIC X(50)
           VALUE "C:\COBOL\MRS-MOVIE-INDEX.dat".

       COPY "./CPYBOOKS/DATETIME.CPY".
      

       01 WS-SWITCHES.
         05 WS-CONFIRM PIC X.
         05 WS-MOVIE-FOUND PIC X.
         05 WS-UPDATED PIC X.

         05 WS-STATE PIC X.
           88 WS-ACTIVE VALUE "A".
           88 WS-DEACTIVE VALUE "D".

         05  WV-DESCRIPTION.
             10  WV-DES1   PIC X(40).
             10  WV-DES2   PIC X(40).
             10  WV-DES3   PIC X(40).
             10  WV-DES4   PIC X(40).
             10  WV-DES5   PIC X(40).

       01 MOVIE.
         05 VENDOR-ID PIC X(2).
         05 MOVIE-ID PIC X(4).
         05 MOVIE-NAME PIC X(20).
         05 PRODUCTION-CO PIC X(15).
         05 DIRECTORS PIC X(20).
         05 RATING PIC X(4).
         05 GENRE PIC X(20).
         05 DESCRIPTION PIC X(200).
         05 RENTAL-COST PIC S9(4)V99.
         05 INACTIVE-FLAG PIC X.

       01 WS-RANDOM.
         05 WS-ACTIVE-MIN PIC 99 VALUE 1.
         05 WS-ACTIVE-MAX PIC 99 VALUE 6.
         05 WS-DEACTIVE-MIN PIC 99 VALUE 7.
         05 WS-DEACTIVE-MAX PIC 99 VALUE 99.
         05 WS-MAX PIC 99 VALUE 99.
         05 WS-MIN PIC 99 VALUE 99.
         05 WS-FULL PIC 9(4) VALUE 0.
         05 WS-MAX-MOVIES PIC 9 VALUE 6.
         05 WS-ISNEW PIC X(4).
       01 WS-MSGS.
         05 ACTIVE PIC X(24) VALUE "  ACTIVATE VENDOR (Y/N):".
         05 DEACT PIC X(24) VALUE "DEACTIVATE VENDOR (Y/N):".
         05 FULL-MOVIE PIC X(24) VALUE "    ACTIVE VENDORS FULL.".
         05 SUCCESS-UPDATE PIC X(24) VALUE "MOVIE HAS BEEN UPDATED!".
         05 ERR-MSG PIC X(40).

       SCREEN SECTION.
       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS230".
           10 COL 30 VALUE "MOVIE THEATER SYSTEM".
           10 COL 70 PIC Z9 FROM WS-MONTH.
           10 COL 72 VALUE "/".
           10 COL 73 PIC Z9 FROM WS-DAY.
           10 COL 75 VALUE "/".
           10 COL 76 PIC 9999 FROM WS-YEAR.

         05 VENDORS-TITLE.
           10 LINE 2 COL 19
           VALUE "MOVIE RENTALS AND SCHEDULING: UPDATE MOVIES".        
         05 SCH-FUNCTION.
           10 LINE 25 COL 1 VALUE "F1 = HELP     F3 = END     ".
           10 COL 27 VALUE " F4 = RETURN     F12 = CANCEL".

       01 GET-THE-DATA BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
       01 MSG.
         05 ERR-2 FOREGROUND-COLOR 4.
           10 LINE 22 COL 9 PIC X(14) VALUE "ERROR MESSAGE:".
           10 LINE 23 COL 16 PIC X(42) VALUE "INVALID COMMAND:".
           10 COL 60 PIC X TO WS-CONFIRM.
         05 ERR-3 FOREGROUND-COLOR 4.
           10 LINE 22 COL 9 PIC X(14) VALUE "INVALID FIELD:".
           10 LINE 23 COL 16 PIC X(30) VALUE "MOVIE DOES NOT EXIST".
           10 COL 60 PIC X TO WS-CONFIRM.
         05 SUCCESS-ID FOREGROUND-COLOR 2.
           10 LINE 22 COL 9 PIC X(23) VALUE "MOVIE HAS BEEN UPDATED!".

         05 CONFIRM-UPDATE.
           10 LINE 23 COL 16 PIC X(19) VALUE "CONFIRM UPDATE? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05 CONFIRM-EXIT.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM EXIT? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05 HACKEY.
           10 LINE 23 PIC X(80) VALUE SPACES.

       01 SCR-ID.
         05 LINE 5 COL 20 PIC X(10) VALUE "VENDOR ID:".
         05 COL 32 PIC X(2) TO MRS-VENDOR-NO REVERSE-VIDEO.
         05 LINE 6 COL 20 PIC X(10) VALUE "MOVIE ID:".
         05 COL 32 PIC X(4) TO MRS-MOVIE-NO REVERSE-VIDEO.

       01 SCR-REC.
         05 SCR1-R6.
           10 LINE 6 COL 25 VALUE "VENDOR ID: ".
           10 COL 36 PIC X(2) FROM MRS-VENDOR-NO.
         05 SCR1-R6.
           10 LINE 7 COL 26 VALUE "MOVIE ID: ".
           10 COL 36 PIC X(4) FROM MRS-MOVIE-NO.
         05 SCR1-R7.
           10 LINE 8 COL 24 VALUE "MOVIE NAME:".
           10 COL 36 PIC X(20) USING MRS-MOVIE-NAME REVERSE-VIDEO.
         05 SCR1-R8.
           10 LINE 9 COL 16 VALUE "PRODUCTION COMPANY:".
           10 COL 36 PIC X(15) FROM MRS-PRODUCTION-CO.
         05 SCR1-R9.
           10 LINE 10 COL 25 VALUE "DIRECTORS:".
           10 COL 36 PIC X(20) USING MRS-DIRECTORS REVERSE-VIDEO.
         05 SCR1-R10.
           10 LINE 11 COL 28 VALUE "RATING:".
           10 COL 36 PIC X(4) USING MRS-RATING REVERSE-VIDEO.
         05 SCR1-R11.
           10 LINE 12 COL 29 VALUE "GENRE:".
           10 COL 36 PIC X(20) USING MRS-GENRE REVERSE-VIDEO.
         05 SCR1-R12.
           10 LINE 13 COL 23 VALUE "RENTAL COST:".
           10 COL 36 PIC S9(4)V99 USING MRS-RENTAL-COST REVERSE-VIDEO.
         05 SCR1-R13.
           10 LINE 14 COL 21 VALUE "ACTIVE FLAG:".
           10 COL 36 PIC X USING MRS-ACTIVE-FLAG REVERSE-VIDEO.
         05  SCR1-R15.
           10  LINE 15 COL 23 VALUE "DESCRIPTION:".
           10  LINE 16 COL 32 PIC X(40) FROM WV-DES1 REVERSE-VIDEO.
           10  LINE 17 COL 32 PIC X(40) FROM WV-DES2 REVERSE-VIDEO.
           10  LINE 18 COL 32 PIC X(40) FROM WV-DES3 REVERSE-VIDEO.
           10  LINE 19 COL 32 PIC X(40) FROM WV-DES4 REVERSE-VIDEO.
           10  LINE 20 COL 32 PIC X(40) FROM WV-DES5 REVERSE-VIDEO.


       procedure division.
       100-MAIN.
           PERFORM 900-INIT THRU 900-END

           PERFORM 300-SEARCH THRU 300-END 
               UNTIL (WS-UPDATED = 'Y' OR F3 OR F4)
           CLOSE MRS-MOVIE-FILE
           GOBACK.
       100-END.
           EXIT.

       300-SEARCH.
           DISPLAY CLEAR
           DISPLAY SCR-ID
           ACCEPT SCR-ID
           MOVE MRS-MOVIE-NO TO WS-ISNEW
      *CHANGE TO UNTIL FOUND
           PERFORM 500-COMPARE-ID THRU 500-END
           IF WS-MOVIE-FOUND EQUALS "Y"
               DISPLAY CLEAR
               DISPLAY SCR-REC
               ACCEPT SCR-REC
               DISPLAY CONFIRM-UPDATE
               ACCEPT CONFIRM-UPDATE
               IF WS-CONFIRM = "Y"
                   IF WS-ISNEW IS NOT EQUAL TO MRS-MOVIE-NO
                       WRITE MRS-MOVIE-REC
                   ELSE
                       REWRITE MRS-MOVIE-REC
                   END-IF
                   MOVE SUCCESS-UPDATE TO ERR-MSG
                   DISPLAY SUCCESS-ID
               END-IF
           ELSE
               DISPLAY ERR-3
               ACCEPT ERR-3
               DISPLAY CLEAR
           END-IF
           DISPLAY HACKEY
           DISPLAY CONFIRM-EXIT
           ACCEPT CONFIRM-EXIT
           IF WS-CONFIRM EQUALS "Y"
               MOVE 'Y' TO WS-UPDATED
           END-IF.
      * IF NOT FOUND DISPLAY ERR-3
       300-END.
           EXIT.

       325-NUM-ACTIVE.
           ADD 1 TO WS-FULL.
           MOVE WS-FULL TO MRS-MOVIE-NO
           PERFORM 500-COMPARE-ID THRU 500-END.
       325-EXIT.
           EXIT.

       500-COMPARE-ID.
           READ MRS-MOVIE-FILE KEY IS MRS-MOVIE-KEY
               INVALID KEY
                   MOVE "N" TO WS-MOVIE-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-MOVIE-FOUND
                   MOVE MRS-MOVIE-REC TO MOVIE
                   MOVE DESCRIPTION TO WV-DESCRIPTION
           END-READ.
       500-END.
           EXIT.

       900-INIT.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           MOVE SPACE TO WS-UPDATED
           OPEN I-O MRS-MOVIE-FILE.
           DISPLAY CLEAR.
       900-END.
           EXIT.
       end program MRS_2300.