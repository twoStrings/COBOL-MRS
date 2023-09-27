       program-id. MRS2200.


       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
           CURSOR IS CRPT
          CRT STATUS IS SCR-STAT.

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

       01  WORKING-VARIABLES.
         05 WV-DESCRIPTION.
            10  WV-DES1    PIC X(40).
            10  WV-DES2    PIC X(40).
            10  WV-DES3    PIC X(40).
            10  WV-DES4    PIC X(40).
            10  WV-DES5    PIC X(40).

         05 WS-CONFIRM PIC X.

       01 WS-SEARCH.
         05 ERRMSG PIC X(30).
         05 WS-QUERY PIC X(20).
         05 WS-MOVIE-FOUND PIC X.
         05 WS-DELETED PIC X.

       01 WS-CURRENT-DATE.
         05 WS-YEAR PIC 9(4).
         05 WS-MONTH PIC 9(2).
         05 WS-DAY PIC 9(2).

       01 WS-MSG.
         05 DNE PIC X(42) VALUE "INVALID ID: DOES NOT EXIST".
         05 SUCCESS-DELETE PIC X(42) VALUE "MOVIE HAS BEEN DELETED!".
         05 ERR-MSG PIC X(42).


     
       SCREEN SECTION.
       01 CLEAR-SCREEN
       BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS220".
           10 COL 30 VALUE "MOVIE THEATER SYSTEM".
           10 COL 70 PIC 99 FROM WS-MONTH.
           10 COL 72 VALUE "/".
           10 COL 73 PIC 99 FROM WS-DAY.
           10 COL 75 VALUE "/".
           10 COL 76 PIC 9999 FROM WS-YEAR.

         05 VENDORS-TITLE.
          10 LINE 2 COL 19 VALUE "MOVIE RENTALS AND SCHEDULING: MOVIES".
         05 SCH-FUNCTION.
           10 LINE 25 COL 1 VALUE "F1 = HELP     F3 = END     ".
           10 COL 27 VALUE " F4 = RETURN     F12 = CANCEL".


       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS220".
           10 COL 30 VALUE "MOVIE THEATER SYSTEM".
           10 COL 70 PIC 99 FROM WS-MONTH.
           10 COL 72 VALUE "/".
           10 COL 73 PIC 99 FROM WS-DAY.
           10 COL 75 VALUE "/".
           10 COL 76 PIC 9999 FROM WS-YEAR.

         05 VENDORS-TITLE.
          10 LINE 2 COL 19 VALUE "MOVIE RENTALS AND SCHEDULING: MOVIES".
         05 SCH-FUNCTION.
           10 LINE 25 COL 1 VALUE "F1 = HELP     F3 = END     ".
           10 COL 27 VALUE " F4 = RETURN     F12 = CANCEL".

           05 LINE 2 COL 20
         VALUE "MOVIE RENTALS AND SCHEDULING: ADD MOVIE".               
         05 LINE 22 COL 9 PIC X(20) VALUE "ENTER ALL THE FIELDS".
         05 LINE 5 COL 20 PIC X(10) VALUE "VENDOR ID:".
         05 COL 32 PIC X(2) TO MRS-VENDOR-NO REVERSE-VIDEO.
         05 LINE 6 COL 21 PIC X(10) VALUE "MOVIE ID:".
         05 COL 32 PIC X(4) TO MRS-MOVIE-NO REVERSE-VIDEO.

       01 GET-THE-DATA BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.

       01 SCR-DEL.
         05 LINE 2 COL 20
         VALUE "MOVIE RENTALS AND SCHEDULING: DELETE MOVIE".            
         05 LINE 22 COL 9 PIC X(20) VALUE "ENTER ALL THE FIELDS".
         05 LINE 5 COL 20 PIC X(10) VALUE "VENDOR ID:".
         05 COL 32 PIC X(2) TO MRS-VENDOR-NO REVERSE-VIDEO.
         05 LINE 6 COL 21 PIC X(10) VALUE "MOVIE ID:".
         05 COL 32 PIC X(4) TO MRS-MOVIE-NO REVERSE-VIDEO.
       01 SCR-DETAIL.
         05 LINE 2 COL 20
         VALUE "MOVIE RENTALS AND SCHEDULING: DELETE MOVIE".            
         05 LINE 22 COL 9 PIC X(23) VALUE "VERIFY RECORD TO DELETE".
         05 SCR1-R6.
           10 LINE 6 COL 25 VALUE "VENDOR ID: ".
           10 COL 36 PIC X(2) FROM MRS-VENDOR-NO.
         05 SCR1-R6.
           10 LINE 7 COL 26 VALUE "MOVIE ID: ".
           10 COL 36 PIC X(4) FROM MRS-MOVIE-NO.
         05 SCR1-R7.
           10 LINE 8 COL 24 VALUE "MOVIE NAME:".
           10 COL 36 PIC X(20) FROM MRS-MOVIE-NAME.
         05 SCR1-R8.
           10 LINE 9 COL 16 VALUE "PRODUCTION COMPANY:".
           10 COL 36 PIC X(15) FROM MRS-PRODUCTION-CO.
         05 SCR1-R9.
           10 LINE 10 COL 25 VALUE "DIRECTORS:".
           10 COL 36 PIC X(20) FROM MRS-DIRECTORS.
         05 SCR1-R10.
           10 LINE 11 COL 28 VALUE "RATING:".
           10 COL 36 PIC X(4) FROM MRS-RATING.
         05 SCR1-R11.
           10 LINE 12 COL 29 VALUE "GENRE:".
           10 COL 36 PIC X(20) FROM MRS-GENRE.
         05 SCR1-R12.
           10 LINE 13 COL 23 VALUE "RENTAL COST:".
           10 COL 36 PIC $ZZ,ZZ9.99 FROM MRS-RENTAL-COST.
         05 SCR1-R13.
           10 LINE 14 COL 23 VALUE "ACTIVE FLAG:".
           10 COL 36 PIC X FROM MRS-ACTIVE-FLAG.
         05  SCR1-R15.
           10  LINE 15 COL 23 VALUE "DESCRIPTION:".
           10  LINE 16 COL 36 PIC X(40) FROM WV-DES1.
           10  LINE 17 COL 36 PIC X(40) FROM WV-DES2.
           10  LINE 18 COL 36 PIC X(40) FROM WV-DES3.
           10  LINE 19 COL 36 PIC X(40) FROM WV-DES4.
           10  LINE 20 COL 36 PIC X(40) FROM WV-DES5. 

       01 MSG.
         05 ERR-ID FOREGROUND-COLOR 4.
           10 LINE 23 COL 16 PIC X(42) FROM ERR-MSG.
           10 COL 60 PIC X TO WS-CONFIRM.
         05 SUCCESS-ID FOREGROUND-COLOR 2.
           10 LINE 22 COL 9 PIC X(42) FROM ERR-MSG.

         05 CONFIRM-DELETE.
           10 LINE 23 COL 16 PIC X(19) VALUE "CONFIRM DELETE? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05 CONFIRM-EXIT.
           10 LINE 23 COL 16 PIC X(19) VALUE "CONFIRM EXIT? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.

       procedure division.

       100-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE SPACE TO WS-DELETED
           OPEN I-O MRS-MOVIE-FILE
           DISPLAY CLEAR

           PERFORM 200-DELETE-MOVIE THRU 200-END
               UNTIL (WS-DELETED = 'Y' OR F3 OR F4)
           MOVE 'N' TO WS-DELETED
           CLOSE MRS-MOVIE-FILE
           GOBACK.
       100-END.
           EXIT.

       200-DELETE-MOVIE.
           DISPLAY SCR-DEL
           ACCEPT SCR-DEL

           PERFORM 250-COMPARE-ID THRU 250-END
           IF WS-MOVIE-FOUND EQUALS "Y"
               PERFORM 300-DELETE THRU 300-END
               IF WS-CONFIRM EQUALS "Y"
                   MOVE "Y" TO WS-DELETED
               END-IF
               IF WS-CONFIRM EQUALS "N"
                   DISPLAY CLEAR
                   ACCEPT CLEAR
               END-IF
           ELSE
               MOVE DNE TO ERR-MSG
               DISPLAY ERR-ID
               ACCEPT ERR-ID
      *        DISPLAY CLEAR-SCREEN
           END-IF.

       200-END.
           EXIT.

       250-COMPARE-ID.
           READ MRS-MOVIE-FILE KEY IS MRS-MOVIE-KEY
               INVALID KEY
                   MOVE "N" TO WS-MOVIE-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-MOVIE-FOUND
                   MOVE MRS-DESCRIPTION TO WV-DESCRIPTION
           END-READ.
       250-END.
           EXIT.

       300-DELETE.
           DISPLAY CLEAR-SCREEN
           DISPLAY SCR-DETAIL
           DISPLAY CONFIRM-DELETE
           ACCEPT CONFIRM-DELETE
           IF WS-CONFIRM = "Y"
               DELETE MRS-MOVIE-FILE
               END-DELETE
               MOVE SUCCESS-DELETE TO ERR-MSG
               DISPLAY SUCCESS-ID
               DISPLAY CONFIRM-EXIT
               ACCEPT CONFIRM-EXIT
           END-IF.
       300-END.
           EXIT.