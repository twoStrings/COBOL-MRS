       program-id. MRS-4100.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT MRS-RENTAL-FILE
                   ASSIGN TO UT-SYS-MSTERFILE
                   ORGANIZATION IS INDEXED
                   ACCESS IS DYNAMIC
                   RECORD KEY IS MRS-RENT-ID.
               SELECT MRS-MOVIE-FILE
                   ASSIGN TO UT-SYS-DETAILFILE
                   ORGANIZATION IS INDEXED
                   ACCESS IS DYNAMIC
                   RECORD KEY IS MRS-MOVIE-KEY.
       data division.
       FILE SECTION.
         COPY "./CPYBOOKS/MRS-MOVIE.CPY".
         COPY "./CPYBOOKS/MRS-RENTAL.CPY".
       working-storage section.
       COPY "./CPYBOOKS/FUNCTION-KEYS.CPY".
    
       01 WS-MOVIE-ID PIC 9(4).
       01 WS-FILENAMES.
         05 UT-SYS-MSTERFILE PIC X(50) VALUE "C:\COBOL\MRS-RENTAL.DAT".
         05 UT-SYS-DETAILFILE PIC X(50)
           VALUE "C:\COBOL\MRS-MOVIE-INDEX.DAT".

       01 WS-SEARCH.
         05 ERRMSG PIC X(30).
         05 WS-QUERY PIC X(20).
         05 WS-RENTAL-FOUND PIC X.
         05 WS-MOVIE-FOUND PIC X.
         05 WS-CONFIRM PIC X.
         05 WS-ADDED PIC X.
       01 WS-CURRENT-DATE.
         05 WS-YEAR PIC 9(4).
         05 WS-MONTH PIC 9(2).
         05 WS-DAY PIC 9(2).

       01 WS-MSG.
         05 DNE PIC X(42) VALUE "INVALID ID: ALREADY EXISTS".
         05 INACTIVE PIC X(40) VALUE "INVALID ID:".
         05 SUCCESS-ADDED PIC X(40) VALUE "RENTAL HAS BEEN ADDED!".
         05 RENTAL-EXISTS PIC X(40) VALUE "RENTAL ALREADY EXISTS!".
         05 MOVIE-NOT-FOUND PIC X(40)
           VALUE "MOVIE DOES NOT EXITS!".
         05 ERR-MSG PIC X(42).


       SCREEN SECTION.
       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS410".
           10 COL 30 VALUE "MOVIE THEATER SYSTEM".
           10 COL 70 PIC 99 FROM WS-MONTH.
           10 COL 72 VALUE "/".
           10 COL 73 PIC 99 FROM WS-DAY.
           10 COL 75 VALUE "/".
           10 COL 76 PIC 9999 FROM WS-YEAR.

         05 VENDORS-TITLE.
          10 LINE 2 COL 19 VALUE "MOVIE RENTALS AND SCHEDULING: RENTAL".
         05 SCH-FUNCTION.
           10 LINE 25 COL 1 VALUE "F1 = HELP     F3 = END     ".
           10 COL 27 VALUE " F4 = RETURN     F12 = CANCEL".

       01 GET-THE-DATA BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.

       01 SCR-ADD.
         05 LINE 2 COL 20
         VALUE "MOVIE RENTALS AND SCHEDULING: ADD RENTAL".              
         05 LINE 22 COL 9 PIC X(20) VALUE "ENTER ALL THE FIELDS".
         05 LINE 5 COL 20 PIC X(10) VALUE "VENDOR ID:".
         05 COL 32 PIC X(2) TO MRS-VENDOR-NO REVERSE-VIDEO.
         05 LINE 6 COL 21 PIC X(10) VALUE "MOVIE ID:".
         05 COL 32 PIC X(4) TO MRS-MOVIE-NO REVERSE-VIDEO.

       01 SCR-DETAIL.
         05 LINE 2 COL 20
           VALUE "MOVIE RENTALS AND SCHEDULING: ADD RENTAL".            
         05 LINE 22 COL 9 PIC X(20) VALUE "ENTER ALL THE FIELDS".
         05 SCR1-R6.
           10 LINE 6 COL 25 VALUE "RENTAL ID: ".
           10 COL 36 PIC X(6) FROM MRS-RENT-ID.
         05 SCR1-R7.
           10 LINE 7 COL 26 VALUE "MOVIE ID:".
           10 COL 36 PIC 9(4) FROM MRS-MOVIE-NO.
         05 SCR1-R8.
           10 LINE 8 COL 27 VALUE "COPY ID:".
           10 COL 36 PIC 99 TO MRS-COPY-ID REVERSE-VIDEO.
         05 SCR1-R9.
           10 LINE 9 COL 24 VALUE "START DATE:".
           10 COL 36 PIC X(8) TO MRS-START-DATE REVERSE-VIDEO.
         05 SCR1-R10.
           10 LINE 10 COL 26 VALUE "END DATE:".
           10 COL 36 PIC X(8) TO MRS-END-DATE REVERSE-VIDEO.
         05 SCR1-R11.
           10 LINE 11 COL 26 VALUE "SUBTOTAL:".
           10 COL 36 PIC S9(5)V99 TO MRS-SUBTOTAL REVERSE-VIDEO.
         05 SCR1-R12.
           10 LINE 12 COL 20 VALUE "JOURNAL NUMBER:".
           10 COL 36 PIC X(10) FROM MRS-JOURNAL-NUMBER.
         05 SCR1-R13.
           10 LINE 13 COL 21 VALUE "SCHEDULE FLAG:".
           10 COL 36 PIC X TO MRS-READY-TO-SCHEDULE-FLAG REVERSE-VIDEO.
         05 SCR1-R14.
           10 LINE 14 COL 25 VALUE "PAID FLAG:".
           10 COL 36 PIC X FROM MRS-RETURN-FLAG.

       01 SCR-MOVIE.
         05 LINE 22 COL 9 PIC X(28)
         VALUE "THE SELECTED MOVIE IS ABOVE!".
         05 SCR1-R6.
           10 LINE 6 COL 25 VALUE "VENDOR ID: ".
           10 COL 36 PIC X(2) FROM MRS-VENDOR-NO.
         05 SCR1-R6.
           10 LINE 7 COL 26 VALUE "MOVIE ID: ".
           10 COL 36 PIC X(4) FROM MRS-MOVIE-NO.
         05 SCR1-R7.
           10 LINE 8 COL 24 VALUE "MOVIE NAME:".
           10 COL 36 PIC X(30) FROM MRS-MOVIE-NAME.
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


       01 MSG.
         05 ERR-ID FOREGROUND-COLOR 4.
           10 LINE 22 COL 9 PIC X(20) VALUE "ERROR MESSAGE:".
           10 LINE 23 COL 16 PIC X(42) FROM ERR-MSG.
           10 COL 60 PIC X TO WS-CONFIRM.
         05 CONFIRM-ADD.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM ADD? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05 SUCCESS-ID FOREGROUND-COLOR 2.
           10 LINE 22 COL 9 PIC X(42) FROM ERR-MSG.
         05 CONFIRM-EXIT.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM EXIT? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05 CONTINUE-ADD.
           10 LINE 23 COL 16 PIC X(23) VALUE "PRESS ENTER TO CONTINUE".
           10 COL 60 PIC X TO WS-CONFIRM.

       procedure division.

       100-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE SPACE TO WS-ADDED
           OPEN I-O MRS-RENTAL-FILE
           OPEN I-O MRS-MOVIE-FILE
           DISPLAY CLEAR
           PERFORM 200-ADD THRU 200-END
               UNTIL (WS-ADDED = 'Y' OR F3 OR F4)
           CLOSE MRS-RENTAL-FILE
           CLOSE MRS-MOVIE-FILE
           GOBACK.
       100-END.
           EXIT.

       200-ADD.
           DISPLAY CLEAR
           DISPLAY SCR-ADD
           ACCEPT SCR-ADD
           PERFORM 250-COMPARE-MOVIE-ID THRU 250-END
           IF WS-MOVIE-FOUND EQUALS "Y"
               PERFORM 400-VIEW-MOVIE THRU 400-EXIT
               MOVE MRS-MOVIE-KEY TO MRS-RENT-ID
               PERFORM 270-COMPARE-RENTAL-ID THRU 270-END
               IF WS-RENTAL-FOUND EQUALS "N"
                   PERFORM 300-CREATE THRU 300-EXIT
                   IF (WS-CONFIRM EQUALS "Y" OR F3 OR F4)
                       MOVE "Y" TO WS-ADDED
                   END-IF
               ELSE
                   MOVE RENTAL-EXISTS TO ERR-MSG
                   DISPLAY ERR-ID
                   ACCEPT ERR-ID
                   DISPLAY CLEAR
                   PERFORM 450-CONFIRM-EXIT THRU 450-EXIT
               END-IF
           ELSE
               MOVE MOVIE-NOT-FOUND TO ERR-MSG
               DISPLAY ERR-ID
               ACCEPT ERR-ID
               DISPLAY CLEAR
               PERFORM 450-CONFIRM-EXIT THRU 450-EXIT
           END-IF.
       200-END.
           EXIT.

       250-COMPARE-MOVIE-ID.
           READ MRS-MOVIE-FILE KEY IS MRS-MOVIE-KEY
               INVALID KEY
                   MOVE "N" TO WS-MOVIE-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-MOVIE-FOUND
           END-READ.
       250-END.
           EXIT.

       270-COMPARE-RENTAL-ID.
           READ MRS-RENTAL-FILE KEY IS MRS-RENT-ID
               INVALID KEY
                   MOVE "N" TO WS-RENTAL-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-RENTAL-FOUND
           END-READ.
       270-END.
           EXIT.

       300-CREATE.
           MOVE MRS-MOVIE-KEY TO MRS-RENT-ID
           MOVE ZEROES TO MRS-JOURNAL-NUMBER
           MOVE 'N' TO MRS-RETURN-FLAG
           DISPLAY CLEAR

           DISPLAY SCR-DETAIL
           ACCEPT SCR-DETAIL

           MOVE MRS-MOVIE-NO TO MRS-MOVIE-ID
           PERFORM 460-CONFIRM-ADD THRU 460-EXIT
           IF WS-CONFIRM = 'Y'
               WRITE MRS-RENTAL-REC
               MOVE SUCCESS-ADDED TO ERR-MSG
               DISPLAY SUCCESS-ID
           END-IF
           PERFORM 450-CONFIRM-EXIT THRU 450-EXIT.
       300-EXIT.
           EXIT.

       400-VIEW-MOVIE.
           DISPLAY CLEAR
           DISPLAY SCR-MOVIE
           DISPLAY CONTINUE-ADD
           ACCEPT CONTINUE-ADD
           DISPLAY CLEAR.
       400-EXIT.
           EXIT.

       450-CONFIRM-EXIT.
           IF(F3)
               MOVE "Y" TO WS-CONFIRM
           ELSE
               DISPLAY CONFIRM-EXIT
               ACCEPT CONFIRM-EXIT
           END-IF
           IF WS-CONFIRM EQUALS "Y"
               MOVE 'Y' TO WS-ADDED
           ELSE
               EXIT
           END-IF.
       450-EXIT.
           EXIT.

       460-CONFIRM-ADD.
           IF(F3)
               MOVE "Y" TO WS-CONFIRM
           ELSE
               DISPLAY CONFIRM-ADD
               ACCEPT CONFIRM-ADD
           END-IF
           IF WS-CONFIRM EQUALS "Y"
               MOVE 'Y' TO WS-ADDED

           ELSE
               EXIT
           END-IF.
       460-EXIT.
           EXIT.
