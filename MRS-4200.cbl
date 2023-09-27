       program-id. MRS4200.


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
 

       01 WS-FILENAMES.
         05 UT-SYS-MSTERFILE PIC X(50) VALUE "C:\COBOL\MRS-RENTAL.DAT".
         05 UT-SYS-DETAILFILE PIC X(50)
           VALUE "C:\COBOL\MRS-MOVIE-INDEX.DAT".

         05 WS-CONFIRM PIC X.
       01 WS-SEARCH.
         05 ERRMSG PIC X(30).
         05 WS-QUERY PIC X(20).
         05 WS-RENTAL-FOUND PIC X.
         05 WS-DELETED PIC X.

       01 WS-CURRENT-DATE.
         05 WS-YEAR PIC 9(4).
         05 WS-MONTH PIC 9(2).
         05 WS-DAY PIC 9(2).

       01 WS-MSG.
         05 DNE PIC X(42) VALUE "INVALID ID: DOES NOT EXIST".
         05 SUCCESS-DELETE PIC X(42) VALUE "RENTAL HAS BEEN DELETED!".
         05 RENTAL-NOT-FOUND PIC X(42) VALUE "RENTAL NOT FOUND!".
         05 ERR-MSG PIC X(42).


     
       SCREEN SECTION.
       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS420".
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

       01 SCR-DEL.
         05 LINE 2 COL 20
         VALUE "MOVIE RENTALS AND SCHEDULING: DELETE MOVIE".            
         05 LINE 22 COL 9 PIC X(20) VALUE "ENTER ALL THE FIELDS".
         05 LINE 5 COL 20 PIC X(10) VALUE "RENTAL ID:".
         05 COL 32 PIC 9(6) TO MRS-RENT-ID REVERSE-VIDEO.
       01 SCR-DETAIL.
         05 LINE 2 COL 20
         VALUE "MOVIE RENTALS AND SCHEDULING: DELETE MOVIE".            
         05 LINE 22 COL 9 PIC X(23) VALUE "VERIFY RECORD TO DELETE".
         05 SCR1-R6.
           10 LINE 6 COL 25 VALUE "RENTAL ID: ".
           10 COL 36 PIC X(6) FROM MRS-RENT-ID.
         05 SCR1-R7.
           10 LINE 7 COL 26 VALUE "MOVIE ID:".
           10 COL 36 PIC 9(4) FROM MRS-MOVIE-ID.
         05 SCR1-R8.
           10 LINE 8 COL 27 VALUE "COPY ID:".
           10 COL 36 PIC 99 FROM MRS-COPY-ID.
         05 SCR1-R9.
           10 LINE 9 COL 24 VALUE "START DATE:".
           10 COL 36 PIC X(8) FROM MRS-START-DATE.
         05 SCR1-R10.
           10 LINE 10 COL 26 VALUE "END DATE:".
           10 COL 36 PIC X(8) FROM MRS-END-DATE.
         05 SCR1-R11.
           10 LINE 11 COL 26 VALUE "SUBTOTAL:".
           10 COL 36 PIC $ZZ,ZZ9.99 FROM MRS-SUBTOTAL.
         05 SCR1-R12.
           10 LINE 12 COL 20 VALUE "JOURNAL NUMBER:".
           10 COL 36 PIC X(10) FROM MRS-JOURNAL-NUMBER.
         05 SCR1-R13.
           10 LINE 13 COL 21 VALUE "SCHEDULE FLAG:".
           10 COL 36 PIC X FROM 
           MRS-READY-TO-SCHEDULE-FLAG.
         05 SCR1-R14.
           10 LINE 14 COL 23 VALUE "RETURN FLAG:".
           10 COL 36 PIC X FROM MRS-RETURN-FLAG.

       01 MSG.
         05 ERR-ID FOREGROUND-COLOR 4.
           10 LINE 22 COL 9 PIC X(20) VALUE "ERROR MESSAGE:".
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
           OPEN I-O MRS-RENTAL-FILE
           DISPLAY CLEAR

           PERFORM 200-DELETE-RENTAL THRU 200-EXIT
               UNTIL (WS-DELETED = 'Y' OR F3 OR F4)
           MOVE 'N' TO WS-DELETED
           CLOSE MRS-RENTAL-FILE
           GOBACK.
       100-END.
           EXIT.

       200-DELETE-RENTAL.
           DISPLAY SCR-DEL
           ACCEPT SCR-DEL

           PERFORM 250-COMPARE-ID THRU 250-EXIT
           IF WS-RENTAL-FOUND EQUALS "Y"
               PERFORM 300-DELETE THRU 300-EXIT
               IF WS-CONFIRM EQUALS "Y"
                   MOVE "Y" TO WS-DELETED
               END-IF
               IF WS-CONFIRM EQUALS "N" 
                   DISPLAY CLEAR
                   DISPLAY SCR-DEL
               END-IF
           ELSE
               MOVE RENTAL-NOT-FOUND TO ERR-MSG
               DISPLAY ERR-ID
               ACCEPT ERR-ID
               DISPLAY CLEAR
               DISPLAY CONFIRM-EXIT
               ACCEPT CONFIRM-EXIT
               IF WS-CONFIRM EQUALS "Y"
                   MOVE "Y" TO WS-DELETED
               END-IF
           END-IF.
       200-EXIT.
           EXIT.

       250-COMPARE-ID.
           READ MRS-RENTAL-FILE KEY IS MRS-RENT-ID
               INVALID KEY
                   MOVE "N" TO WS-RENTAL-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-RENTAL-FOUND
           END-READ.
       250-EXIT.
           EXIT.

       300-DELETE.
           DISPLAY CLEAR
           DISPLAY SCR-DETAIL
           DISPLAY CONFIRM-DELETE
           ACCEPT CONFIRM-DELETE
           IF WS-CONFIRM = "Y"
               DELETE MRS-RENTAL-FILE
               END-DELETE
               MOVE SUCCESS-DELETE TO ERR-MSG
               DISPLAY SUCCESS-ID
               DISPLAY CONFIRM-EXIT
               ACCEPT CONFIRM-EXIT
           END-IF.
       300-EXIT.
           EXIT.