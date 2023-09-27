       program-id. MRS_4300.
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

       COPY "DATETIME.CPY".
   
       01 WS-SWITCHES.
         05 WS-CONFIRM PIC X.
         05 WS-RENTAL-FOUND PIC X.
         05 WS-UPDATED PIC X.

         05 WS-STATE PIC X.
           88 WS-ACTIVE VALUE "A".
           88 WS-DEACTIVE VALUE "D".
       01 RENTAL.
         05 RENT-ID  PIC X(6).
         05 MOVIE-ID PIC 9(4).
         05 COPY-ID PIC 99.
         05 START-DATE PIC X(8).
         05 END-DATE PIC X(8).
         05 SUBTOTAL PIC 9(4)V99.
         05 JOURNAL-NUM PIC X(10).
         05 SCHEDULE-FLAG PIC X.
         05 RETURN-FLAG PIC X.
        

       01 WS-RANDOM.
         05 WS-ACTIVE-MIN PIC 99 VALUE 1.
         05 WS-ACTIVE-MAX PIC 99 VALUE 6.
         05 WS-DEACTIVE-MIN PIC 99 VALUE 7.
         05 WS-DEACTIVE-MAX PIC 99 VALUE 99.
         05 WS-MAX PIC 99 VALUE 99.
         05 WS-MIN PIC 99 VALUE 99.
         05 WS-FULL PIC 9(6) VALUE 0.
         05 WS-MAX-MOVIES PIC 9 VALUE 6.
         05 WS-ISNEW PIC 9(6).

       01 WS-MSGS.
         05 NOT-FOUND PIC X(24) VALUE "RENTAL DOES NOT EXIST!".
         05 SUCCESS-UPDATE PIC X(24) VALUE "RENTAL HAS BEEN UPDATED!".
         05 ERR-MSG PIC X(40).

      

       SCREEN SECTION.
       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS430".
           10 COL 30 VALUE "MOVIE THEATER SYSTEM".
           10 COL 70 PIC Z9 FROM WS-MONTH.
           10 COL 72 VALUE "/".
           10 COL 73 PIC Z9 FROM WS-DAY.
           10 COL 75 VALUE "/".
           10 COL 76 PIC 9999 FROM WS-YEAR.

         05 VENDORS-TITLE.
           10 LINE 2 COL 19
           VALUE "MOVIE RENTALS AND SCHEDULING: UPDATE RENTAL".        
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
           10 LINE 23 COL 16 PIC X(30) VALUE "RENTAL DOES NOT EXIST".
           10 COL 60 PIC X TO WS-CONFIRM.
         05 SUCCESS-ID FOREGROUND-COLOR 2.
           10 LINE 22 COL 9 PIC X(23) VALUE "RENTAL HAS BEEN UPDATED!".

         05 CONFIRM-UPDATE.
           10 LINE 23 COL 16 PIC X(19) VALUE "CONFIRM UPDATE? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05 CONFIRM-EXIT.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM EXIT? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05 HACKEY.
           10 LINE 23 PIC X(80) VALUE SPACES.
       01 SCR-ID.

         05 LINE 4 COL 6 PIC X(20) VALUE "ENTER A VALID ID: ".
         05 REVERSE-VIDEO COL 29 PIC X(6) TO MRS-RENT-ID.
       01 SCR-VIEW.
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
           10 COL 36 PIC S9(5)V99 FROM MRS-SUBTOTAL.
         05 SCR1-R12.
           10 LINE 12 COL 20 VALUE "JOURNAL NUMBER:".
           10 COL 36 PIC X(10) FROM MRS-JOURNAL-NUMBER.
         05 SCR1-R13.
           10 LINE 13 COL 21 VALUE "SCHEDULE FLAG:".
           10 COL 36 PIC X FROM MRS-READY-TO-SCHEDULE-FLAG.
         05 SCR1-R14.
           10 LINE 14 COL 23 VALUE "PAID FLAG:".
           10 COL 36 PIC X FROM MRS-RETURN-FLAG.

       01 SCR-UPDATE.
         05 LINE 6 COL 25 PIC X(10) VALUE "RENTAL ID:".
         05 COL 36 PIC X(6) FROM MRS-RENT-ID.
         05 SCR1-R7.
           10 LINE 7 COL 26 VALUE "MOVIE ID:".
           10 COL 36 PIC 9(4) FROM MRS-MOVIE-ID.
         05 SCR1-R8.
           10 LINE 8 COL 27 VALUE "COPY ID:".
           10 COL 36 PIC 99 USING MRS-COPY-ID REVERSE-VIDEO.
         05 SCR1-R9.
           10 LINE 9 COL 24 VALUE "START DATE:".
           10 COL 36 PIC X(8) USING MRS-START-DATE REVERSE-VIDEO.
         05 SCR1-R10.
           10 LINE 10 COL 26 VALUE "END DATE:".
           10 COL 36 PIC X(8) USING MRS-END-DATE REVERSE-VIDEO.
         05 SCR1-R11.
           10 LINE 11 COL 26 VALUE "SUBTOTAL:".
           10 COL 36 PIC S9(5)V99 USING MRS-SUBTOTAL REVERSE-VIDEO.
         05 SCR1-R12.
           10 LINE 12 COL 20 VALUE "JOURNAL NUMBER:".
           10 COL 36 PIC X(10) USING
           MRS-JOURNAL-NUMBER REVERSE-VIDEO.
         05 SCR1-R13.
           10 LINE 13 COL 21 VALUE "SCHEDULE FLAG:".
           10 COL 36 PIC X USING 
           MRS-READY-TO-SCHEDULE-FLAG REVERSE-VIDEO.
         05 SCR1-R14.
           10 LINE 14 COL 23 VALUE "PAID FLAG:".
           10 COL 36 PIC X USING MRS-RETURN-FLAG REVERSE-VIDEO.





       procedure division.
       100-MAIN.
           PERFORM 900-INIT THRU 900-END
           PERFORM 300-SEARCH THRU 300-END
               UNTIL (WS-UPDATED = 'Y' OR F3 OR F4)
           CLOSE MRS-RENTAL-FILE
           GOBACK.
       100-END.
           EXIT.

       300-SEARCH.
           DISPLAY CLEAR
           DISPLAY SCR-ID
           ACCEPT SCR-ID.
           MOVE MRS-RENT-ID TO WS-ISNEW
      *CHANGE TO UNTIL FOUND
           PERFORM 500-COMPARE-ID THRU 500-END.
           IF WS-RENTAL-FOUND EQUALS "Y"
               DISPLAY SCR-UPDATE
               ACCEPT SCR-UPDATE
               PERFORM 350-PARTIAL-CHECK THRU 350-END
               DISPLAY CLEAR
               DISPLAY SCR-VIEW
               DISPLAY CONFIRM-UPDATE
               ACCEPT CONFIRM-UPDATE
               IF WS-CONFIRM = 'Y'
                   IF WS-ISNEW IS NOT EQUAL TO MRS-RENT-ID
                       WRITE MRS-RENTAL-REC FROM RENTAL
                   ELSE
                       REWRITE MRS-RENTAL-REC FROM RENTAL
                   END-IF
                   MOVE SUCCESS-UPDATE TO ERR-MSG
                   DISPLAY SUCCESS-ID

               END-IF
           ELSE
               DISPLAY ERR-3
               ACCEPT ERR-3
               DISPLAY CLEAR
           END-IF.
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
           MOVE WS-FULL TO MRS-RENT-ID
           PERFORM 500-COMPARE-ID THRU 500-END.
       325-EXIT.
           EXIT.
       350-PARTIAL-CHECK.
           IF MOVIE-ID EQUALS SPACES
               MOVE MRS-MOVIE-ID TO MOVIE-ID
           END-IF.
           IF COPY-ID EQUALS SPACES
               MOVE MRS-COPY-ID TO COPY-ID
           END-IF
           IF START-DATE EQUALS SPACES
               MOVE MRS-START-DATE TO START-DATE
           END-IF
           IF END-DATE EQUALS SPACES
               MOVE MRS-END-DATE TO END-DATE
           END-IF.
           IF SUBTOTAL EQUALS SPACES
               MOVE MRS-SUBTOTAL TO SUBTOTAL
           END-IF.
           IF JOURNAL-NUM EQUALS SPACES
               MOVE MRS-JOURNAL-NUMBER TO JOURNAL-NUM
           END-IF.
           IF SCHEDULE-FLAG EQUALS SPACES
               MOVE MRS-READY-TO-SCHEDULE-FLAG TO SCHEDULE-FLAG
           END-IF.
           IF RETURN-FLAG EQUALS SPACES
               MOVE MRS-RETURN-FLAG TO RETURN-FLAG
           END-IF.
       350-END.
           EXIT.

    
      

       500-COMPARE-ID.
           MOVE MRS-RENT-ID TO RENT-ID
           READ MRS-RENTAL-FILE KEY IS MRS-RENT-ID
               INVALID KEY
                   MOVE "N" TO WS-RENTAL-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-RENTAL-FOUND
           END-READ.
       500-END.
           EXIT.

       900-INIT.
      *    COPY "ENABLE-KEYS".
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           MOVE SPACE TO WS-UPDATED
           OPEN I-O MRS-RENTAL-FILE.
           DISPLAY CLEAR.
       900-END.
           EXIT.
       end program MRS_4300.