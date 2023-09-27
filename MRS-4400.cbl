       program-id. MRS-4400.

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

       01 WS-CURRENT-DATE.
         05 WS-YEAR PIC 9(4).
         05 WS-MONTH PIC 9(2).
         05 WS-DAY PIC 9(2).
       01 WS-SWITCHES.
         05 WS-COMMAND PIC 9.
         05 WS-VIEW PIC X.
         05 OPTIONS.
           10 OPT-ID PIC 9 VALUE 1.
           10 OPT-NAME PIC 9 VALUE 2.
           10 OPT-NUM PIC 9 VALUE 3.
           10 OPT-EMAIL PIC 9 VALUE 4.
           10 OPT-ALL PIC 9 VALUE 5.
           10 OPT-ACTION PIC 9 VALUE 6.


         05 WS-RENTAL-FOUND PIC X.
         05 WS-CONFIRM PIC X.
         05 WS-SELECTED PIC A(6).

       01 WS-SEARCH.
         05 ERRMSG PIC X(30).
         05 WS-QUERY PIC X(20).
   
       SCREEN SECTION.
       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS440".
           10 COL 30 VALUE "MOVIE THEATER SYSTEM".
           10 COL 70 PIC Z9 FROM WS-MONTH.
           10 COL 72 VALUE "/".
           10 COL 73 PIC Z9 FROM WS-DAY.
           10 COL 75 VALUE "/".
           10 COL 76 PIC 9999 FROM WS-YEAR.

         05 VENDORS-TITLE.
           10 LINE 2 COL 19
           VALUE "MOVIE RENTALS AND SCHEDULING: RENTAL".               
         05 MV-HELP.
           10 LINE 25 COL 1 VALUE "F1 = HELP     F3 = END     ".
           10 COL 27 VALUE " F4 = RETURN     F12 = CANCEL".

       01 SCR2.
         05 LINE 4 COL 25 PIC X(23) VALUE "1. SEARCH RENTALS BY ID".
         05 LINE 6 COL 25 PIC X(25) VALUE "2. SEARCH RENTALS BY NAME".
         05 LINE 8 COL 25 PIC X(27) VALUE "3. SEARCH RENTALS BY NUMBER".
         05 LINE 10 COL 25 PIC X(26) VALUE "4. SEARCH RENTALS BY EMAIL".
         05 LINE 12 COL 25 PIC X(23) VALUE "5. VIEW ALL THE RENTALS".
         05 LINE 14 COL 25 PIC X(40)
         VALUE "6. PERFORM ADD / UPDATE / DELETE / OTHER".              
         05 LINE 20 COL 25 PIC X(28)
         VALUE "ENTER A COMMAND 1 THROUGH 6:".                          
         05 REVERSE-VIDEO COL 57 PIC X TO WS-COMMAND.
       01 MSG.
         05 ERR-2 FOREGROUND-COLOR 4.
           10 LINE 22 COL 9 PIC X(14) VALUE "ERROR MESSAGE:".
           10 LINE 23 COL 16 PIC X(42)
           VALUE "INVALID COMMAND: MUST BE AN INTEGER 1 TO 6".          
           10 COL 60 PIC X TO WS-CONFIRM.
         05 ERR-3 FOREGROUND-COLOR 4.
           10 LINE 22 COL 9 PIC X(14) VALUE "INVALID FIELD:".
           10 LINE 23 COL 16 PIC X(30) VALUE "RENTAL DOES NOT EXIST".
           10 COL 60 PIC X TO WS-CONFIRM.

         05 CONFIRM.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM EXIT? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
       01 SCR-ID.

         05 LINE 4 COL 6 PIC X(20) VALUE "ENTER A VALID ID: ".
         05 REVERSE-VIDEO COL 29 PIC 9(6) TO MRS-RENT-ID.
       01 SCR-REC.
         05 SCR1-R6.
           10 LINE 6 COL 25 VALUE "RENTAL ID: ".
           10 COL 36 PIC X(6) FROM MRS-RENT-ID.
         05 SCR1-R7.
           10 LINE 7 COL 26 VALUE "MOVIE ID:".
           10 COL 36 PIC X(4) FROM MRS-MOVIE-ID.
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

       procedure division.
       100-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE SPACE TO WS-VIEW
           OPEN I-O MRS-RENTAL-FILE
           DISPLAY CLEAR
           PERFORM 300-SEARCH UNTIL WS-VIEW = 'Y'
           CLOSE MRS-RENTAL-FILE
           GOBACK
           EXIT program.
       100-END.
           EXIT.

       300-SEARCH.
           DISPLAY CLEAR
           DISPLAY SCR-ID
           ACCEPT SCR-ID
           PERFORM 350-COMPARE-ID THRU 350-END
           IF WS-RENTAL-FOUND EQUALS "Y"
               DISPLAY SCR-REC
           ELSE
               DISPLAY ERR-3
               ACCEPT ERR-3
               DISPLAY CLEAR
           END-IF
           DISPLAY CONFIRM
           ACCEPT CONFIRM
           IF WS-CONFIRM EQUALS "Y"
               MOVE "Y" TO WS-VIEW

           END-IF
           MOVE 'N' TO WS-RENTAL-FOUND.
      
       300-END.

       350-COMPARE-ID.
           READ MRS-RENTAL-FILE KEY IS MRS-RENT-ID
      *        INVALID KEY MOVE "N" TO WS-RENTAL-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-RENTAL-FOUND
           END-READ.
       350-END.

       end program MRS-4400.
