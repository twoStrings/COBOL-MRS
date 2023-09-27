       program-id. MRS-2400.

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
         05  WV-DESCRIPTION.
             10  WV-DES1   PIC X(40).
             10  WV-DES2   PIC X(40).
             10  WV-DES3   PIC X(40).
             10  WV-DES4   PIC X(40).
             10  WV-DES5   PIC X(40).


         05 WS-MOVIE-FOUND PIC X.
         05 WS-CONFIRM PIC X.
         05 WS-SELECTED PIC A(6).

       01 WS-SEARCH.
         05 ERRMSG PIC X(30).
         05 WS-QUERY PIC X(20).

       SCREEN SECTION.
       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS240".
           10 COL 30 VALUE "MOVIE THEATER SYSTEM".
           10 COL 70 PIC Z9 FROM WS-MONTH.
           10 COL 72 VALUE "/".
           10 COL 73 PIC Z9 FROM WS-DAY.
           10 COL 75 VALUE "/".
           10 COL 76 PIC 9999 FROM WS-YEAR.

         05 VENDORS-TITLE.
           10 LINE 2 COL 19
           VALUE "MOVIE RENTALS AND SCHEDULING: MOVIES".               
         05 MV-HELP.
           10 LINE 25 COL 1 VALUE "F1 = HELP     F3 = END     ".
           10 COL 27 VALUE " F4 = RETURN     F12 = CANCEL".

       01 SCR2.
         05 LINE 4 COL 25 PIC X(23) VALUE "1. SEARCH VENDORS BY ID".
         05 LINE 6 COL 25 PIC X(25) VALUE "2. SEARCH VENDORS BY NAME".
         05 LINE 8 COL 25 PIC X(27) VALUE "3. SEARCH VENDORS BY NUMBER".
         05 LINE 10 COL 25 PIC X(26) VALUE "4. SEARCH VENDORS BY EMAIL".
         05 LINE 12 COL 25 PIC X(23) VALUE "5. VIEW ALL THE VENDORS".
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
           10 LINE 23 COL 16 PIC X(30) VALUE "MOVIE DOES NOT EXIST".
           10 COL 60 PIC X TO WS-CONFIRM.

         05 CONFIRM.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM EXIT? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
       01 SCR-ID.

         05 LINE 5 COL 20 PIC X(10) VALUE "VENDOR ID:".
         05 COL 32 PIC X(2) TO MRS-VENDOR-NO REVERSE-VIDEO.
         05 LINE 6 COL 21 PIC X(10) VALUE "MOVIE ID:".
         05 COL 32 PIC X(4) TO MRS-MOVIE-NO REVERSE-VIDEO.
       01 SCR-REC.
         05 SCR1-R6.
           10 LINE 6 COL 25 VALUE "VENDOR ID: ".
           10 COL 36 PIC X(2) FROM MRS-VENDOR-NO REVERSE-VIDEO.
         05 SCR1-R6.
           10 LINE 7 COL 26 VALUE "MOVIE ID: ".
           10 COL 36 PIC X(4) FROM MRS-MOVIE-NO REVERSE-VIDEO.
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
         05  SCR1-R15.
           10  LINE 15 COL 23 VALUE "DESCRIPTION:".
           10  LINE 16 COL 32 PIC X(40) FROM WV-DES1.
           10  LINE 17 COL 32 PIC X(40) FROM WV-DES2.
           10  LINE 18 COL 32 PIC X(40) FROM WV-DES3.
           10  LINE 19 COL 32 PIC X(40) FROM WV-DES4.
           10  LINE 20 COL 32 PIC X(40) FROM WV-DES5. 


       procedure division.
       100-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE SPACE TO WS-VIEW
           OPEN I-O MRS-MOVIE-FILE
           DISPLAY CLEAR
           PERFORM 300-SEARCH UNTIL WS-VIEW = 'Y'
           CLOSE MRS-MOVIE-FILE
           GOBACK
           EXIT program.
       100-END.
           EXIT.



       300-SEARCH.
           DISPLAY CLEAR
           DISPLAY SCR-ID
           ACCEPT SCR-ID
           PERFORM 350-COMPARE-ID THRU 350-END
           IF WS-MOVIE-FOUND EQUALS "Y"
               MOVE MRS-DESCRIPTION TO WV-DESCRIPTION
               DISPLAY CLEAR
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
           MOVE 'N' TO WS-MOVIE-FOUND.
      * IF NOT FOUND DISPLAY ERR-3
       300-END.

       350-COMPARE-ID.
           READ MRS-MOVIE-FILE KEY IS MRS-MOVIE-KEY
      *        INVALID KEY MOVE "N" TO WS-VEND-FOUND
               NOT INVALID KEY
                   MOVE "Y" TO WS-MOVIE-FOUND
           END-READ.
       350-END.

       end program MRS-2400.
