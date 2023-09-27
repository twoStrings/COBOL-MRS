       program-id. MRS-2100.


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
               SELECT MRS-VENDOR-FILE
                   ASSIGN TO UT-SYS-DETAILFILE
                   ORGANIZATION IS INDEXED
                   ACCESS IS DYNAMIC 
                   RECORD KEY IS MRS-VENDOR-ID.
       data division.
       FILE SECTION.
       COPY "./CPYBOOKS/MRS-MOVIE.CPY".
       COPY "./CPYBOOKS/MRS-VENDOR.CPY".
       working-storage section.
       COPY "./CPYBOOKS/FUNCTION-KEYS.CPY".
       01  WORKING-VARIABLES.
           05  WV-VENDOR-VALID    PIC X.
           05  WV-VEN-EOF         PIC X.
           05  WV-ENTER           PIC X.
           05  WV-DESCRIPTION.
               10  WV-DES1        PIC X(40).
               10  WV-DES2        PIC X(40).
               10  WV-DES3        PIC X(40).
               10  WV-DES4        PIC X(40).
               10  WV-DES5        PIC X(40).

       01 WS-FILENAMES.
         05 UT-SYS-MSTERFILE PIC X(50)
           VALUE "C:\COBOL\MRS-MOVIE-INDEX.dat".
         05 UT-SYS-DETAILFILE PIC X(50)
           VALUE "C:\COBOL\VENDOR-INDEXED.DAT".

       01 WORKING-OUTPUT.
          05  WO-MOVIE-KEY.
              10 WO-VENDOR-NO                     PIC X(2).
              10 WO-MOVIE-NO                      PIC X(4).
          05 WO-MOVIE-NAME                        PIC X(20).
          05 WO-PRODUCTION-CO                     PIC X(15).
          05 WO-DIRECTORS                         PIC X(20).
          05 WO-RATING                            PIC X(4).
          05 WO-GENRE                             PIC X(20).
          05 WO-DESCRIPTION                       PIC X(200).
          05 WO-RENTAL-COST                       PIC S9(4)V99.
          05 WO-ACTIVE-FLAG                       PIC X.

       01 WS-SEARCH.
         05 ERRMSG PIC X(30).
         05 WS-QUERY PIC X(20).
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
         05 SUCCESS-ADDED PIC X(40) VALUE "MOVIE HAS BEEN ADDED!".
         05 ERR-MSG PIC X(42).


       SCREEN SECTION.
       01 CLEAR BLANK SCREEN PROMPT AUTO REQUIRED BACKGROUND-COLOR 0 
       FOREGROUND-COLOR 7.
         05 MV-TITLE-LINE.
           10 LINE 1 COL 1 VALUE "MRS210".
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

       01 SCR-DETAIL.
         05 SCR1-R8.
           10 LINE 8 COL 19 VALUE "MOVIE NAME:".
           10 COL 32 PIC X(20) TO WO-MOVIE-NAME REVERSE-VIDEO.
         05 SCR1-R9.
           10 LINE 9 COL 11 VALUE "PRODUCTION COMPANY:".
           10 COL 32 PIC X(15) FROM WO-PRODUCTION-CO.
         05 SCR1-R10.
           10 LINE 10 COL 20 VALUE "DIRECTORS:".
           10 COL 32 PIC X(20) TO WO-DIRECTORS REVERSE-VIDEO.
         05 SCR1-R11.
           10 LINE 11 COL 23 VALUE "RATING:".
           10 COL 32 PIC X(4) TO WO-RATING REVERSE-VIDEO.
         05 SCR1-R12.
           10 LINE 12 COL 24 VALUE "GENRE:".
           10 COL 32 PIC X(20) TO WO-GENRE REVERSE-VIDEO.
         05 SCR1-R13.
           10 LINE 13 COL 18 VALUE "RENTAL COST:".
           10 COL 32 PIC S9(4)V99 TO WO-RENTAL-COST REVERSE-VIDEO.
         05 SCR1-R14.
           10 LINE 14 COL 18 VALUE "ACTIVE FLAG:".
           10 COL 32 PIC X FROM WO-ACTIVE-FLAG.
         05  SCR1-R15.
           10  LINE 15 COL 18 VALUE "DESCRIPTION:".
           10          COL 32 PIC X(40) TO WV-DES1 REVERSE-VIDEO.
           10  LINE 16 COL 32 PIC X(40) TO WV-DES2 REVERSE-VIDEO.
           10  LINE 17 COL 32 PIC X(40) TO WV-DES3 REVERSE-VIDEO.
           10  LINE 18 COL 32 PIC X(40) TO WV-DES4 REVERSE-VIDEO.
           10  LINE 19 COL 32 PIC X(40) TO WV-DES5 REVERSE-VIDEO.


       01 MSG.
         05 ERR-ID FOREGROUND-COLOR 4.
           10 LINE 22 COL 9 PIC X(20) VALUE "ERROR MESSAGE:".
           10 LINE 23 COL 16 PIC X(42) FROM ERR-MSG.
           10 COL 60 PIC X TO WS-CONFIRM.
         05 SUCCESS-ID FOREGROUND-COLOR 2.
           10 LINE 22 COL 9 PIC X(42) FROM ERR-MSG.
           10 COL 60 PIC X TO WS-CONFIRM.

         05 CONFIRM-ADD.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM ADD? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
         05  VENDOR-ERROR   FOREGROUND-COLOR 4.
           10  LINE 23 COL 1 BLANK LINE.
           10          COL 10  VALUE "VENDOR CODE WAS INVALID.".
           10  LINE 24 COL 1 BLANK LINE.
           10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
           10          COL 70 PIC X TO WV-ENTER.

         05 CONFIRM-EXIT.
           10 LINE 23 COL 16 PIC X(17) VALUE "CONFIRM EXIT? Y/N".
           10 REVERSE-VIDEO COL 38 PIC X TO WS-CONFIRM.
       procedure division.

       100-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE SPACE TO WS-ADDED
           OPEN I-O MRS-MOVIE-FILE
           OPEN I-O MRS-VENDOR-FILE
           DISPLAY CLEAR
           ACCEPT CLEAR
           MOVE MRS-VENDOR-NO TO WO-VENDOR-NO
           MOVE MRS-MOVIE-NO TO WO-MOVIE-NO
           PERFORM 200-ADD THRU 200-END
               UNTIL (WS-ADDED = 'Y' OR F3 OR F4)

           MOVE 'N' TO WS-CONFIRM
           MOVE 'N' TO WS-ADDED

           CLOSE MRS-MOVIE-FILE
           CLOSE MRS-VENDOR-FILE
           goback.
       100-END.
           EXIT.
       
       200-ADD.
           MOVE MRS-VENDOR-NO TO MRS-VENDOR-ID
           PERFORM 250-COMPARE-ID THRU 250-END
           IF WS-MOVIE-FOUND EQUALS "N"
               PERFORM 300-CREATE THRU 300-END
               IF WS-CONFIRM EQUALS "Y"
                   MOVE "Y" TO WS-ADDED
               END-IF
               IF WS-CONFIRM EQUALS "N"
                   DISPLAY CLEAR
                   ACCEPT CLEAR
                   MOVE MRS-VENDOR-NO TO WO-VENDOR-NO
                   MOVE MRS-MOVIE-NO TO WO-MOVIE-NO
               END-IF
           ELSE
               MOVE DNE TO ERR-MSG
               DISPLAY ERR-ID
               ACCEPT ERR-ID

               DISPLAY CLEAR
               ACCEPT CLEAR
               MOVE MRS-VENDOR-NO TO WO-VENDOR-NO
               MOVE MRS-MOVIE-NO TO WO-MOVIE-NO
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

       300-CREATE.
           PERFORM 400-CHECK-VENDOR THRU 400-EXIT  

           IF(WV-VENDOR-VALID = "Y")
               MOVE "N" TO MRS-ACTIVE-FLAG
               MOVE "N" TO WO-ACTIVE-FLAG
               DISPLAY SCR-DETAIL
               ACCEPT SCR-DETAIL
               IF(F3)
                   MOVE "Y" TO WS-CONFIRM
               ELSE
                   DISPLAY CONFIRM-ADD
                   ACCEPT CONFIRM-ADD
               END-IF
           END-IF
           IF(WV-VENDOR-VALID = "N")
               DISPLAY VENDOR-ERROR
               ACCEPT VENDOR-ERROR
               MOVE "N" TO WS-CONFIRM
           END-IF
               IF (WS-CONFIRM = 'Y' AND NOT F3)
                   MOVE WV-DESCRIPTION TO WO-DESCRIPTION
                   WRITE MRS-MOVIE-REC FROM WORKING-OUTPUT
                   MOVE SUCCESS-ADDED TO ERR-MSG
                   DISPLAY SUCCESS-ID
                   DISPLAY CONFIRM-EXIT
                   ACCEPT CONFIRM-EXIT
               END-IF.
       300-END.
           EXIT.

       400-CHECK-VENDOR.
           MOVE MRS-VENDOR-NO TO MRS-VENDOR-ID
           READ MRS-VENDOR-FILE KEY IS MRS-VENDOR-ID
               INVALID KEY
                   MOVE "Y" TO WV-VEN-EOF
                   MOVE "N" TO WV-VENDOR-VALID
               NOT INVALID KEY
                   MOVE "Y" TO WV-VENDOR-VALID
                   MOVE MRS-VENDOR-COMPANY TO WO-PRODUCTION-CO
           END-READ.
       400-EXIT.
           EXIT.

