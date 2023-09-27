       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MRS-3400.
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
               10  WV-SHOW-1.
                   15  SHOW-1-HOUR   PIC 99.
                   15  SHOW-1-MIN    PIC 99.
               10  WV-SHOW-2.
                   15  SHOW-2-HOUR   PIC 99.
                   15  SHOW-2-MIN    PIC 99.
               10  WV-SHOW-3.
                   15  SHOW-3-HOUR   PIC 99.
                   15  SHOW-3-MIN    PIC 99.
               10  WV-SHOW-4.
                   15  SHOW-4-HOUR   PIC 99.
                   15  SHOW-4-MIN    PIC 99.
               10  WV-SHOW-5.
                   15  SHOW-5-HOUR   PIC 99.
                   15  SHOW-5-MIN    PIC 99.
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
       

      * File path(s)
           05  UT-SYS-MRS-SCH         PIC X(50)
                                   VALUE "C:\COBOL\MRS-SCH-INDEX.dat".


       SCREEN SECTION.
       01  MOVIE-SCHEDULE-VIEW              BLANK SCREEN
                                            PROMPT
                                            AUTO
                                            REQUIRED
                                            BACKGROUND-COLOR 0
                                            FOREGROUND-COLOR 7.
           05  MV-TITLE-LINE.
               10  LINE 1 COL 1            VALUE "MRS340".
               10         COL 30           VALUE "MOVIE THEATER SYSTEM".
               10         COL 70           PIC Z9 FROM WS-MONTH.
               10         COL 72           VALUE "/".
               10         COL 73           PIC Z9 FROM WS-DAY.
               10         COL 75           VALUE "/".
               10         COL 76           PIC 9999 FROM WS-YEAR.

           05  SCHEDULE-ADD-TITLE.
               10  LINE 2 COL 17
                   VALUE "MOVIE RENTALS AND SCHEDULING: ".
               10         COL 47 VALUE "MOVIE SCHEDULE VIEW".
               
           05  CHECK-ID.
               10  LINE 6 COL 17 VALUE "SCREEN NUMBER:".
               10  LINE 6 COL 32 PIC 9 TO WV-SCR-NUM REVERSE-VIDEO.

           05 SCH-FUNCTION.
             10  LINE 25   COL 1  VALUE "F1 = HELP     F3 = END     ".
             10            COL 27 VALUE " F4 = RETURN".

       01  SCHEDULE-VIEW.
           05  VIEW-SCHEDULE.
               10  LINE 6 COL 1 BLANK LINE.
               10         COL 22 VALUE "MOVIE ID:".
               10  LINE 7 COL 19  VALUE "MOVIE TITLE:".
               10  LINE 8 COL 19  VALUE "VENDOR NAME:".
               10  LINE 9 COL 17  VALUE "SCREEN NUMBER:".
               10  LINE 10 COL 20 VALUE "SHOW TIMES:".
               10  LINE 15 COL 19 VALUE "SEAT NUMBER:".
               10  LINE 16 COL 24 VALUE "RATING:".
               10  LINE 17 COL 19 VALUE "DESCRIPTION:".

           05  VIEW-GET-DATA.
               10  LINE 6 COL 32 PIC 9999 FROM WI-MOVIE-ID.
               10  LINE 7 COL 32  PIC X(20) FROM WI-MOVIE-NAME.
               10  LINE 8 COL 32  PIC X(15) FROM WI-MOVIE-VENDOR.
               10  LINE 9 COL 32  PIC 9 FROM WI-SCREEN-NUMBER.
               10  LINE 10 COL 32 PIC 99 FROM SHOW-1-HOUR.
               10          COL 34 PIC X VALUE ":".
               10          COL 35 PIC 99 FROM SHOW-1-MIN.
               10  LINE 11 COL 32 PIC 99 FROM SHOW-2-HOUR.
               10          COL 34 PIC X VALUE ":".
               10          COL 35 PIC 99 FROM SHOW-2-MIN.
               10  LINE 12 COL 32 PIC 99 FROM SHOW-3-HOUR.
               10          COL 34 PIC X VALUE ":".
               10          COL 35 PIC 99 FROM SHOW-3-MIN.
               10  LINE 13 COL 32 PIC 99 FROM SHOW-4-HOUR.
               10          COL 34 PIC X VALUE ":".
               10          COL 35 PIC 99 FROM SHOW-4-MIN.
               10  LINE 14 COL 32 PIC 99 FROM SHOW-5-HOUR.
               10          COL 34 PIC X VALUE ":".
               10          COL 35 PIC 99 FROM SHOW-5-MIN.
               10  LINE 15 COL 32 PIC 99 FROM WI-SEATS.
               10  LINE 16 COL 32 PIC X(4) FROM WI-RATING.
               10  LINE 17 COL 32 PIC X(40) FROM WV-DES-1.
               10  LINE 18 COL 32 PIC X(40) FROM WV-DES-2.
               10  LINE 19 COL 32 PIC X(40) FROM WV-DES-3.
               10  LINE 20 COL 32 PIC X(40) FROM WV-DES-4.
               10  LINE 21 COL 32 PIC X(40) FROM WV-DES-5.

       01  MESSAGE-BOX.
           05  VIEW-FAILED            FOREGROUND-COLOR 4.
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10 VALUE "MOVIE WAS NOT FOUND".
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  MOVIE-VIEW-AGAIN       FOREGROUND-COLOR 7.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "WOULD YOU LIKE TO VIEW".
               10          COL 34 VALUE "ANOTHER RECORD:  Y/N".
               10          COL 60 PIC X TO WV-AGAIN REVERSE-VIDEO.
               10  LINE 24 COL 1 BLANK LINE.

           05  MOVIE-NOT-FOUND            FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "THERE IS NO MOVIE FOR THIS".
               10          COL 36 VALUE " SCREEN NUMBER.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.


       procedure division.
       100-MAIN.
           OPEN I-O MRS-SCH-INFO-FILE
           COPY "CPYBOOKS/ENABLE-KEYS.CPY".
           MOVE FUNCTION current-date TO WS-DATETIME
           MOVE SPACES TO WV-AGAIN
           PERFORM 200-DISPLAY THRU 200-EXIT
           UNTIL (WV-AGAIN EQUALS "N")

           CLOSE MRS-SCH-INFO-FILE.
           GOBACK.

       100-EXIT.
           exit.

      *This will display the movie information in the schedule file
       200-DISPLAY.
           DISPLAY MOVIE-SCHEDULE-VIEW
           ACCEPT MOVIE-SCHEDULE-VIEW

           if(F3 OR F4)
               MOVE "N" TO WV-AGAIN
           else
               PERFORM UNTIL (SCH-EOF = "Y")
                   MOVE WV-SCR-NUM TO MRS-SCH-SCREEN-NUMBER
                   READ MRS-SCH-INFO-FILE KEY IS MRS-SCH-SCREEN-NUMBER
                   INVALID
                       MOVE "Y" TO SCH-EOF
                       DISPLAY MOVIE-NOT-FOUND
                       ACCEPT MOVIE-NOT-FOUND
                   NOT INVALID KEY
                       MOVE MRS-SCH-INFO-REC TO WORKING-INPUT
                       MOVE WI-SHOW-TIME TO WV-SHOW-TIMES
                       MOVE WI-DESCRIPTION TO WV-DESCRIPTION
                       MOVE "Y" TO SCH-EOF
                       DISPLAY SCHEDULE-VIEW
                   end-read
               END-PERFORM

               MOVE "N" TO SCH-EOF

               DISPLAY MOVIE-VIEW-AGAIN
               ACCEPT MOVIE-VIEW-AGAIN
           end-if.
       200-EXIT.
           EXIT.


       end program MRS-3400.