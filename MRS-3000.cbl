       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MRS-3000.
       AUTHOR.  JOHN BELLEK.
      *****************************************************************
      * This will display and accept the movie scheduling menu
      * It can be called from the subsystem main menu
      * 
      * 
      *  
      * 
      *****************************************************************

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
          CURSOR IS CRPT
          CRT STATUS IS SCR-STAT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "CPYBOOKS/FUNCTION-KEYS.CPY".
       COPY "CPYBOOKS/DATETIME.CPY".

       01  WORKING-VARIABLES.
           05  WV-ENTER               PIC X.

       LINKAGE SECTION.
       01  LS-OPTION    PIC 9.

       SCREEN SECTION.
       01  MOVIE-SCHEDULING-SCREEN          BLANK SCREEN
                                            PROMPT
                                            AUTO
                                            REQUIRED
                                            BACKGROUND-COLOR 0
                                            FOREGROUND-COLOR 7.
           05  SCH-TITLE-LINE.
               10  LINE 1 COL 1            VALUE "MRS300".
               10         COL 30           VALUE "MOVIE THEATER SYSTEM".
               10         COL 70           PIC Z9 FROM WS-MONTH.
               10         COL 72           VALUE "/".
               10         COL 73           PIC Z9 FROM WS-DAY.
               10         COL 75           VALUE "/".
               10         COL 76           PIC 9999 FROM WS-YEAR.

           05  SCHEDULE-TITLE.
               10  LINE 2 COL 21
                   VALUE "MOVIE RENTALS AND SCHEDULING: MOVIE SCHEDULE".
           05  MAINTAIN-SCHEDULE.
               10  LINE 4 COL 25  VALUE "1. ADD SCHEDULE".
               10  LINE 6 COL 25  VALUE "2. DELETE SCHEDULE".
               10  LINE 8 COL 25  VALUE "3. UPDATE SCHEDULE".
               10  LINE 10 COL 25 VALUE "4. VIEW SCHEDULE".

           05  SCH-INPUT.
               10  LINE 20 COL 25 VALUE "ENTER OPTION:     ".
               10          COL 40 PIC 9 TO LS-OPTION.

           05 SCH-FUNCTION.
             10  LINE 25   COL 1  VALUE "F1 = HELP     F3 = END     ".
             10            COL 27 VALUE " F4 = RETURN".
       01  SCH-MESSAGES.
           05  SCH-HELP               FOREGROUND-COLOR 3.
               10 LINE 22 COL 10 VALUE "ENTER A NUMBER BETWEEN 1 AND 4".
               10 LINE 23 COL 10 VALUE "OR HIT F4 TO GO BACK.".
               10 LINE 24 COL 40 VALUE "PRESS ENTER TO CONTINUE".
               10         COL 65 PIC X TO WV-ENTER.

           05  SCH-ERROR               FOREGROUND-COLOR 4.
               10 LINE 22 COL 10 VALUE "INVALID OPTION. PLEASE ENTER A".
               10         COL 35 VALUE "NUMBER BETWEEN 1 AND 4".
               10 LINE 24 COL 40 VALUE "PRESS ENTER TO CONTINUE".
               10         COL 65 PIC X TO WV-ENTER.

       procedure division
          USING LS-OPTION.

           COPY "CPYBOOKS/ENABLE-KEYS.CPY".
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME
          DISPLAY MOVIE-SCHEDULING-SCREEN
          ACCEPT MOVIE-SCHEDULING-SCREEN
           
           PERFORM 200-SCREENS THRU 200-EXIT
              UNTIL (F3 OR F4)
           goback.
       100-EXIT.
           exit.

      * This function will check to see if the number entered was a 
      * valid option or not. If not then it will display an error
      * message.
       200-SCREENS.
           IF (LS-OPTION = 0 OR LS-OPTION > 4)
               DISPLAY SCH-ERROR
               ACCEPT SCH-ERROR
           ELSE
               PERFORM 300-EVALUATION THRU 300-EXIT
           END-IF

           DISPLAY MOVIE-SCHEDULING-SCREEN
           ACCEPT MOVIE-SCHEDULING-SCREEN.
       200-EXIT.
           exit.

      * This function will look at what option the user entered and then
      * display the corrisponding screen.
       300-EVALUATION.
               EVALUATE LS-OPTION
                   WHEN 1 
                 CALL 'MRS-3100'
                   WHEN 2
                 CALL 'MRS-3200'
                   WHEN 3
                 CALL 'MRS-3300'
                   WHEN 4
                 CALL 'MRS-3400'
                   WHEN OTHER
                       DISPLAY SCH-ERROR
           END-EVALUATE.
       300-EXIT.
       EXIT.
           
           
       end program MRS-3000.