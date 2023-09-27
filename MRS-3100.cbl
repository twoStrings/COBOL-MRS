
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MRS-3100.
       AUTHOR.  JOHN BELLEK.
      *****************************************************************
      * This will display and accept the movie scheduling menu
      * It can be called from the subsystem main menu
      * 
      * ADD DATA STORE FOR MOVIE AND VENDORS AS WELL AS A DATA STORE FOR
      * THE ACTUAL SCHEDULE THAT I WILL BE MAKING.
      * 
      *****************************************************************

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.
          CURSOR IS CRPT
          CRT STATUS IS SCR-STAT.

       FILE-CONTROL.
           SELECT MRS-MOVIE-FILE
               ASSIGN TO UT-SYS-MRS-MOVIE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MRS-MOVIE-KEY.

           SELECT MRS-SCH-INFO-FILE
               ASSIGN TO UT-SYS-MRS-SCH
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC 
               RECORD KEY IS MRS-SCH-MOVIE-ID
               ALTERNATE KEY IS MRS-SCH-SCREEN-NUMBER.

           SELECT MRS-RENTAL-FILE
               ASSIGN TO UT-SYS-MRS-RENT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MRS-RENT-ID.

           SELECT MRS-VENDOR-FILE
               ASSIGN TO UT-SYS-MRS-VEN
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MRS-VENDOR-ID.

           SELECT MRS-TICKET-INFO
               ASSIGN TO UT-SYS-MRS-TIC
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MRS-TIC-SCREEN-NUMBER.

       DATA DIVISION.
       FILE SECTION.
       COPY "CPYBOOKS/MRS-MOVIE.CPY".
       COPY "CPYBOOKS/MRS-SCH-INFO.CPY".
       COPY "CPYBOOKS/MRS-RENTAL.CPY".
       COPY "CPYBOOKS/MRS-VENDOR.CPY".
       COPY "CPYBOOKS/MRS-TICKET-INFO.CPY".


       WORKING-STORAGE SECTION.
       COPY "CPYBOOKS/FUNCTION-KEYS.CPY".
       COPY "CPYBOOKS/DATETIME.CPY".
      

       01  WORKING-VARIABLES.
           05  WV-ID.
               10  WV-VENDOR-ID         PIC 99.
               10  WV-MOVIE-ID          PIC 9999.
           05  WV-MOVIE-TITLE           PIC X(20).
           05  WV-MOVIE-VENDOR          PIC X(15).
           05  WV-START-TIME            PIC 9999.
           05  WV-RATING                PIC X(4).
           05  WV-DESCRIPTION.
               10  WV-DESC1             PIC X(40).
               10  WV-DESC2             PIC X(40).
               10  WV-DESC3             PIC X(40).
               10  WV-DESC4             PIC X(40).
               10  WV-DESC5             PIC X(40).
           05  WV-SCREEN-NUM            PIC 9.
           05  WV-READY-TO-SCH          PIC X.
           05  WS-VENDOR-EXISTS         PIC X.

       01  WORKING-OUTPUT.
           05  WO-MOVIE-ID              PIC X(4).
           05  WO-MOVIE-TITLE           PIC X(20).
           05  WO-MOVIE-VENDOR          PIC X(15).
           05  WO-SHOW-TIMES            PIC 9(20).
           05  WO-SEAT-NUM              PIC 99 VALUE 40.
           05  WO-RATING                PIC X(4).
           05  WO-DESCRIPTION           PIC X(200).
           05  WO-SCREEN-NUM            PIC 9.

       01  WORKING-MOVIE.
           05  WM-MOVIE-KEY.
               10 WM-VENDOR-NO          PIC X(2).
               10 WM-MOVIE-NO           PIC X(4).
           05 WM-MOVIE-NAME             PIC X(20).
           05 WM-PRODUCTION-CO          PIC X(15).
           05 WM-DIRECTORS              PIC X(20).
           05 WM-RATING                 PIC X(4).
           05 WM-GENRE                  PIC X(20).
           05 WM-DESCRIPTION            PIC X(200).
           05 WM-RENTAL-COST            PIC S9(4)V99.
           05 WM-ACTIVE-FLAG            PIC X.

       01  WORKING-INPUT.
           05  WI-MOVIE-ID              PIC X(4).
           05  WI-MOVIE-TITLE           PIC X(20).
           05  WI-MOVIE-VENDOR          PIC X(15).
           05  WI-SHOW-TIMES            PIC 9(20).
           05  WI-SEAT-NUM              PIC 99 VALUE 40.
           05  WI-RATING                PIC X(4).
           05  WI-DESCRIPTION           PIC X(200).
           05  WI-SCREEN-NUM            PIC 9.

       01  WORKING-TICKET-TABLE.
           05  WT-TICKET-TABLE OCCURS 6 TIMES INDEXED BY TIC-I.
               10  WT-TIC-MOVIE-NAME        PIC X(20).
               10  WT-TIC-SHOW-TIME         PIC 9(20).
               10  WT-TIC-SEATS-AVALIBLE    PIC 99 VALUE 40.
               10  WT-TIC-RATING            PIC X(4).
               10  WT-TIC-SCREEN-NUMBER     PIC 9.

       01  WORKING-TICKET-INPUT.
           05  WTI-MOVIE-NAME           PIC X(20).
           05  WTI-SHOW-TIME            PIC 9(20).
           05  WTI-SEATS-AVALIBLE       PIC 99 VALUE 40.
           05  WTI-RATING               PIC X(4).
           05  WTI-SCREEN-NUMBER        PIC 9.

       01  WORKING-CONFIRM.
           05  WV-CONFIRM               PIC X.
           05  WV-TEMP-TIME             PIC 9999.
           05  WV-CONTINUE              PIC X VALUE "Y".
           05  SCH-EOF                  PIC X.
           05  SCH-SAME-SCR             PIC X.
           05  SCH-EXISTS               PIC X.
           05  VENDOR-EOF               PIC X.
           05  WV-ENTER                 PIC X.
           05  MOVIE-EOF                PIC X.
           05  EOF-RENTAL               PIC X.
           05  TIC-EOF                  PIC X.
           05  WV-TIC-COUNTER           PIC 9 VALUE 1.
           05  WV-SCH-COUNTER           PIC 9 VALUE 0.
           05  WC-SHOW-TIMES.
               10  WC-TIME1             PIC 9999.
               10  WC-TIME2             PIC 9999.
               10  WC-TIME3             PIC 9999.
               10  WC-TIME4             PIC 9999.
               10  WC-TIME5             PIC 9999.
           05  WV-SCH-COUNT             PIC 9 VALUE 0.
           05  WV-SCH-TAB-WRITE         PIC X VALUE 'N'.
           05  WV-TIC-TAB-WRITE         PIC X VALUE 'N'.
           05  WV-SCH-HOLDER            PIC 9 VALUE 1.

       01  WORKING-TABLE.
           05  WT-SCH-TABLE OCCURS 6 TIMES INDEXED BY SCH-I.
               10  WT-SCH-MOVIE-ID      PIC X(4).
               10  WT-SCH-MOVIE-TITLE   PIC X(20).
               10  WT-SCH-MOVIE-VENDOR  PIC X(15).
               10  WT-SCH-SHOW-TIMES    PIC 9(20).
               10  WT-SCH-SEAT-NUM      PIC 99 VALUE 40.
               10  WT-SCH-RATING        PIC X(4).
               10  WT-SCH-DESCRIPTION   PIC X(200).
               10  WT-SCH-SCREEN-NUM    PIC 9.


      * These are the files that are used
           05  UT-SYS-MRS-MOVIE         PIC X(50)
                                 VALUE "C:\COBOL\MRS-MOVIE-INDEX.dat".
           05  UT-SYS-MRS-RENT          PIC X(50)
                                   VALUE "C:\COBOL\MRS-RENTAL.DAT".
           05  UT-SYS-MRS-SCH           PIC X(50)
                                   VALUE "C:\COBOL\MRS-SCH-INDEX.dat".
           05  UT-SYS-MRS-VEN           PIC X(50)
                                   VALUE "C:\COBOL\VENDOR-INDEXED.DAT".
           05  UT-SYS-MRS-TIC           PIC X(50)
                                   VALUE "C:\COBOL\MRS-TICKET-INFO.DAT".



       SCREEN SECTION.
       01  MOVIE-SCHEDULING-SCREEN-ADD      BLANK SCREEN
                                            PROMPT
                                            AUTO
                                            REQUIRED
                                            BACKGROUND-COLOR 0
                                            FOREGROUND-COLOR 7.
           05  SCH-TITLE-LINE.
               10  LINE 1 COL 1            VALUE "MRS310".
               10         COL 30           VALUE "MOVIE THEATER SYSTEM".
               10         COL 70           PIC Z9 FROM WS-MONTH.
               10         COL 72           VALUE "/".
               10         COL 73           PIC Z9 FROM WS-DAY.
               10         COL 75           VALUE "/".
               10         COL 76           PIC 9999 FROM WS-YEAR.

           05  SCHEDULE-ADD-TITLE.
               10  LINE 2 COL 19
                   VALUE "MOVIE RENTALS AND SCHEDULING: ".
               10         COL 49 VALUE "MOVIE SCHEDULE ADD".

           05  CHECK-ID.
               10  LINE 6 COL 20 VALUE "VENDOR ID:".
               10  LINE 6 COL 32 PIC 99 TO WV-VENDOR-ID REVERSE-VIDEO.
               10  LINE 7 COL 21 VALUE "MOVIE ID:".
               10  LINE 7 COL 32 PIC 9999 TO WV-MOVIE-ID REVERSE-VIDEO.
           
           05 SCH-FUNCTION.
             10  LINE 25   COL 1  VALUE "F1 = HELP     F3 = END     ".
             10            COL 27 VALUE " F4 = RETURN     F12 = CLEAR".

       01  GET-THE-DATA              BACKGROUND-COLOR 0
                                     FOREGROUND-COLOR 7.

           05  ADD-SCHEDULE.
               10  LINE 8 COL 19  VALUE "MOVIE TITLE:".
               10  LINE 9 COL 19  VALUE "VENDOR NAME:".
               10  LINE 10 COL 17  VALUE "SCREEN NUMBER:".
               10  LINE 11 COL 20 VALUE "SHOW TIMES:".
               10  LINE 12 COL 19 VALUE "SEAT NUMBER:".
               10  LINE 13 COL 24 VALUE "RATING:".
               10  LINE 14 COL 19 VALUE "DESCRIPTION:".

           05  ADD-GET-DATA.
               10  LINE 8 COL 32  PIC X(20) FROM MRS-MOVIE-NAME.
               10  LINE 9 COL 32  PIC X(15) FROM MRS-PRODUCTION-CO.
               10  LINE 10 COL 32  PIC 9
                                    USING WV-SCREEN-NUM REVERSE-VIDEO.
               10  LINE 11 COL 32  PIC 99 FROM WO-SEAT-NUM.
               10  LINE 12 COL 32  PIC 9999
                                    USING WV-START-TIME REVERSE-VIDEO.
               10  LINE 13 COL 32 PIC X(5) FROM MRS-RATING.
               10  LINE 14 COL 32 PIC X(40) FROM WV-DESC1.
               10  LINE 15 COL 32 PIC X(40) FROM WV-DESC2.
               10  LINE 16 COL 32 PIC X(40) FROM WV-DESC3.
               10  LINE 17 COL 32 PIC X(40) FROM WV-DESC4.
               10  LINE 18 COL 32 PIC X(40) FROM WV-DESC5.

           05  SCH-ADD-CONFIRM.
               10  LINE 22 COL 1 BLANK LINE.
               10          COL 18 VALUE "CONFIRM ADD:  Y/N".
               10          COL 39 PIC X TO WV-CONFIRM REVERSE-VIDEO.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10
                    VALUE "HIT TAB TO MOVE TO THE NEXT FIELD"
                    FOREGROUND-COLOR 3.
          

       01  ADD-MESSAGES.
           05  ADD-DATA-HELP          FOREGROUND-COLOR 3.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 23 COL 10 "PLEASE ENTER A SCREEN NUMBER AND A".
               10          COL 45 VALUE " START TIME".
               10  LINE 24 COL 1 BLANK LINE.
               10  LINE 24 COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  ADD-HELP               FOREGROUND-COLOR 3.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 23 COL 10 "PLEASE ENTER A 4 DIGIT MOVIE ID".
               10  LINE 24 COL 1 BLANK LINE.
               10  LINE 24 COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  ADD-SUCCESS           FOREGROUND-COLOR 2.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "ADD SUCCESSFUL!".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  ADD-FAILED            FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "DATA NOT ADDED".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  ALREADY-EXISTS        FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "MOVIE IS ALREADY ".
               10          COL 27 VALUE "IN THE SCHEDULE".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  SAME-SCREEN                  FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "A MOVIE IS ALREADY SCHEDULED".
               10          COL 38 VALUE " FOR THIS SCREEN.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  AGAIN                        BACKGROUND-COLOR 0
                                            FOREGROUND-COLOR 7.
               10  LINE 23 COL 1 BLANK LINE.
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10 VALUE "WOULD YOU LIKE TO ADD ANOTHER".
               10          COL 40 VALUE "MOVIE TO THE SCHEDULE: Y/N".
               10          COL 71 PIC X TO WV-CONTINUE REVERSE-VIDEO.

           05  EXPIRED                   FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "THE RENTING PERIOD FOR THIS ".
               10          COL 29 VALUE "MOVIE IS EXPIRED.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  NOT-READY                   FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "THIS MOVIE IS NOT READY TO BE".
               10          COL 40 VALUE "SCHEDULED YET.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  DOES-NOT-EXIST              FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "THIS IS AN INVALID MOVIE ID".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  INVALID-SCREEN              FOREGROUND-COLOR 04.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "THAT SCREEN DOES NOT EXIST".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 10 VALUE"SCREENS SHOULD BE BETWEEN".
               10          COL 36 VALUE " 1 AND 6".
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  INVALID-TIME                FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "INVALID TIME WAS ENTETED".
               10          COL 35 VALUE "TIME SHOULD BE BETWEEN 1000".
               10          COL 62 VALUE " AND 1059".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

           05  FULL-SCHEDULE                FOREGROUND-COLOR 4.
               10  LINE 23 COL 1 BLANK LINE.
               10          COL 10 VALUE "THE SCHEDULE IS FULL.".
               10          COL 32 VALUE "YOU CANNOT ENTER ANY MORE".
               10          COL 57 VALUE " MOVIES.".
               10  LINE 24 COL 1 BLANK LINE.
               10          COL 45 VALUE "PRESS ENTER TO CONTINUE".
               10          COL 70 PIC X TO WV-ENTER.

       procedure division.
       100-SCH-ADD-MAIN.
           PERFORM 910-OPEN-FILES THRU 910-EXIT
           
           COPY "CPYBOOKS/ENABLE-KEYS.CPY".
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME
           MOVE SPACES TO WV-CONTINUE
           PERFORM 825-READ-SCH THRU 825-EXIT
           PERFORM 875-READ-TIC THRU 875-EXIT

           PERFORM 150-ADD-REC THRU 150-EXIT
           UNTIL (WV-CONTINUE = "N")

           PERFORM 800-WRITE-SCH-FILE THRU 800-EXIT
           PERFORM 850-WRITE-TIC THRU 850-EXIT
           
           PERFORM 900-CLOSE-FILES THRU 900-EXIT

           goback.
       100-EXIT.
           exit.

       150-ADD-REC.

           DISPLAY MOVIE-SCHEDULING-SCREEN-ADD
           ACCEPT MOVIE-SCHEDULING-SCREEN-ADD

           if(F3 or F4)
               MOVE "N" TO WV-CONTINUE
           ELSE

           if(F1)
               DISPLAY ADD-HELP
               ACCEPT ADD-HELP
           ELSE
               PERFORM 425-DOES-IT-EXIST THRU 425-EXIT

               if(WV-CONFIRM EQUALS "Y")
                   PERFORM 200-WRITE-RECORD THRU 200-EXIT

                   DISPLAY ADD-SUCCESS
                   ACCEPT ADD-SUCCESS
               END-IF

               if((WV-CONFIRM EQUALS "N" OR
                  WV-CONFIRM EQUALS SPACES)
                  AND (NOT F3 OR NOT F4))
                   DISPLAY ADD-FAILED
                   ACCEPT ADD-FAILED
               end-if

               if(NOT F3 OR F4)
                   DISPLAY AGAIN
                   ACCEPT AGAIN
               END-IF

               if(WV-CONTINUE = "Y")
                   PERFORM 900-CLOSE-FILES THRU 900-EXIT
                   PERFORM 910-OPEN-FILES THRU 910-EXIT
               end-if
           END-IF
           PERFORM 705-CLEAR-OUTPUT THRU 705-EXIT
           END-IF.
       150-EXIT.
           exit.

      * Writes the record to the scheduling file and then prompts the
      * user to see if they wish to enter another record
       200-WRITE-RECORD.
           PERFORM 300-CALCULATE-TIME THRU 300-EXIT

           PERFORM 700-MOVE-VARS THRU 700-EXIT
           VARYING SCH-I FROM 1 BY 1
           UNTIL (SCH-I > 6)

           PERFORM 710-MOVE-TICK THRU 710-EXIT
           VARYING TIC-I FROM 1 BY 1
           UNTIL (TIC-I > 6).
       200-EXIT.
           EXIT.
           
      * Calculates and stores the times for all of the showing of a 
      * movie for one day
       300-CALCULATE-TIME.
           MOVE WV-START-TIME TO WV-TEMP-TIME
           MOVE WV-TEMP-TIME TO WC-TIME1

           ADD 300 TO WC-TIME1 GIVING WC-TIME2
           ADD 300 TO WC-TIME2 GIVING WC-TIME3
           ADD 300 TO WC-TIME3 GIVING WC-TIME4
           ADD 300 TO WC-TIME4 GIVING WC-TIME5

           MOVE ZEROS TO WV-TEMP-TIME.
       300-EXIT.
           exit.

       425-DOES-IT-EXIST.
           PERFORM 525-GET-VENDOR-INFO THRU 525-EXIT
           if(WS-VENDOR-EXISTS = "Y")
               PERFORM 450-READ-MOVIE THRU 450-EXIT
               UNTIL(MOVIE-EOF = "Y" OR MOVIE-EOF = "M")
             
               if(MOVIE-EOF = "Y")
                   DISPLAY DOES-NOT-EXIST
                   ACCEPT DOES-NOT-EXIST

               end-if
               if(MOVIE-EOF = "M")
                   PERFORM 475-CHECK-MOVIE-ID THRU 475-EXIT
               end-if
           end-if.
       425-EXIT.
           exit.


      * Reads the schedule file to see if the movie is alread scheduled
      * Returns X if the movie is already in the file
      * Returns S if the screen is already being used
      * Returns Y at the end of the file
       450-READ-MOVIE.
           READ MRS-MOVIE-FILE INTO WORKING-MOVIE
           AT END
               MOVE "Y" TO MOVIE-EOF
           NOT AT END 
               if(WM-MOVIE-KEY = WV-ID)
                   MOVE "M" TO MOVIE-EOF
               end-if
           END-READ.
       450-EXIT.
           EXIT.

      * Checks to see if the movie Id entered exists with a movie that
      * is already purchased. Then checks a variety of other variables 
      * to make sure that the movie hasnt been previously entered into
      * the schedule already and to make sure that it is a valid movie
      * to enter into the schedule
       475-CHECK-MOVIE-ID.
           PERFORM 750-SCREEN-INFO THRU 750-EXIT
           PERFORM 550-READ-RENTAL THRU 550-EXIT
             UNTIL (EOF-RENTAL = "Y" OR EOF-RENTAL = "D")
      * Checks to see if the movie is ready to be added
               if(WV-READY-TO-SCH EQUALS "Y")
      * Searches sch file to see if the record is already scheduled
                   PERFORM 500-READ-SCH-TABLE THRU 500-EXIT
                   VARYING SCH-I FROM 1 BY 1
                   UNTIL (SCH-I > 6)
      * The "X" means that the movie is already scheduled
                   if(SCH-EXISTS = "X")
                      DISPLAY ALREADY-EXISTS
                      ACCEPT ALREADY-EXISTS
                      MOVE "N" TO SCH-EOF
                   else
      * Allows the user to enter the screen number and time of first
      * showing
                       if(WV-SCH-COUNTER NOT EQUAL 6)
                       perform 490-EVALUATE thru 490-exit
                       UNTIL (WV-CONFIRM = "Y" OR
                              WV-CONFIRM = "N")
      * Checks to see if the time is valid
                       if(WV-START-TIME >= 1000 AND
                          WV-START-TIME <= 1059)
      * Checks to make sure that the screen entered is within range
                           if(WV-SCREEN-NUM <= 6 AND WV-SCREEN-NUM >= 1)
                            
      * Looks to see if the date is valid
                              if(EOF-RENTAL = "D")
      * Checks to see if a movie is already scheduled for the screen
      * that was entered by the user.
                                PERFORM 500-READ-SCH-TABLE THRU 500-EXIT
                                   VARYING SCH-I FROM 1 BY 1
                                   UNTIL (SCH-I > 6)
                                  if(SCH-SAME-SCR = "S")
                                    MOVE "N" TO WV-CONFIRM
                                    DISPLAY SAME-SCREEN
                                    ACCEPT SAME-SCREEN
                                end-if
                            else
                                MOVE "N" TO WV-CONFIRM
                                display EXPIRED
                                ACCEPT EXPIRED
                            end-if
                       ELSE
                           MOVE "N" TO WV-CONFIRM
                           DISPLAY INVALID-SCREEN
                           ACCEPT INVALID-SCREEN
                       end-if
                       ELSE
                           MOVE "N" TO WV-CONFIRM
                           DISPLAY INVALID-TIME
                           ACCEPT INVALID-TIME
                       END-IF
                       ELSE
                           MOVE "N" TO WV-CONFIRM
                           DISPLAY FULL-SCHEDULE
                           ACCEPT FULL-SCHEDULE
                       END-IF
                   end-if
               ELSE
                   MOVE "N" TO WV-CONFIRM
                   DISPLAY NOT-READY
                   ACCEPT NOT-READY
               end-if.
       475-EXIT.
           exit.

       490-EVALUATE.
           DISPLAY GET-THE-DATA
           ACCEPT GET-THE-DATA

      *****************************************************************
      * Checks to see if any funtion keys were hit
      *****************************************************************

      * This will display the help message and redisplay the screen
           if(F1)
               DISPLAY ADD-DATA-HELP
               ACCEPT ADD-DATA-HELP

               DISPLAY GET-THE-DATA
               ACCEPT GET-THE-DATA
           end-if
      *This will goback to the screen where the user enters the movie id
          if(F4 OR F3)
              MOVE "N" TO WV-CONFIRM
              MOVE "Y" TO WV-CONTINUE
          END-IF
      * This will redisplay the screen with blank values for the user
           if(F12)
                MOVE ZEROS TO WV-SCREEN-NUM
                MOVE ZEROS TO WV-START-TIME
                DISPLAY GET-THE-DATA
                ACCEPT GET-THE-DATA
           end-if.
       490-EXIT.
           exit.

      * Reads the schedule table to see if the movie has already been
      * added to the schedule
      * Returns X if the movie is already has already been added
      * Returns S if the screen is already being used
       500-READ-SCH-TABLE.
           if(WT-SCH-MOVIE-ID(SCH-I) NOT EQUAL SPACES)
               IF (WT-SCH-MOVIE-ID(SCH-I) EQUALS WV-MOVIE-ID)
                   MOVE "X" TO SCH-EXISTS
               end-if

                if(WV-SCREEN-NUM EQUALS WT-SCH-SCREEN-NUM(SCH-I))
                    MOVE "S" TO SCH-SAME-SCR
                END-IF

                ADD 1 TO WV-SCH-COUNTER
            end-if.
       500-EXIT.
           exit.

      * Reads through the vendor file to find the vendor that
      * corresponds with the one for the movie
       525-GET-VENDOR-INFO.
           READ MRS-VENDOR-FILE
           AT end
               MOVE "Y" TO VENDOR-EOF
           NOT AT end
               if(WV-VENDOR-ID = MRS-VENDOR-ID)
                   MOVE MRS-VENDOR-COMPANY TO WO-MOVIE-VENDOR
                   MOVE "Y" TO WS-VENDOR-EXISTS
               end-iF
           end-read.
       525-EXIT.
           exit.

      * Reads the rental file and searchs for a valid date for the movie
      * Returns "D" if the date is valid
      * Retruns "Y" at the end of the file
       550-READ-RENTAL.
           READ MRS-RENTAL-FILE
           AT end
               MOVE "Y" TO EOF-RENTAL
           NOT AT END
               IF (MRS-END-DATE < WS-DATETIME)
                   MOVE "D" TO EOF-RENTAL
               END-IF
               if(MRS-READY-TO-SCHEDULE-FLAG = "Y")
                   MOVE "Y" TO WV-READY-TO-SCH
               end-if
           end-read.
       550-EXIT.
           EXIT.

      * Moves information to output variables
       700-MOVE-VARS.
           if(WT-SCH-MOVIE-ID(SCH-I) EQUALS SPACES AND
              WV-SCH-TAB-WRITE = "N")
               MOVE MRS-MOVIE-NAME TO WT-SCH-MOVIE-TITLE(SCH-I)
               MOVE WO-MOVIE-VENDOR TO WT-SCH-MOVIE-VENDOR(SCH-I)
               MOVE MRS-RATING TO WT-SCH-RATING(SCH-I)
               MOVE MRS-DESCRIPTION TO WT-SCH-DESCRIPTION(SCH-I)
               MOVE WV-SCREEN-NUM TO WT-SCH-SCREEN-NUM(SCH-I)
               MOVE WV-MOVIE-ID TO WT-SCH-MOVIE-ID(SCH-I)
               MOVE WC-SHOW-TIMES TO WT-SCH-SHOW-TIMES(SCH-I)
               MOVE "Y" TO WV-SCH-TAB-WRITE
           end-if.
       700-EXIT.
           EXIT.

      * Clears variables for next run through
       705-CLEAR-OUTPUT.
      * Clears variables that are shown on screen
           MOVE ZEROS TO WV-MOVIE-ID
           MOVE ZEROS TO WV-SCREEN-NUM
           MOVE ZEROS TO WV-START-TIME

           MOVE "N" TO WV-TIC-TAB-WRITE
           MOVE "N" TO WV-SCH-TAB-WRITE
           
           MOVE ZEROS TO WV-SCH-COUNT
           MOVE SPACES TO SCH-SAME-SCR
           MOVE SPACES TO SCH-EXISTS

      * Resets variables    
           MOVE "N" TO MOVIE-EOF
           MOVE "N" TO VENDOR-EOF
           MOVE "N" TO SCH-EOF
           MOVE "N" TO EOF-RENTAL
           MOVE "N" TO WS-VENDOR-EXISTS
           MOVE SPACES TO WV-CONFIRM.
       705-EXIT.
           exit.

       710-MOVE-TICK.
           if(WT-TIC-SCREEN-NUMBER(TIC-I) EQUALS ZEROS and
              WV-TIC-TAB-WRITE EQUALS "N")
               MOVE MRS-MOVIE-NAME TO WT-TIC-MOVIE-NAME(TIC-I)
               MOVE WV-SCREEN-NUM TO WT-TIC-SCREEN-NUMBER(TIC-I)
               MOVE MRS-RATING TO WT-TIC-RATING(TIC-I)
               MOVE WC-SHOW-TIMES TO WT-TIC-SHOW-TIME(TIC-I)
               MOVE WO-SEAT-NUM TO WT-TIC-SEATS-AVALIBLE(TIC-I)
               MOVE "Y" TO WV-TIC-TAB-WRITE
           END-IF.
       710-EXIT.
           EXIT.

       750-SCREEN-INFO.
           MOVE MRS-RATING TO WV-RATING
           MOVE MRS-DESCRIPTION TO WV-DESCRIPTION.
       750-EXIT.
           EXIT.

      * Writes the SCH-FILE from the table
       800-WRITE-SCH-FILE.
           PERFORM VARYING SCH-I FROM WV-SCH-HOLDER BY 1
           UNTIL (SCH-I > 6)
               if(WT-SCH-MOVIE-ID(SCH-I) NOT EQUAL SPACES)
                   WRITE MRS-SCH-INFO-REC FROM WT-SCH-TABLE(SCH-I)
                   end-write
               END-IF
           END-PERFORM.
       800-EXIT.
           exit.

      * Reads the SCHEDULE FILE into the table
       825-READ-SCH.
           PERFORM VARYING SCH-I FROM 1 BY 1
           UNTIL (SCH-I > 6 OR SCH-EOF EQUALS "Y")
               READ MRS-SCH-INFO-FILE NEXT RECORD INTO WORKING-INPUT
               AT END
                   MOVE "Y" TO SCH-EOF
               NOT AT END
                   MOVE MRS-SCH-INFO-REC TO WORKING-INPUT
                   if(WI-MOVIE-ID NOT EQUAL spaces)
                       MOVE WORKING-INPUT TO WT-SCH-TABLE(SCH-I)
                       ADD 1 TO WV-SCH-HOLDER
                   end-if 
               END-READ
           END-PERFORM.
       825-EXIT.
           exit.
           
      * Writes the TICKET-FILE from the ticket table
       850-WRITE-TIC.
           PERFORM VARYING TIC-I FROM WV-TIC-COUNTER BY 1
           UNTIL (TIC-I > 6)
               WRITE MRS-TICK-REC FROM WT-TICKET-TABLE(TIC-I)
           END-PERFORM.
       850-EXIT.
           exit.


      * Reads the TICKETING FILE into the table
       875-READ-TIC.
           PERFORM VARYING TIC-I FROM 1 BY 1
           UNTIL (TIC-I > 6 OR TIC-EOF EQUALS "Y")
               READ MRS-TICKET-INFO NEXT RECORD
               INTO WORKING-TICKET-INPUT 
               AT END
                   MOVE "Y" TO TIC-EOF
               NOT AT END
                   MOVE MRS-TICK-REC TO WORKING-TICKET-INPUT
                   if(WTI-SCREEN-NUMBER NOT EQUAL zeros)
                       MOVE WORKING-TICKET-INPUT TO 
                                               WT-TICKET-TABLE(TIC-I)
                       ADD 1 TO WV-TIC-COUNTER
                   end-if
               END-READ
           END-PERFORM.
       875-EXIT.
           exit.
           

       900-CLOSE-FILES.
           CLOSE MRS-MOVIE-FILE
                 MRS-RENTAL-FILE
                 MRS-SCH-INFO-FILE
                 MRS-TICKET-INFO
                 MRS-VENDOR-FILE.
       900-EXIT.
           EXIT.

       910-OPEN-FILES.
           OPEN INPUT   MRS-MOVIE-FILE
                        MRS-VENDOR-FILE
                        MRS-RENTAL-FILE.
           OPEN I-O     MRS-SCH-INFO-FILE
                        MRS-TICKET-INFO.
       910-EXIT.
           EIXT.

       end program MRS-3100.