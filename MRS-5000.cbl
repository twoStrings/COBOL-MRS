       program-id. MRS_5000.
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
           SELECT MRS-GL-POST-FILE
               ASSIGN TO UT-SYS-POST
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MRS-RENTAL-FILE
               ASSIGN TO UT-SYS-MRS-RENT
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS MRS-RENT-ID.
           SELECT CGL-POSTING-CHECK-FILE
               ASSIGN TO UT-SYS-POST-CHECK
               ORGANIZATION IS LINE SEQUENTIAL.
       data division.
       FILE SECTION. 
           FD MRS-GL-POST-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 65 CHARACTERS.
       01 MRS-GL-POST-REC.
           05  MRS-GL-POST-ID.
               10 MRS-PREFIX                   PIC X(4).
               10 MRS-IDENTIFIER               PIC X(3).
           05  MRS-GL-ACC-NUM                  PIC 999.
           05  MRS-GL-DATE-OF-TRANS.
               10 MRS-GL-MONTH-OF-TRANS        PIC 99.
               10 MRS-GL-DAY-OF-TRANS          PIC 99.
               10 MRS-GL-YEAR-OF-TRANS         PIC 9999.
           05  MRS-GL-ADJUST-AMOUNT            PIC S9(5)V99.
           05  MRS-GL-JOURNAL-NUM.
               10  MRS-GL-JOR-DATE             PIC 9(8).
               10  MRS-GL-JOR-NUM              PIC 99.
           05  MRS-GL-DESC                     PIC X(30).
       COPY "CGL-FD-POST-CHECK.CPY".
       FD MRS-RENTAL-FILE
           RECORD CONTAINS 46 CHARACTERS.
       01 MRS-RENTAL-REC.
           05 MRS-RENT-ID                  PIC X(6).
           05 MRS-MOVIE-ID                 PIC 9(4).
           05 MRS-COPY-ID                  PIC 99.
           05 MRS-START-DATE.
               10 START-MONTH              PIC 9(2).
               10 START-DAY                PIC 9(2).
               10 START-YEAR               PIC 9(4). 
           05 MRS-END-DATE.
               10 END-MONTH              PIC 9(2).
               10 END-DAY                PIC 9(2).
               10 END-YEAR               PIC 9(4).
           05 MRS-SUBTOTAL                 PIC 9(4)V99.
           05 MRS-JOURNAL-NUMBER           PIC X(10).
           05 MRS-READY-TO-SCHEDULE-FLAG   PIC X.
           05 MRS-RETURN-FLAG              PIC X.
       working-storage section.
       01 WS-FILENAMES.
         05 UT-SYS-POST      PIC X(50)
           VALUE "C:\COBOL\MRS-GL-POST-FILE.DAT".
         05 UT-SYS-MRS-RENT  PIC X(50)
           VALUE "C:\COBOL\MRS-RENTAL.DAT". 
         05 UT-SYS-POST-CHECK PIC X(50)
           VALUE "C:\COBOL\POSTINGCHECK.DAT".
       COPY "DATETIME.CPY".
              
       01 WS-POSTING. 
           05 WS-POSTNUM          PIC 9(3).
           05 ONE                 PIC 9 VALUE 1.
           05 WS-ACCNUM           PIC 999 VALUE 100. 
           05 WS-PREFIX           PIC X(4) VALUE 'MRS-'.
           05 WS-DESC             PIC X(30) VALUE 
           'THIS IS A HELPFUL DESCRIPTION.'.
           05 WS-RENTAL-EOF       PIC X. 
               88 EOF             VALUE 'Y'.
           05 WS-DAYDIF           PIC 9(8) VALUE ZERO.
           05 WS-EARLIER          PIC 9(8) VALUE ZERO. 
           05 WS-LATER            PIC 9(8).
           05 WS-CURRENT          PIC 9(8).
           05 WS-CURRENT-DAY      PIC 9(8).
       procedure division.
       100-MAIN. 
           PERFORM 900-INIT THRU 900-END.
           PERFORM 400-SCHEDULE-CHECK
           PERFORM 200-POPULATE-POST THRU 200-END
               UNTIL EOF. 
           PERFORM 950-CLOSE-PAY THRU 950-END. 
           GOBACK. 
       100-END.
           STOP RUN. 
      
      *****************************************************************
      * Right now I am just taking the Journal number from the file 
      * GL has made and writing to that Journal number to our
      * post file. Should it be the rental file for our referance?

       200-POPULATE-POST.
      *Need a flag that will let me know to post or not for archiving?
      *(RETURN-FLAG?) !JOURNAL-NUMBER test below!
           IF PSTCK-MRSFLAG = 'N'
               MOVE 'Y' TO PSTCK-MRSFLAG
               MOVE PSTCK-JNUM TO MRS-GL-JOURNAL-NUM
           END-IF
           COMPUTE WS-POSTNUM = ONE + WS-POSTNUM 
           MOVE WS-POSTNUM TO MRS-IDENTIFIER
           MOVE WS-PREFIX TO MRS-PREFIX
           MOVE WS-ACCNUM TO MRS-GL-ACC-NUM
           
           MOVE WS-MONTH TO MRS-GL-MONTH-OF-TRANS
           MOVE WS-DAY TO MRS-GL-DAY-OF-TRANS 
           MOVE WS-YEAR TO MRS-GL-YEAR-OF-TRANS.
      *    Assuming that rental will subtotal with my function
      *    else I will just perform it here 
           MOVE MRS-SUBTOTAL TO MRS-GL-ADJUST-AMOUNT.
           MOVE WS-DESC TO MRS-GL-DESC
           MOVE MRS-GL-JOURNAL-NUM TO MRS-GL-JOURNAL-NUM.
           WRITE MRS-GL-POST-REC
           READ MRS-RENTAL-FILE
               AT END
                   MOVE 'Y' TO WS-RENTAL-EOF.

       200-END. 
           EXIT. 

      *****************************************************************
      * If we are doing this in Rentals do I still need this?

       300-COMPUTE-SUB. 
      *    THESE COMPUTES GET THE DIFFERNCES OF 
      *    TWO DATES COMPARED TO 12 31 1600
           COMPUTE WS-DAYDIF =
             (START-YEAR * 10000) + (START-MONTH * 100) + START-DAY
           COMPUTE WS-EARLIER = FUNCTION INTEGER-OF-DATE (WS-DAYDIF)
           COMPUTE WS-DAYDIF =
             (END-YEAR * 10000) + (END-MONTH * 100) + END-DAY
           COMPUTE WS-LATER = FUNCTION INTEGER-OF-DATE (WS-DAYDIF)
           COMPUTE WS-DAYDIF = WS-LATER - WS-EARLIER
           COMPUTE MRS-SUBTOTAL = WS-DAYDIF * MRS-SUBTOTAL.   
       300-END. 
           EXIT. 

      *****************************************************************
      * Flips the schedule flag if the end date has passed 

       400-SCHEDULE-CHECK. 
           COMPUTE WS-DAYDIF =
             (END-YEAR * 10000) + (END-MONTH * 100) + END-DAY
           COMPUTE WS-LATER = FUNCTION INTEGER-OF-DATE (WS-DAYDIF)
           COMPUTE WS-CURRENT = 
             (WS-YEAR * 10000) + (WS-MONTH * 100) + WS-DAY
           COMPUTE WS-CURRENT-DAY = FUNCTION INTEGER-OF-DATE(WS-CURRENT)
           IF WS-CURRENT-DAY > WS-LATER
               MOVE 'N' TO MRS-READY-TO-SCHEDULE-FLAG
           END-IF.
       400-END. 
           EXIT. 
       900-INIT.
           OPEN INPUT MRS-RENTAL-FILE CGL-POSTING-CHECK-FILE
           OPEN OUTPUT MRS-GL-POST-FILE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME.
           READ CGL-POSTING-CHECK-FILE
           READ MRS-RENTAL-FILE
               AT END MOVE 'Y' TO WS-RENTAL-EOF.
       900-END.
           EXIT.
       
       950-CLOSE-PAY.
           CLOSE MRS-RENTAL-FILE MRS-GL-POST-FILE. 
       950-END. 
           EXIT.
       end program MRS_5000.