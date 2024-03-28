      ******************************************************************
      * Author: Mostapha A
      * Purpose: Convert an sequential file to an indexed file
      ******************************************************************
	   IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB09.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *        EXTERNAL FILE WITH STUDENT RECORDS
               SELECT INVENT-FILE
                   ASSIGN TO "../INVENT.TXT"
                       ORGANIZATION IS LINE SEQUENTIAL.

      *        EXTERNAL FILE TO WRITE STUDENT REPORTS TO
               SELECT INDEXED-INVENT-FILE
                   ASSIGN TO "../INDEXED-INVENT.TXT"
                       ORGANIZATION IS INDEXED
                           ACCESS MODE IS RANDOM
                               RECORD KEY IS INVENTORY-ID-OUT
                                  ALTERNATE KEY IS VENDOR-NAME-OUT
                                      WITH DUPLICATES
                                          FILE STATUS IS STATUS-FILED.

       DATA DIVISION.
       FILE SECTION.
       FD INVENT-FILE.
       01 INVENTORY-RECORD-IN.
           05 INVENTORY-ID PIC X(9).
           05 VENDOR-NAME PIC X(20).
           05 INVENTORY-DESCRIPTION PIC X(40).

       FD INDEXED-INVENT-FILE.
       01 INVENTORY-RECORD-OUT.
           05 INVENTORY-ID-OUT PIC X(9).
           05 VENDOR-NAME-OUT PIC X(20).
           05 INVENTORY-DESCRIPTION-OUT PIC X(40).

       WORKING-STORAGE SECTION.
       01 CONTROL-FIELDS.
      *    END OF FILE FLAG
           05 EOF-FLAG PIC A.
      *    STORES STATUS OF WRITING TO INDEXED FILE
           01 STATUS-FILED PIC X(2).

       PROCEDURE DIVISION.
       100-CREATE-INDEXED-FILE.
           PERFORM 201-INITIALIZE
      *     LOOP UNTIL THE END OF FILE
           PERFORM 202-MOVE-RECORDS UNTIL EOF-FLAG EQUALS "Y".
           PERFORM 203-TERMINATE.
           STOP RUN.

      * OPEN THE FILES WE ARE READING AND WRITING
       201-INITIALIZE.
           OPEN INPUT INVENT-FILE.
           OPEN OUTPUT INDEXED-INVENT-FILE.
           PERFORM 300-READ-RECORDS.

      * MOVES THE RECORDS
       202-MOVE-RECORDS.
           PERFORM 301-WRITE-RECORDS.
           PERFORM 300-READ-RECORDS.

      * CLOSES THE FILES
       203-TERMINATE.
           CLOSE INVENT-FILE
                 INDEXED-INVENT-FILE.

      * READS A RECORD
       300-READ-RECORDS.
           READ INVENT-FILE AT END MOVE "Y" TO EOF-FLAG.

      * WRITES A RECORD
       301-WRITE-RECORDS.
           MOVE INVENTORY-RECORD-IN TO INVENTORY-RECORD-OUT.
           WRITE INVENTORY-RECORD-OUT
           INVALID KEY
               DISPLAY "INVALID KEY FOR " INVENTORY-ID-OUT
               " STATUS FILED IS " STATUS-FILED
           NOT INVALID KEY
               DISPLAY "INDEXED VENDOR " INVENTORY-ID-OUT
           END-WRITE.

       END PROGRAM LAB09.
