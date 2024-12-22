       IDENTIFICATION DIVISION. 
       PROGRAM-ID.  DEMINEUR.
       AUTHOR. Thibaut.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 GAME-SIZE        PIC 9(2) VALUE 15.
       01 MINE-NUMBER      PIC 9(3) VALUE 20.
       
       01 GAME-TABLE.
          05 X-COLUMNS OCCURS 100 TIMES.
             10 Y-COLUMNS OCCURS 100 TIMES.
                25 CASE    PIC X    VALUE "-".

       01 X                PIC 9(2).
       01 Y                PIC 9(2).
       01 MINE-COUNTER     PIC 9(3).
       01 RAND             PIC 9(2).

       PROCEDURE DIVISION.
           PERFORM GAME-SETTINGS.
           
           DISPLAY "Your Settings: "
           DISPLAY GAME-SIZE
           DISPLAY MINE-NUMBER

           PERFORM CREATE-GAMEBOARD.

           PERFORM PRINT-GAMEBOARD.
           
           STOP RUN.


       GAME-SETTINGS.
           DISPLAY "Game Size : ".
           ACCEPT GAME-SIZE.
           DISPLAY "Number of mine : ".
           ACCEPT MINE-NUMBER.

       PRINT-GAMEBOARD.
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > GAME-SIZE 
                   PERFORM VARYING X FROM 1 BY 1 UNTIL X > GAME-SIZE 
                           DISPLAY CASE(X, Y) WITH NO ADVANCING 
                   END-PERFORM
                   DISPLAY " "
           END-PERFORM.

       CREATE-GAMEBOARD.
           PERFORM UNTIL MINE-COUNTER >
              MINE-NUMBER 
                   COMPUTE RAND = FUNCTION RANDOM * GAME-SIZE 
                   MOVE RAND TO X 
                   COMPUTE RAND = FUNCTION RANDOM * GAME-SIZE 
                   MOVE RAND TO Y 

                   IF CASE(X, Y) EQUAL "-"
                      ADD 1 TO MINE-COUNTER
                      MOVE "*" TO CASE(X, Y)
                   END-IF

                
           END-PERFORM.