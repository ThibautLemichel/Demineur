       IDENTIFICATION DIVISION. 
       PROGRAM-ID.  DEMINEUR.
       AUTHOR. Thibaut.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 GAME-SIZE             PIC 9(2) VALUE 15.
       01 MINE-NUMBER           PIC 9(3) VALUE 20.
       
       01 GAME-TABLE.
          05 X-COLUMNS OCCURS 100 TIMES.
             10 Y-COLUMNS OCCURS 100 TIMES.
                15 CASE         PIC X    VALUE "-".
       
       01 GAME-NUMBER-TABLE.
          05 X-NUMBER OCCURS 100 TIMES.
             10 Y-COLUMNS OCCURS 100 TIMES.
                15 CASE-NUMBER  PIC 9.
       
       01 GAME-VIS-TABLE.
          05 X-VIS OCCURS 100 TIMES.
             10 Y-VIS OCCURS 100 TIMES.
                15 CASE-VIS     PIC 9.

       01 X                     PIC 9(2).
       01 XX                    PIC 9(2).
       01 CURR-X                PIC 9(2).
       01 USER-X                PIC 9(2).
       01 Y                     PIC 9(2).
       01 YY                    PIC 9(2).
       01 CURR-Y                PIC 9(2).
       01 USER-Y                PIC 9(2).
       01 MAX-X                 PIC 9(2).
       01 MAX-Y                 PIC 9(2).
       01 MINE-COUNTER          PIC 9(3).
       01 UNVISITED-COUNT       PIC 9(3) VALUE 0.
       01 RAND                  PIC 9(2).
       01 GAME-VARIABLES.
          05 GAME-DONE          PIC 9    VALUE 0.

       PROCEDURE DIVISION.
           PERFORM GAME-SETTINGS.
           
           DISPLAY "Your Settings: "
           DISPLAY GAME-SIZE
           DISPLAY MINE-NUMBER

           PERFORM CREATE-GAMEBOARD.

           PERFORM CALCULATE-NUMBERS-SURROUNDING.

           PERFORM PRINT-GAMEBOARD-NUMBER
           PERFORM UNTIL GAME-DONE EQUAL 1
                   PERFORM PRINT-GAME
                   PERFORM GAME 
           END-PERFORM
           
           STOP RUN.

       GAME-SETTINGS.
           DISPLAY "Game Size : ".
           ACCEPT GAME-SIZE.
           DISPLAY "Number of mine : ".
           ACCEPT MINE-NUMBER.

       PRINT-GAMEBOARD-NUMBER.
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > GAME-SIZE 
                   PERFORM VARYING X FROM 1 BY 1 UNTIL X > GAME-SIZE 
                           IF CASE(X, Y) EQUAL "*"
                              DISPLAY "*" WITH NO ADVANCING
                           ELSE
                              DISPLAY CASE-NUMBER(X, Y) WITH NO
                                 ADVANCING
                           END-IF 
                   END-PERFORM
                   DISPLAY " "
           END-PERFORM.

       CREATE-GAMEBOARD.
           PERFORM UNTIL MINE-COUNTER > MINE-NUMBER 
                   COMPUTE RAND = FUNCTION RANDOM * GAME-SIZE 
                   MOVE RAND TO X 
                   COMPUTE RAND = FUNCTION RANDOM * GAME-SIZE 
                   MOVE RAND TO Y 

                   IF CASE(X, Y) EQUAL "-"
                      ADD 1 TO MINE-COUNTER
                      MOVE "*" TO CASE(X, Y)
                   END-IF
           END-PERFORM.

       CALCULATE-NUMBERS-SURROUNDING.
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > GAME-SIZE
                   PERFORM VARYING X FROM 1 BY 1 UNTIL X > GAME-SIZE
                           IF CASE(X, Y) EQUAL "-"
                              MOVE 0 TO MINE-COUNTER

                              COMPUTE XX = X - 1 
                              IF XX < 1
                                 MOVE 1 TO XX
                              END-IF

                              COMPUTE MAX-X = X + 1
                              IF MAX-X > GAME-SIZE
                                 MOVE GAME-SIZE TO MAX-X
                              END-IF

                              COMPUTE YY = Y - 1
                              IF YY < 1
                                 MOVE 1 TO YY
                              END-IF

                              COMPUTE MAX-Y = Y + 1
                              IF MAX-Y > GAME-SIZE
                                 MOVE GAME-SIZE TO MAX-Y
                              END-IF

                              PERFORM VARYING CURR-Y FROM YY BY 1 UNTIL
                                 CURR-Y > MAX-Y
                                      PERFORM VARYING CURR-X FROM XX BY
                                         1 UNTIL CURR-X > MAX-X
                                              IF NOT (CURR-X = X AND
                                                 CURR-Y = Y) 
                                                 IF CASE(CURR-X, CURR-Y)
                                                    = "*"
                                                    ADD 1 TO
                                                       MINE-COUNTER
                                                 END-IF
                                              END-IF
                                      END-PERFORM
                              END-PERFORM

                              MOVE MINE-COUNTER TO CASE-NUMBER(X, Y)
                           END-IF
                   END-PERFORM
           END-PERFORM.

       GAME.
           DISPLAY "Input X :".
           ACCEPT USER-X.
           DISPLAY "Input Y :".
           ACCEPT USER-Y.

           IF USER-X < 0 OR USER-Y < 0
              MOVE 1 TO GAME-DONE
           ELSE 
              MOVE 1 TO CASE-VIS(USER-X, USER-Y)
              IF CASE(USER-X, USER-Y) EQUAL "*"
                 DISPLAY "Game Over"
                 MOVE 1 TO GAME-DONE
              END-IF
              PERFORM CHECK-WIN
           END-IF.

       CHECK-WIN.
           MOVE 0 TO UNVISITED-COUNT
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > GAME-SIZE 
                   PERFORM VARYING X FROM 1 BY 1 UNTIL X > GAME-SIZE
                           IF CASE-VIS(X, Y) EQUAL 0
                              ADD 1 TO UNVISITED-COUNT
                           END-IF
                   END-PERFORM
           END-PERFORM

           IF UNVISITED-COUNT EQUAL MINE-NUMBER 
              DISPLAY "That's a Win"
              MOVE 1 TO GAME-DONE
           END-IF.

       PRINT-GAME.
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > GAME-SIZE 
                   PERFORM VARYING X FROM 1 BY 1 UNTIL X > GAME-SIZE 
                           IF CASE-VIS(X, Y) EQUAL 1
                              IF CASE(X, Y) EQUAL "-"
                                 IF CASE-NUMBER(X, Y) EQUAL 0
                                    DISPLAY "-" WITH NO ADVANCING
                                 ELSE
                                    DISPLAY CASE-NUMBER(X, Y) WITH NO
                                       ADVANCING 
                                 END-IF 
                              ELSE
                                 DISPLAY "*" WITH NO ADVANCING 
                              END-IF 
                           ELSE 
                              DISPLAY "+" WITH NO ADVANCING 
                           END-IF
                   END-PERFORM
                   DISPLAY " "
           END-PERFORM.