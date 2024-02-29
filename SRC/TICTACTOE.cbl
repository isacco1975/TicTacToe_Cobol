       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          TICTACTOE.
       AUTHOR.              ISAAC GARCIA PEVERI
       DATE-WRITTEN.        26.02.2024 - 27.02.2024
      * LAST EDIT           29.02.2024.
       REMARKS.             A MODERN TIC TAC TOE WRITTEN IN COBOL.
      /
      ******************************************************************
      * WRITTEN IN ACUCOBOL-GT 7.0.0
      *         A MODERN TIC TAC TOE - C 2024 ISAAC GARCIA PEVERI
      *                                IGP TECH BLOG (YOUTUBE)
      *
      *         THIS PROGRAM IS A MODERN TIC TAC TOE GAME WITH
      *         WITH INTERACTIVE GRAPHICAL USER INTERFACE
      ******************************************************************
      ******************************************************************
      *         BUG FIXES:
      *             FIX.1: TWO ABNORMAL SITUATIONS WHICH CPU PLACES
      *                    A WRONG MOVE, LETTING THE PLAYER WIN THE GAME
      *
      *             FIX.2: PLAYER PLACED ON ANGLE, CPU IN CENTRE AND
      *                    AGAIN PLAYER ON THE OPPOSITE ANGLE.
      *
      *
      *             FIX.3: BUG PRESSING AN OCCUPIED CELL WAS TRIGGERING
      *                    COMPUTER MOVE.
      *
      *             FIX.4: IF LAST MOVE WAS FROM CPU AND GAME ENDS DRAW,
      *                    MESSAGE WAS NOT APPEARING.
      ******************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.

       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.

       WORKING-STORAGE SECTION.
          COPY "ACUGUI.DEF".
          COPY "ACUCOBOL.DEF".
          COPY "CRTVARS.DEF".

       77 KEY-STATUS IS SPECIAL-NAMES CRT STATUS PIC 9(5) VALUE 0.
          88 ENTER-PUSHED VALUE 13.
          88 EXIT-PUSHED VALUE 27.
          88 MESSAGE-RECEIVED VALUE 95.
          88 EVENT-OCCURRED VALUE 96.
          88 SCREEN-NO-INPUT-FIELD VALUE 97.

      * PROPERTIES & USER DEFINED WORKING STOARGE
       77 FLAG-FIRST-TIME   PIC  X(1).
           88 FIRST-TIME           VALUE "F".
           88 OTHER-TIME           VALUE "O".

       77 FLAG-MOVE-DONE    PIC  X(1).
           88 MOVE-DONE            VALUE "Y".
           88 MOVE-NOT-DONE        VALUE "N".

       77 FLAG-CPU-STARTS   PIC  X(1).
           88 CPU-STARTS           VALUE "Y".
           88 PLAYER-STARTS        VALUE "N".

       77 FLAG-GRID-FULL    PIC  X(1).
           88 GRID-FULL            VALUE "Y".
           88 GRID-NOT-FULL        VALUE "N".

       77 WINNER            PIC  X(1).
           88 NONE-WINS            VALUE "N".
           88 PLAYER-WINS          VALUE "P".
           88 COMPUTER-WINS        VALUE "C".

       77 FORM1-HANDLE                 USAGE HANDLE OF WINDOW.
       77 BMP-X             PIC  S9(9) COMP-4 VALUE ZERO.
       77 BMP-O             PIC  S9(9) COMP-4 VALUE ZERO.
       77 BMP-N             PIC  S9(9) COMP-4 VALUE ZERO.
       77 IDX               PIC  S9(4) COMP   VALUE ZERO.
       77 IDX-2             PIC  S9(4) COMP   VALUE ZERO.
       77 COMPUTER-CHOSE    PIC  S9(1)        VALUE ZERO.
       77 EL-COUNT          PIC  S9(4) COMP   VALUE ZERO.

       78 WK-O                     VALUE "O".
       78 WK-X                     VALUE "X".

       01 GRID-BUFFER.
          05 ROW-EL         PIC X  OCCURS 9.
      *
       LINKAGE SECTION.
      *
      /
      * --------------------------------------------------------------
      *  THIS IS THE MAIN FORM WITH ALL THE ELEMENTS
      * --------------------------------------------------------------
       SCREEN  SECTION.
       01 FORM1.
          05 LABEL LINE 3 COL 4 COLOR 2
             TITLE "TIC TAC TOE BY ISAAC GARCIA PEVERI".

          05 LABEL LINE 3 COL 4
             TITLE "CLICCA SU UNA CASELLA A TUA SCELTA".

          05 LABEL LINE 4 COL 4 COLOR 7
             TITLE "TANTO PERDERAI INESORABILMENTE!!!".

          05 LBL-MSG LABEL LINE 19 COL 4 COLOR 3
             TITLE "                               ".

      *   RESET AND CANCEL BUTTONS
          05 PB-RS PUSH-BUTTON LINE 21 COL 9  LINES 1 SIZE 10
             TITLE "RESET"
             EXCEPTION-VALUE 5001
             ID 50001.

          05 PB-C  PUSH-BUTTON LINE 21 COL 23 LINES 1 SIZE 10
             TITLE "ESCI"
             EXCEPTION-VALUE 27
             ID 50002.

      *   FIRST ROW BUTTONS
          05 PB-11 PUSH-BUTTON LINE 7 COL 11 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 1001
             ID 10001.
          05 PB-12 PUSH-BUTTON LINE 7 COL 19 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 1002
             ID 10002.
          05 PB-13 PUSH-BUTTON LINE 7 COL 27 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 1003
             ID 10003.

      *   SECOND ROW BUTTONS
          05 PB-21 PUSH-BUTTON LINE 11 COL 11 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 2001
             ID 20001.
          05 PB-22 PUSH-BUTTON LINE 11 COL 19 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 2002
             ID 20002.
          05 PB-23 PUSH-BUTTON LINE 11 COL 27 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 2003
             ID 20003.

      *   THIRD ROW BUTTONS
          05 PB-31 PUSH-BUTTON LINE 15 COL 11 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 3001
             ID 30001.
          05 PB-32 PUSH-BUTTON LINE 15 COL 19 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 3002
             ID 30002.
          05 PB-33 PUSH-BUTTON LINE 15 COL 27 LINES 30 SIZE 30
             BITMAP-HANDLE BMP-N BITMAP-NUMBER 1 FRAMED SQUARE
             EXCEPTION-VALUE 3003
             ID 30003.
      /
      * --------------------------------------------------------------
      *   THE PROCEDURE DIVISION - MAIN LOGIC
      * --------------------------------------------------------------
       PROCEDURE  DIVISION.
       MAIN-LOGIC.
           PERFORM LOAD-IMAGES
           PERFORM INITIALIZE-GAME
           PERFORM DISPLAY-SCREEN

           MOVE FUNCTION CURRENT-DATE(15:1) TO COMPUTER-CHOSE
           IF COMPUTER-CHOSE NOT = 7
              SET CPU-STARTS TO TRUE
              PERFORM COMPUTER-MOVE
           END-IF

           PERFORM FORM1-WORKING-CYCLE
           .
      /
      * --------------------------------------------------------------
      *   THIS SHOULD BE DONE ONLY ONE TIME!
      * --------------------------------------------------------------
       LOAD-IMAGES.
           CALL "W$BITMAP" USING WBITMAP-LOAD "IMG\X.BMP"
                   GIVING BMP-X

           CALL "W$BITMAP" USING WBITMAP-LOAD "IMG\O.BMP"
                   GIVING BMP-O

           CALL "W$BITMAP" USING WBITMAP-LOAD "IMG\N.BMP"
                   GIVING BMP-N
           .
      /
      * --------------------------------------------------------------
      *   PERFORMING GAME INITIALIZE HERE AND BUFFER CLEANING
      *   THIS ROUTINE IS ALSO USED AS RESET BUTTON
      * --------------------------------------------------------------
       INITIALIZE-GAME.
           INITIALIZE GRID-BUFFER
           SET NONE-WINS     TO TRUE
           SET MOVE-NOT-DONE TO TRUE
           SET GRID-NOT-FULL TO TRUE
           SET PLAYER-STARTS TO TRUE
           SET FIRST-TIME    TO TRUE

           MODIFY PB-11 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-12 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-13 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-21 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-22 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-23 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-31 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-32 ENABLED 1 BITMAP-HANDLE BMP-N
           MODIFY PB-33 ENABLED 1 BITMAP-HANDLE BMP-N

           MODIFY LBL-MSG TITLE = SPACES
           .
      /
      * --------------------------------------------------------------
      *   CREATING THE MAIN INTERACTIVE FORM
      * --------------------------------------------------------------
       DISPLAY-SCREEN.
           DISPLAY STANDARD GRAPHICAL WINDOW
              LINES 23 SIZE 41 COLOR 65793,
              TITLE-BAR, TITLE "TIC TAC TOE", WITH SYSTEM MENU,
              HANDLE FORM1-HANDLE

           DISPLAY FORM1 UPON FORM1-HANDLE
           .
      /
      * --------------------------------------------------------------
      *   ACCEPT
      * --------------------------------------------------------------
       FORM1-WORKING-CYCLE.
           PERFORM UNTIL EXIT-PUSHED
              ACCEPT FORM1
                 ON EXCEPTION
                    PERFORM FORM1-EVAL-FUNC
              END-ACCEPT
           END-PERFORM

           DESTROY FORM1-HANDLE
           INITIALIZE KEY-STATUS
           .
      /
      * --------------------------------------------------------------
      *   CHECKING WHICH KEY WAS PRESSED BY THE PLAYER
      * --------------------------------------------------------------
       FORM1-EVAL-FUNC.
           SET MOVE-NOT-DONE       TO TRUE

           EVALUATE TRUE

              WHEN EXIT-PUSHED
                 PERFORM FORM1-EXIT

              WHEN EVENT-OCCURRED
                 IF EVENT-TYPE = CMD-CLOSE
                    PERFORM FORM1-EXIT
                 END-IF

      * >...  FIRST ROW BUTTON CLICKS
              WHEN KEY-STATUS = 1001
                   IF ROW-EL(1) = SPACES
                      MOVE WK-O TO ROW-EL(1)
                      MODIFY PB-11 BITMAP-HANDLE BMP-O
                      MOVE 1    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF
              WHEN KEY-STATUS = 1002
                   IF ROW-EL(2) = SPACES
                      MOVE WK-O TO ROW-EL(2)
                      MODIFY PB-12 BITMAP-HANDLE BMP-O
                      MOVE 2    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF
              WHEN KEY-STATUS = 1003
                   IF ROW-EL(3) = SPACES
                      MOVE WK-O TO ROW-EL(3)
                      MODIFY PB-13 BITMAP-HANDLE BMP-O
                      MOVE 3    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF

      * >...  SECOND ROW BUTTON CLICKS
              WHEN KEY-STATUS = 2001
                   IF ROW-EL(4) = SPACES
                      MOVE WK-O TO ROW-EL(4)
                      MODIFY PB-21 BITMAP-HANDLE BMP-O
                      MOVE 4    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF
              WHEN KEY-STATUS = 2002
                   IF ROW-EL(5) = SPACES
                      MOVE WK-O TO ROW-EL(5)
                      MODIFY PB-22 BITMAP-HANDLE BMP-O
                      MOVE 5    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF
              WHEN KEY-STATUS = 2003
                   IF ROW-EL(6) = SPACES
                      MOVE WK-O TO ROW-EL(6)
                      MODIFY PB-23 BITMAP-HANDLE BMP-O
                      MOVE 6    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF

      * >...  THIRD ROW BUTTON CLICKS
              WHEN KEY-STATUS = 3001
                   IF ROW-EL(7) = SPACES
                      MOVE WK-O TO ROW-EL(7)
                      MODIFY PB-31 BITMAP-HANDLE BMP-O
                      MOVE 7    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF
              WHEN KEY-STATUS = 3002
                   IF ROW-EL(8) = SPACES
                      MOVE WK-O TO ROW-EL(8)
                      MODIFY PB-32 BITMAP-HANDLE BMP-O
                      MOVE 8    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF
              WHEN KEY-STATUS = 3003
                   IF ROW-EL(9) = SPACES
                      MOVE WK-O TO ROW-EL(9)
                      MODIFY PB-33 BITMAP-HANDLE BMP-O
                      MOVE 9    TO IDX
FIX.3                 PERFORM COMPUTER-MOVE
                   END-IF

              WHEN KEY-STATUS = 5001
                   PERFORM INITIALIZE-GAME

           END-EVALUATE

           MOVE 4 TO ACCEPT-CONTROL
           .
      /
      * --------------------------------------------------------------
      *   CALCULATING NEXT MOVE AGAINST THE PLAYER
      * --------------------------------------------------------------
       COMPUTER-MOVE.
           PERFORM UNTIL MOVE-DONE OR GRID-FULL

              MOVE FUNCTION CURRENT-DATE(15:1) TO COMPUTER-CHOSE

              IF COMPUTER-CHOSE = ZERO
                 MOVE 1 TO COMPUTER-CHOSE
              END-IF

      * ..... TO MAKE THE GAME MORE DIFFICULT, I DECIDE THE COMPUTER
      * ..... TO CHOSE ANY ANGLE FOR START (WHEN HE STARTS THE GAME)
              IF CPU-STARTS
                 SET PLAYER-STARTS      TO TRUE
                 EVALUATE COMPUTER-CHOSE
                          WHEN 2 MOVE 3 TO COMPUTER-CHOSE
                          WHEN 4 MOVE 7 TO COMPUTER-CHOSE
                          WHEN 6 MOVE 9 TO COMPUTER-CHOSE
                          WHEN 5 MOVE 1 TO COMPUTER-CHOSE
                          WHEN 8 MOVE 7 TO COMPUTER-CHOSE
                 END-EVALUATE
              END-IF

              IF ROW-EL(COMPUTER-CHOSE) = SPACES
                IF NOT MOVE-DONE
                    SET MOVE-DONE    TO TRUE

                    PERFORM DETERMINE-MOVE

                    MOVE WK-X        TO ROW-EL(COMPUTER-CHOSE)
                    EVALUATE COMPUTER-CHOSE
                       WHEN 1 MODIFY PB-11 BITMAP-HANDLE BMP-X
                       WHEN 2 MODIFY PB-12 BITMAP-HANDLE BMP-X
                       WHEN 3 MODIFY PB-13 BITMAP-HANDLE BMP-X
                       WHEN 4 MODIFY PB-21 BITMAP-HANDLE BMP-X
                       WHEN 5 MODIFY PB-22 BITMAP-HANDLE BMP-X
                       WHEN 6 MODIFY PB-23 BITMAP-HANDLE BMP-X
                       WHEN 7 MODIFY PB-31 BITMAP-HANDLE BMP-X
                       WHEN 8 MODIFY PB-32 BITMAP-HANDLE BMP-X
                       WHEN 9 MODIFY PB-33 BITMAP-HANDLE BMP-X
                    END-EVALUATE
                 END-IF
              END-IF

FIX.4         PERFORM CHECK-GRID-FULL

      * ..... CHECKING IF PLAYER OR CPU WON
              PERFORM CHECK-WINNER

      * ..... FINAL STAGE: LAST MOMENTS AFTER LONG FIGHTING :)
              EVALUATE TRUE
                 WHEN COMPUTER-WINS
                      PERFORM DISABLE-ALL-BUTTONS
                      MODIFY LBL-MSG
                    TITLE "               <<< HO VINTO IO! >>>"  COLOR 5
                 WHEN PLAYER-WINS
                      PERFORM DISABLE-ALL-BUTTONS
                      MODIFY LBL-MSG
                    TITLE "               <<< HAI VINTO!!! >>>"  COLOR 4
              END-EVALUATE

              IF GRID-FULL AND NONE-WINS
                 PERFORM DISABLE-ALL-BUTTONS
                 MODIFY LBL-MSG
                    TITLE "               <<< SIAMO PARI!! >>>"  COLOR 6
              END-IF

           END-PERFORM
           .
      /
      * --------------------------------------------------------------
      *   CHECKING IF GRID IS FULL TO END GAME EVEN IF LAST MOVE
      *   WAS THE CPU
      * --------------------------------------------------------------
       CHECK-GRID-FULL.
      * . CHECKING IF GRID IS FULL
          MOVE ZERO TO EL-COUNT

          PERFORM VARYING IDX-2 FROM 1 BY 1 UNTIL IDX-2 > 9
             IF ROW-EL(IDX-2) NOT = SPACES
                ADD 1 TO EL-COUNT
             END-IF
          END-PERFORM

          IF EL-COUNT = 9
             SET GRID-FULL TO TRUE
          END-IF
          .
      /
      * --------------------------------------------------------------
      *   CHECKING ALL THE POSSIBLE COMBINATIONS, GIVING PRIORITY
      *   TO THE COMPUTER.
      * --------------------------------------------------------------
       DETERMINE-MOVE.
      * ---
      * ---
      * >>>>> IF PLAYER STARTS, BE SURE THAT CPU WILL NOT CHOSE ANGLES!
      * >>>>> TO GIVE THE PLAYER SOME POSSIBILITY TO WIN, SIMPLY
      * >>>>> COMMENT THIS IF STATEMENT.
      * >>>>> BY KEEPING THIS "IF", PLAYER WILL NEVER WIN A GAME!!!
      * ---
      * ---
           IF (IDX = 1 OR 3 OR 7 OR 9) AND FIRST-TIME
              MOVE 5 TO COMPUTER-CHOSE
              SET OTHER-TIME TO TRUE
              MOVE ZERO TO IDX
           END-IF

      * ---
           EVALUATE TRUE

      ***---
FIX.1 * >>>**** BUGFIX 20240229.1
FIX.1 *         THERE ARE 2 SITUATIONS, WHICH
FIX.1 *         THE COMPUTER IS NOT PLACING HIS MOVE CORRECTLY, ALLOWING
FIX.1 *         THE PLAYER TO WIN: THOSE SITUATIONS ARE DESCRIBED BELOW.
FIX.1
FIX.1 * > SITUATION 1: PLAYER PLACED ON CELLS 3 AND 4, BUT CPU PLACES
FIX.1 *                HIS MOVE ON CELL 2:
FIX.1
FIX.1 * ........... +-----------+
FIX.1 * ........... +   |   | O +  <-- I HAVE TO AVOID THAT THE CPU
FIX.1 * ........... +-----------+      PLACES HIS MOVE ON 2TH CELL.
FIX.1 * ........... + O | X |   +
FIX.1 * ........... +-----------+
FIX.1 * ........... +   |   |   +
FIX.1 * ........... +-----------+
FIX.1
FIX.1 *                THE RIGHT CPU MOVE SHOULD BE TO PLACE IN CELL 9
FIX.1
FIX.1 * ........... +-----------+
FIX.1 * ........... +   | * | O +  <-- CPU INCORRECT PREVISION
FIX.1 * ........... +-----------+
FIX.1 * ........... + O | X |   +
FIX.1 * ........... +-----------+
FIX.1 * ........... +   |   | X +  <-- CORRECT MOVE
FIX.1 * ........... +-----------+
FIX.1               WHEN ROW-EL(3) = WK-O
FIX.1               AND  ROW-EL(4) = WK-O
FIX.1               AND  ROW-EL(5) = WK-X
FIX.1               AND  ROW-EL(9) = SPACES
FIX.1                   MOVE WK-X           TO ROW-EL(9)
FIX.1                    MOVE 9             TO COMPUTER-CHOSE
FIX.1
FIX.1 * > SITUATION 2: IS THE MIRRORED SAME SITUATION:
FIX.1
FIX.1 * ........... +-----------+
FIX.1 * ........... + O |   |   +
FIX.1 * ........... +-----------+
FIX.1 * ........... +   | X | O +
FIX.1 * ........... +-----------+
FIX.1 * ........... +   | X |   +  <-- CPU PLACEMENT WRONG
FIX.1 * ........... +-----------+
FIX.1
FIX.1 * ........... +-----------+
FIX.1 * ........... + O |   |   +
FIX.1 * ........... +-----------+
FIX.1 * ........... +   | X | O +
FIX.1 * ........... +-----------+
FIX.1 * ........... + X | * |   +  <-- CORRECT POSITION IS 7TH CELL
FIX.1 * ........... +-----------+
FIX.1               WHEN ROW-EL(1) = WK-O
FIX.1                AND ROW-EL(6) = WK-O
FIX.1                AND ROW-EL(5) = WK-X
FIX.1                AND ROW-EL(7) = SPACES
FIX.1                   MOVE WK-X           TO ROW-EL(7)
FIX.1                   MOVE SPACES         TO ROW-EL(8)
FIX.1                    MOVE 7             TO COMPUTER-CHOSE
FIX.1
FIX.1 * <<<**** END FIX
      ***---
FIX.2 * > SITUATION 3: PLAYER PLACED ON ANGLE, CPU IN CENTRE AND AGAIN
FIX.2 *                PLAYER ON THE OPPOSITE ANGLE AS SHOWN
FIX.2 *                IN THIS CASE, CPU SHOUD CHOSE A SIDE CELL AND
FIX.2 *                NOT AN ANGLE

FIX.2 * ........... +-----------+        +-----------+
FIX.2 * ........... +   |   | O +        + O |   |   +
FIX.2 * ........... +-----------+        +-----------+
FIX.2 * ........... +   | X |   +   OR   +   | X |   +
FIX.2 * ........... +-----------+        +-----------+
FIX.2 * ........... + O |   |   +        +   |   | O +
FIX.2 * ........... +-----------+        +-----------+

FIX.2               WHEN ROW-EL(3) = WK-O
FIX.2                AND ROW-EL(5) = WK-X
FIX.2                AND ROW-EL(7) = WK-O
FIX.2                AND ROW-EL(8) = SPACES | CELL TO PLACE MOVE
FIX.2                AND ROW-EL(2) = SPACES | THIS MUST BE EMPTY!
FIX.2                   MOVE WK-X           TO ROW-EL(8)
FIX.2                    MOVE 8             TO COMPUTER-CHOSE

FIX.2               WHEN ROW-EL(1) = WK-O
FIX.2                AND ROW-EL(5) = WK-X
FIX.2                AND ROW-EL(9) = WK-O
FIX.2                AND ROW-EL(2) = SPACES | CELL TO PLACE MOVE
FIX.2                AND ROW-EL(4) = SPACES | THIS MUST BE EMPTY
FIX.2                   MOVE WK-X           TO ROW-EL(2)
FIX.2                    MOVE 2             TO COMPUTER-CHOSE
      ***---
FIX.2 * >>>**** BUGFIX 20240229.2

FIX.2 * <<<**** END FIX
      ***---

      * ---
      * ---
      * >>>>> MOVES THAT MAKE THE COMPUTER TO WIN THE GAME
      * ---
      * ---

      * ........... +-----------+
      * ........... + X | X |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(2) = WK-X
                    AND  ROW-EL(3) = SPACES
                        MOVE WK-X           TO ROW-EL(3)
                         MOVE 3             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X |   | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(3) = WK-X
                    AND  ROW-EL(2) = SPACES
                        MOVE WK-X           TO ROW-EL(2)
                         MOVE 2             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   | X | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-X
                    AND  ROW-EL(3) = WK-X
                    AND  ROW-EL(1) = SPACES
                        MOVE WK-X           TO ROW-EL(1)
                         MOVE 1             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X | X |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(6) = SPACES
                        MOVE WK-X           TO ROW-EL(6)
                         MOVE 6             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X |   | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-X
                    AND  ROW-EL(6) = WK-X
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   | X | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-X
                    AND  ROW-EL(6) = WK-X
                    AND  ROW-EL(4) = SPACES
                        MOVE WK-X           TO ROW-EL(4)
                         MOVE 4             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X | X |   +
      * ........... +-----------+
                    WHEN ROW-EL(7) = WK-X
                    AND  ROW-EL(8) = WK-X
                    AND  ROW-EL(9) = SPACES
                        MOVE WK-X           TO ROW-EL(9)
                         MOVE 9             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(7) = WK-X
                    AND  ROW-EL(9) = WK-X
                    AND  ROW-EL(8) = SPACES
                        MOVE WK-X           TO ROW-EL(8)
                         MOVE 8             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   | X | X +
      * ........... +-----------+
                    WHEN ROW-EL(8) = WK-X
                    AND  ROW-EL(9) = WK-X
                    AND  ROW-EL(7) = SPACES
                        MOVE WK-X           TO ROW-EL(7)
                         MOVE 7             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(4) = WK-X
                    AND  ROW-EL(7) = SPACES
                        MOVE WK-X           TO ROW-EL(7)
                         MOVE 7             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(7) = WK-X
                    AND  ROW-EL(4) = SPACES
                        MOVE WK-X           TO ROW-EL(4)
                         MOVE 4             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-X
                    AND  ROW-EL(7) = WK-X
                    AND  ROW-EL(1) = SPACES
                        MOVE WK-X           TO ROW-EL(1)
                         MOVE 1             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(8) = SPACES
                        MOVE WK-X           TO ROW-EL(8)
                         MOVE 8             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-X
                    AND  ROW-EL(8) = WK-X
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-X
                    AND  ROW-EL(8) = WK-X
                    AND  ROW-EL(2) = SPACES
                        MOVE WK-X           TO ROW-EL(2)
                         MOVE 2             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-X
                    AND  ROW-EL(6) = WK-X
                    AND  ROW-EL(9) = SPACES
                        MOVE WK-X           TO ROW-EL(9)
                         MOVE 9             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-X
                    AND  ROW-EL(9) = WK-X
                    AND  ROW-EL(6) = SPACES
                        MOVE WK-X           TO ROW-EL(6)
                         MOVE 6             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(6) = WK-X
                    AND  ROW-EL(9) = WK-X
                    AND  ROW-EL(3) = SPACES
                        MOVE WK-X           TO ROW-EL(3)
                         MOVE 3             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(9) = SPACES
                        MOVE WK-X           TO ROW-EL(9)
                         MOVE 9             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(9) = WK-X
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-X
                    AND  ROW-EL(9) = WK-X
                    AND  ROW-EL(1) = SPACES
                        MOVE WK-X           TO ROW-EL(1)
                         MOVE 1             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(7) = SPACES
                        MOVE WK-X           TO ROW-EL(7)
                         MOVE 7             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-X
                    AND  ROW-EL(7) = WK-X
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-X
                    AND  ROW-EL(7) = WK-X
                    AND  ROW-EL(3) = SPACES
                        MOVE WK-X           TO ROW-EL(3)
                         MOVE 3             TO COMPUTER-CHOSE

      * ---
      * ---
      * >>>>> MOVES THAT MAKE THE COMPUTER TO BLOCK POSSIBILITIES TO
      * >>>>> THE PLAYER TO WIN
      * ---
      * ---

      * ........... +-----------+
      * ........... + O | O | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(2) = WK-O
                    AND  ROW-EL(3) = SPACES
                        MOVE WK-X           TO ROW-EL(3)
                         MOVE 3             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + O | X | O +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(3) = WK-O
                    AND  ROW-EL(2) = SPACES
                        MOVE WK-X           TO ROW-EL(2)
                         MOVE 2             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X | O | O +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-O
                    AND  ROW-EL(3) = WK-O
                    AND  ROW-EL(1) = SPACES
                        MOVE WK-X           TO ROW-EL(1)
                         MOVE 1             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + O | O | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(6) = SPACES
                        MOVE WK-X           TO ROW-EL(6)
                         MOVE 6             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + O | X | O +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-O
                    AND  ROW-EL(6) = WK-O
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X | O | O +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-O
                    AND  ROW-EL(6) = WK-O
                    AND  ROW-EL(4) = SPACES
                        MOVE WK-X           TO ROW-EL(4)
                         MOVE 4             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + O | O | X +
      * ........... +-----------+
                    WHEN ROW-EL(7) = WK-O
                    AND  ROW-EL(8) = WK-O
                    AND  ROW-EL(9) = SPACES
                        MOVE WK-X           TO ROW-EL(9)
                         MOVE 9             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + O | X | O +
      * ........... +-----------+
                    WHEN ROW-EL(7) = WK-O
                    AND  ROW-EL(9) = WK-O
                    AND  ROW-EL(8) = SPACES
                        MOVE WK-X           TO ROW-EL(8)
                         MOVE 8             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X | O | O +
      * ........... +-----------+
                    WHEN ROW-EL(8) = WK-O
                    AND  ROW-EL(9) = WK-O
                    AND  ROW-EL(7) = SPACES
                        MOVE WK-X           TO ROW-EL(7)
                         MOVE 7             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(4) = WK-O
                    AND  ROW-EL(7) = SPACES
                        MOVE WK-X           TO ROW-EL(7)
                         MOVE 7             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(7) = WK-O
                    AND  ROW-EL(4) = SPACES
                        MOVE WK-X           TO ROW-EL(4)
                         MOVE 4             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-O
                    AND  ROW-EL(7) = WK-O
                    AND  ROW-EL(1) = SPACES
                        MOVE WK-X           TO ROW-EL(1)
                         MOVE 1             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(8) = SPACES
                        MOVE WK-X           TO ROW-EL(8)
                         MOVE 8             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-O
                    AND  ROW-EL(8) = WK-O
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-O
                    AND  ROW-EL(8) = WK-O
                    AND  ROW-EL(2) = SPACES
                        MOVE WK-X           TO ROW-EL(2)
                         MOVE 2             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-O
                    AND  ROW-EL(6) = WK-O
                    AND  ROW-EL(9) = SPACES
                        MOVE WK-X           TO ROW-EL(9)
                         MOVE 9             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-O
                    AND  ROW-EL(9) = WK-O
                    AND  ROW-EL(6) = SPACES
                        MOVE WK-X           TO ROW-EL(6)
                         MOVE 6             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
                    WHEN ROW-EL(6) = WK-O
                    AND  ROW-EL(9) = WK-O
                    AND  ROW-EL(3) = SPACES
                        MOVE WK-X           TO ROW-EL(3)
                         MOVE 3             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(9) = SPACES
                        MOVE WK-X           TO ROW-EL(9)
                         MOVE 9             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(9) = WK-O
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-O
                    AND  ROW-EL(9) = WK-O
                    AND  ROW-EL(1) = SPACES
                        MOVE WK-X           TO ROW-EL(1)
                         MOVE 1             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(7) = SPACES
                        MOVE WK-X           TO ROW-EL(7)
                         MOVE 7             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-O
                    AND  ROW-EL(7) = WK-O
                    AND  ROW-EL(5) = SPACES
                        MOVE WK-X           TO ROW-EL(5)
                         MOVE 5             TO COMPUTER-CHOSE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(5) = WK-O
                    AND  ROW-EL(7) = WK-O
                    AND  ROW-EL(3) = SPACES
                        MOVE WK-X           TO ROW-EL(3)
                         MOVE 3             TO COMPUTER-CHOSE

           END-EVALUATE
           .
      /
      * ---------------------------------------------------------------
      *   CHECKING WHO IS WINNING
      * ---------------------------------------------------------------
       CHECK-WINNER.
           SET NONE-WINS                      TO TRUE

           EVALUATE TRUE

      * ........... +-----------+
      * ........... + O | O | O +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(2) = WK-O
                    AND  ROW-EL(3) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(9) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + O | O | O +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(6) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(7) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + O | O | O +
      * ........... +-----------+
                    WHEN ROW-EL(7) = WK-O
                    AND  ROW-EL(8) = WK-O
                    AND  ROW-EL(9) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
      * ........... + O |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-O
                    AND  ROW-EL(4) = WK-O
                    AND  ROW-EL(7) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
      * ........... +   | O |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-O
                    AND  ROW-EL(5) = WK-O
                    AND  ROW-EL(8) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
      * ........... +   |   | O +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-O
                    AND  ROW-EL(6) = WK-O
                    AND  ROW-EL(9) = WK-O
                         SET PLAYER-WINS      TO TRUE

      * ........... +-----------+
      * ........... + X | X | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(2) = WK-X
                    AND  ROW-EL(3) = WK-X
                         SET COMPUTER-WINS    TO TRUE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(9) = WK-X
                         SET COMPUTER-WINS    TO TRUE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X | X | X +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(4) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(6) = WK-X
                         SET COMPUTER-WINS    TO TRUE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(7) = WK-X
                         SET COMPUTER-WINS    TO TRUE

      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... +   |   |   +
      * ........... +-----------+
      * ........... + X | X | X +
      * ........... +-----------+
                    WHEN ROW-EL(7) = WK-X
                    AND  ROW-EL(8) = WK-X
                    AND  ROW-EL(9) = WK-X
                         SET COMPUTER-WINS    TO TRUE

      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
      * ........... + X |   |   +
      * ........... +-----------+
                    WHEN ROW-EL(1) = WK-X
                    AND  ROW-EL(4) = WK-X
                    AND  ROW-EL(7) = WK-X
                         SET COMPUTER-WINS    TO TRUE

      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
      * ........... +   | X |   +
      * ........... +-----------+
                    WHEN ROW-EL(2) = WK-X
                    AND  ROW-EL(5) = WK-X
                    AND  ROW-EL(8) = WK-X
                         SET COMPUTER-WINS    TO TRUE

      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
      * ........... +   |   | X +
      * ........... +-----------+
                    WHEN ROW-EL(3) = WK-X
                    AND  ROW-EL(6) = WK-X
                    AND  ROW-EL(9) = WK-X
                         SET COMPUTER-WINS    TO TRUE
           END-EVALUATE
           .
      /
      * --------------------------------------------------------------
      *   DISABLE ALL THE BUTTONS WHEN GAME ENDS
      * --------------------------------------------------------------
       DISABLE-ALL-BUTTONS.
           MODIFY PB-11 ENABLED 0
           MODIFY PB-12 ENABLED 0
           MODIFY PB-13 ENABLED 0
           MODIFY PB-21 ENABLED 0
           MODIFY PB-22 ENABLED 0
           MODIFY PB-23 ENABLED 0
           MODIFY PB-31 ENABLED 0
           MODIFY PB-32 ENABLED 0
           MODIFY PB-33 ENABLED 0
           .
      /
      * --------------------------------------------------------------
      *   END PROGRAM
      * --------------------------------------------------------------
       FORM1-EXIT.
           MOVE 27 TO KEY-STATUS
           STOP RUN.
           .
