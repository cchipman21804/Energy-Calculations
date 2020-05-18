      *****************************************************************
      * FAN / PUMP AFFINITY LAW CALCULATIONS                          *
      *                                                               *
      * A simple program that calculates the change in horsepower,    *
      * pressure, and flow rate with changes in rotational speed.     *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   affinity.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Affinity Laws:                                                *
      *                                                               *
      * Law #1                                                        *
      * Quantity (CFM/GPM) changes proportionally with fan/pump speed *
      *                                                               *
      *       CFM2|GPM2   [ RPM2 ]^1                                  *
      *      ---------- = [------]                                    *
      *       CFM1|GPM1   [ RPM1 ]                                    *
      *                                                               *
      * Law #2                                                        *
      * Pressure varies with the SQUARE of fan/pump speed             *
      *                                                               *
      *       P2    [ RPM2 ]^2                                        *
      *      ---- = [------]                                          *
      *       P1    [ RPM1 ]                                          *
      *                                                               *
      * Law #3                                                        *
      * Horsepower varies with the CUBE of the fan/pump speed         *
      *                                                               *
      *       HP2    [ RPM2 ]^3                                       *
      *      ----- = [------]                                         *
      *       HP1    [ RPM1 ]                                         *
      *                                                               *
      *****************************************************************

       01 CALC-FIELDS.
              05 RPM1              PIC S9(4)V99 USAGE COMP.
              05 RPM2              PIC S9(4)V99 USAGE COMP.
              05 QUANTITY1         PIC S9(4)V99 USAGE COMP.
              05 QUANTITY2         PIC S9(4)V99 USAGE COMP.
              05 WATTS             PIC S9(5)V99 USAGE COMP.
              05 MOTOR-EFF         PIC S99V9(4) USAGE COMP.
              05 POWER-FACTOR      PIC S9V99 USAGE COMP VALUE 1.
              05 SQUARE-ROOT3      PIC 9V99 USAGE COMP VALUE 1.
              05 LAW               PIC 9 USAGE COMP.
              05 YES-NO            PIC X.

       01 DISP-FIELDS.
              05 RPM1-OUT          PIC ZZZ9 USAGE DISPLAY.
              05 RPM2-OUT          PIC ZZZ9 USAGE DISPLAY.
              05 QUANTITY1-OUT     PIC ZZZ9.99 USAGE DISPLAY.
              05 QUANTITY2-OUT     PIC ZZZ9.99 USAGE DISPLAY.
              05 WATTS-OUT         PIC ZZ,ZZ9.9 USAGE DISPLAY.
              05 MOTOR-EFF-OUT     PIC ZZ9.99 USAGE DISPLAY.
              05 DESCRIPTION       PIC X(10).

       procedure division.
       init-ws.
              initialize calc-fields
              initialize disp-fields.

       user-input.
              display "AFFINITY LAWS CALCULATOR"
              display "Enter zero for any parameter to end the program."
              display spaces
              display "Law #1 - Flow rate changes proportionally with"
              display "         changes in fan/pump speed (RPM)"
              display spaces
              display "    #2 - Pressure changes with the SQUARE of"
              display "         changes in fan/pump speed (RPM)"
              display spaces
              display "    #3 - Horsepower changes with the CUBE of"
              display "         changes in fan/pump speed (RPM)"
              display spaces
              display "Select a law: "
              accept law

              if law = 0
                     go to end-program
              end-if
              if law > 3 or law < 0
                     go to user-input
              end-if

      *****************************************************************
      *                                                               *
      * Affinity Laws:                                                *
      *                                                               *
      * Law #1                                                        *
      * Quantity (CFM/GPM) changes proportionally with fan/pump speed *
      *                                                               *
      *       CFM2|GPM2   [ RPM2 ]^1                                  *
      *      ---------- = [------]                                    *
      *       CFM1|GPM1   [ RPM1 ]                                    *
      *                                                               *
      * Law #2                                                        *
      * Pressure varies with the SQUARE of fan/pump speed             *
      *                                                               *
      *       P2    [ RPM2 ]^2                                        *
      *      ---- = [------]                                          *
      *       P1    [ RPM1 ]                                          *
      *                                                               *
      * Law #3                                                        *
      * Horsepower varies with the CUBE of the fan/pump speed         *
      *                                                               *
      *       HP2    [ RPM2 ]^3                                       *
      *      ----- = [------]                                         *
      *       HP1    [ RPM1 ]                                         *
      *                                                               *
      *****************************************************************

              if law = 1
                     move "quantity" to description
              end-if

              if law = 2
                     move "pressure" to description
              end-if

              if law = 3
                     move "horsepower" to description
                     display spaces
                     display "Is the motor AC powered? (Y/N)"
                     accept yes-no
                     if yes-no = "Y" or "y"
                            display spaces
                            display "Enter power factor: "
                            accept power-factor
                            if power-factor = 0
                                   go to end-program
                            end-if
                            if power-factor > 1
                                   display "Power factor must be <= 1"
                                   go to user-input
                            end-if

                            display spaces
                            display "Is the AC power 3-phase? (Y/N)"
                            accept yes-no
                            if yes-no = "Y" or "y"
                                   move 1.73 to square-root3
                            end-if
                     end-if

                     display spaces
                     display "Enter motor efficiency %: "
                     accept motor-eff

                     if motor-eff = 0
                            go to end-program
                     end-if
                     if motor-eff > 100
                            display spaces
                            display "Motor efficiency % must be <= 100"
                            go to user-input
                     end-if
                     move motor-eff to motor-eff-out
                     divide 100 into motor-eff rounded
              end-if

              display spaces
              display "Enter previous " description
              accept quantity1
              if quantity1 = 0
                     go to end-program
              end-if

      *       display spaces
      *       display "Enter new " description
      *       accept quantity2
      *       if quantity2 = 0
      *              go to end-program
      *       end-if

              display spaces
              display "Enter old RPM value: "
              accept rpm1
              if rpm1 = 0
                     go to end-program
              end-if

              display spaces
              display "Enter new RPM value: "
              accept rpm2
              if rpm2 = 0
                     go to end-program
              end-if.

       calculate-it.

      *****************************************************************
      *                                                               *
      * Affinity Laws:                                                *
      *                                                               *
      * Law #1                                                        *
      * Quantity (CFM/GPM) changes proportionally with fan/pump speed *
      *                                                               *
      *       CFM2|GPM2   [ RPM2 ]^1                                  *
      *      ---------- = [------]                                    *
      *       CFM1|GPM1   [ RPM1 ]                                    *
      *                                                               *
      * Law #2                                                        *
      * Pressure varies with the SQUARE of fan/pump speed             *
      *                                                               *
      *       P2    [ RPM2 ]^2                                        *
      *      ---- = [------]                                          *
      *       P1    [ RPM1 ]                                          *
      *                                                               *
      * Law #3                                                        *
      * Horsepower varies with the CUBE of the fan/pump speed         *
      *                                                               *
      *       HP2    [ RPM2 ]^3                                       *
      *      ----- = [------]                                         *
      *       HP1    [ RPM1 ]                                         *
      *                                                               *
      *****************************************************************

              compute quantity2 = quantity1 * (rpm2 / rpm1) ** law

              if law = 3
                     compute watts = quantity2 * 746 *
                                     square-root3 * power-factor
                     move watts to watts-out
              end-if

              move rpm1 to rpm1-out
              move rpm2 to rpm2-out
              move quantity1 to quantity1-out
              move quantity2 to quantity2-out.

       disp-result.
              display spaces
              display "Old RPM: " rpm1-out
              display "New RPM: " rpm2-out
              display "Old " description " : " quantity1-out
              display "New " description " : " quantity2-out
              if law = 3
                     display "Motor efficiency: " motor-eff-out "%"
                     display "Motor electrical power: " watts-out
                     " watts"
              end-if.

       end-program.
              stop run.
