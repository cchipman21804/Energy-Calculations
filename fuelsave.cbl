      *****************************************************************
      * CALCULATE % FUEL SAVINGS BASED ON EFFICIENCY UPGRADE          *
      *                                                               *
      * A simple program that calculates the fuel savings percentage  *
      * based on a combustion efficiency upgrade,                     *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   fuelsave.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Percent Fuel Savings formula:                                 *
      *                                                               *
      * %FuelSavings = [(neweff - oldeff) / neweff] * 100             *
      *                                                               *
      *****************************************************************

       01 CALC-FIELDS.
              05 OLDEFF            PIC S99V99 USAGE COMP.
              05 NEWEFF            PIC S99V99 USAGE COMP.
              05 PCTEFF            PIC S99V9999 USAGE COMP.
              05 NUMERATOR         PIC S99V99 USAGE COMP.

       01 DISP-FIELDS.
              05 OLD-EFF-OUT       PIC Z9.99 USAGE DISPLAY.
              05 NEW-EFF-OUT       PIC Z9.99 USAGE DISPLAY.
              05 PCT-EFF-OUT       PIC Z9.99 USAGE DISPLAY.

       procedure division.
       init-ws.
              initialize calc-fields
              initialize disp-fields.

       user-input.
              display "PERCENT FUEL SAVINGS CALCULATOR"
              display "Enter zero for any parameter to end the program."
              display "Enter old efficiency % value: "
              accept oldeff
              if oldeff = 0
                     go to end-program
              end-if
              if oldeff > 100
                     display "Efficiency % must be <= 100"
                     go to user-input
              end-if

              display "Enter new efficiency % value: "
              accept neweff
              if neweff = 0
                     go to end-program
              end-if
              if neweff > 100
                     display "Efficiency % must be <= 100"
                     go to user-input
              end-if.

       calculate-it.

      *****************************************************************
      *                                                               *
      * Percent Fuel Savings formula:                                 *
      *                                                               *
      * %FuelSavings = [(neweff - oldeff) / neweff] * 100             *
      *                                                               *
      *****************************************************************

              subtract oldeff from neweff giving numerator
              divide numerator by neweff giving pcteff rounded

      *       compute pcteff = (neweff - oldeff) / neweff

              multiply 100 by pcteff

              move oldeff to old-eff-out
              move neweff to new-eff-out
              move pcteff to pct-eff-out.

       disp-result.
              display "Old Efficiency: " old-eff-out "%"
              display "New Efficiency: " new-eff-out "%"
              display "Fuel Savings: " pct-eff-out "%".

       end-program.
              stop run.
