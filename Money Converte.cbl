       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASLKDJALKSD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       
       01 USD-PHP PIC 9(2)V9(2) VALUE 51.33.
       01 CAD-PHP PIC 9(2)V9(2) VALUE 40.96.
       01 AUD-PHP PIC 9(2)V9(2) VALUE 40.25.
       01 PHP-USD PIC 9(2)V9(2) VALUE 0.02.
       01 PHP-AUD PIC 9(2)V9(3) VALUE 0.025.
       01 PHP-CAD PIC 9(2)V9(3) VALUE 0.024.
       01 XI PIC 9(9)V9(2) VALUE 0.00.
       01 SCURRENCY PIC XXX.
       01 TCURRENCY PIC XXX.
       01 CONTINUE-YN PIC X VALUE 'Y'.
       SCREEN SECTION.
       01 MAIN-SCREEN
       	BLANK SCREEN, AUTO, REQUIRED.
       	03 LINE 1 COL 25 'INTERNATIONAL MONEY REMITTANCE'.
       	03 LINE 2 COL 25 '  CURRENCY CONVERTER SYSTEM'.
       	03 LINE 3 COL 1 '------------------------------------------'.
       	03 LINE 3 COL 43 '-----------------------------------------'.
       	03 LINE 4 COL 1 'CURRENCY LIST'.
       	03 LINE 5 COL 1 '    USD (US Dollars), CAD (Canadian Dollar'.
       	03 LINE 5 COL 44 '), AUD (Australian Dollar), PHP (Philippi'.
       	03 LINE 5 COL 97 'ne Peso)'.
       	03 LINE 6 COL 1 '------------------------------------------'.
       	03 LINE 6 COL 43 '-----------------------------------------'.
       	03 LINE 7 'DO NOT PRESS "RETURN"; INSTEAD PRESS "TAB"'.
       	03 LINE 9 COL 1 'ENTER AMOUNT             : '.
       	03 LINE 9 COL 34 PIC Z(10).Z(2) TO XI.
       	03 LINE 10 COL 1 'ENTER SOURCE CURRENCY    : '.
       	03 LINE 10 COL 42 PIC XXX TO SCURRENCY.
       	03 LINE 11 COL 1 'ENTER TARGET CURRENCY    : '.
       	03 LINE 11 COL 42 PIC XXX TO TCURRENCY.
       01 CONVERTED-SCREEN
       	AUTO .
      * 	03 LINE 1 COL 25 'INTERNATIONAL MONEY REMITTANCE'.
      * 	03 LINE 2 COL 25 '  CURRENCY CONVERTER SYSTEM'.
      	03 LINE 12 COL 1 '------------------------------------------'.
       	03 LINE 12 COL 43 '-----------------------------------------'.
       	03 LINE 13 COL 1 'CONVERTED AMOUNT         : '.
       	03 LINE 13 COL 35 PIC Z(9).Z(2) FROM  XI.
       	03 LINE 14 COL 1 '------------------------------------------'.
       	03 LINE 14 COL 43 '--------------------------------------'.
       	03 LINE 15 COL 34 'CONTINUE? Y/N'.
       	03 LINE 15 COL 48 PIC X TO CONTINUE-YN.
       	
       	 
       	
       	
       PROCEDURE DIVISION.
       PERFORM  METH UNTIL CONTINUE-YN IS EQUAL TO 'N'.
       
       STOP RUN.
       
       
       CONVERSION-METHOD.
      *>USD 
       IF SCURRENCY IS EQUAL TO 'USD' AND TCURRENCY IS EQUAL TO 'PHP' 
       THEN MULTIPLY XI BY USD-PHP GIVING XI
        
       ELSE IF SCURRENCY IS EQUAL TO 'USD' AND TCURRENCY IS EQUAL TO
        'AUD'
       THEN MULTIPLY XI BY USD-PHP GIVING XI MULTIPLY XI BY PHP-AUD
       GIVING XI
        
       ELSE IF SCURRENCY IS EQUAL TO 'USD' AND TCURRENCY IS EQUAL TO 
       'CAD' 
       THEN MULTIPLY XI BY USD-PHP GIVING XI 
       MULTIPLY XI BY PHP-CAD GIVING XI
      *>CAD
       ELSE IF SCURRENCY IS EQUAL TO 'CAD' AND TCURRENCY IS EQUAL TO 
       'PHP'
       THEN MULTIPLY XI BY CAD-PHP GIVING XI
       
       ELSE IF SCURRENCY IS EQUAL TO 'CAD' AND TCURRENCY IS EQUAL TO 
       'AUD' 
       THEN MULTIPLY XI BY CAD-PHP GIVING XI
       MULTIPLY XI BY PHP-AUD GIVING XI
       
       ELSE IF SCURRENCY IS EQUAL TO 'CAD' AND TCURRENCY IS EQUAL TO 
       'USD'
       THEN MULTIPLY XI BY CAD-PHP GIVING XI
       MULTIPLY XI BY PHP-USD GIVING XI
      *>AUD
       ELSE IF SCURRENCY IS EQUAL TO 'AUD' AND TCURRENCY IS EQUAL TO
       'PHP'
       THEN MULTIPLY XI BY AUD-PHP GIVING XI
       
       ELSE IF SCURRENCY IS EQUAL TO 'AUD' AND TCURRENCY IS EQUAL TO
       'CAD'
       THEN MULTIPLY XI BY AUD-PHP GIVING XI
       MULTIPLY XI BY PHP-CAD GIVING XI
       
       ELSE IF SCURRENCY IS EQUAL TO 'AUD' AND TCURRENCY IS EQUAL TO
       'USD'
       THEN MULTIPLY XI BY AUD-PHP GIVING XI
       MULTIPLY XI BY PHP-USD GIVING XI
      *>PHP
       ELSE IF SCURRENCY IS EQUAL TO 'PHP' AND TCURRENCY IS EQUAL TO
       'AUD' 
       THEN MULTIPLY XI BY PHP-AUD GIVING XI
       
       ELSE IF SCURRENCY IS EQUAL TO 'PHP' AND TCURRENCY IS EQUAL TO 
       'CAD'
       THEN MULTIPLY XI BY PHP-CAD GIVING XI
       
       ELSE IF SCURRENCY IS EQUAL TO 'PHP' AND TCURRENCY IS EQUAL TO 
       'USD' 
       THEN MULTIPLY XI BY PHP-USD GIVING XI
       	
       
       
       
       
       
       
       
       
       
       
       END-IF.
      
       ACCEPT-METHOD.
       DISPLAY MAIN-SCREEN.
       ACCEPT MAIN-SCREEN.
       
       METH.
       PERFORM ACCEPT-METHOD.
       PERFORM CONVERSION-METHOD.
       DISPLAY CONVERTED-SCREEN.
       
       ACCEPT CONVERTED-SCREEN.