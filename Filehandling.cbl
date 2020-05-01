       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-HANDLING.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT MY-FILE ASSIGN TO "MYFILE.TXT"
       	ORGANIZATION IS LINE SEQUENTIAL
       	FILE STATUS IS F-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD MY-FILE.
       01 MY-REC.
       	02 STUDNO PIC 9(10).
       	02 STUDNAME PIC X(30).
       	02 STUDPROGRAM PIC X(10).
       	02 CHOICE PIC 9.
       	
       WORKING-STORAGE SECTION.
       77 F-STATUS PIC XX.
       	88 F-SUCCESS VALUE'00'.
       
       77 X-STUDNO PIC 9(9).
       
       77 END-OF-FILE PIC X.
       	88 END-REACH VALUE'Y'.
       
       PROCEDURE DIVISION.
      
       DISPLAY "[1] CREATE [2]APPEND [3]READ [4] UPDATE";
       ACCEPT CHOICE;
       IF CHOICE = '1' 
       PERFORM FILECREATE
       ELSE IF CHOICE = '2'THEN
       PERFORM FILEAPPEND 
       ELSE IF CHOICE = '3'THEN
       PERFORM FILEREAD 
       ELSE IF CHOICE='4'THEN
       PERFORM FILEUPDATE
       
       END-IF.
       
       CLOSE MY-FILE.
       STOP RUN.
       
       
       FILEUPDATE.
       DISPLAY "SEARCH STUDNO: " WITH NO ADVANCING.
       ACCEPT X-STUDNO.
       OPEN I-O MY-FILE.
       
       IF F-SUCCESS
      	 PERFORM READ-MY-REC
      	 	UNTIL END-REACH
      	 	
       ELSE
        	DISPLAY "ERROR" F-STATUS
       END-IF.
        READ-MY-REC.
        READ MY-FILE
        	AT END
        		MOVE 'Y' TO END-OF-FILE
        		DISPLAY 'RECORD NOT FOUND'
        	NOT AT END
        		IF X-STUDNO = STUDNO
        	
      	  		DISPLAY STUDNO
      	  		DISPLAY STUDNAME
      	  		DISPLAY STUDPROGRAM
      	  		
      	  		ACCEPT STUDNO
      	  		ACCEPT STUDNAME
      	  		ACCEPT STUDPROGRAM
      	  		
      	  		REWRITE MY-REC
      	  		MOVE 'Y' TO END-OF-FILE
      	  	END-IF
      	  
        	END-READ.
       FILECREATE. 	
        OPEN OUTPUT MY-FILE.
       
       IF F-SUCCESS
      	  DISPLAY "ENTER STUDENT NO: "
      	  ACCEPT STUDNO
      	  DISPLAY "ENTER STUDNET NAME: "
      	  ACCEPT STUDNAME
      	  DISPLAY "ENTER STUDENT PROGRAM: "
      	  ACCEPT STUDPROGRAM
      	  WRITE MY-REC
       ELSE
        	DISPLAY "ERROR" F-STATUS
       END-IF.
       
       FILEAPPEND.
       OPEN EXTEND MY-FILE.
       
       IF F-SUCCESS
      	  DISPLAY "ENTER STUDENT NO: "
      	  ACCEPT STUDNO
      	  DISPLAY "ENTER STUDNET NAME: "
      	  ACCEPT STUDNAME
      	  DISPLAY "ENTER STUDENT PROGRAM: "
      	  ACCEPT STUDPROGRAM
      	  WRITE MY-REC
       ELSE
        	DISPLAY "ERROR" F-STATUS
       END-IF.
       FILEREAD.
       OPEN INPUT MY-FILE.
       
       IF F-SUCCESS
      	 PERFORM READ-MY-REC
      	 	UNTIL END-REACH
      	 	
       ELSE
        	DISPLAY "ERROR" F-STATUS
       END-IF.
       READ-MY-REC2.
        READ MY-FILE
        	AT END
        		MOVE 'Y' TO END-OF-FILE
        	NOT AT END
        		DISPLAY STUDNO
        		DISPLAY STUDNAME
        		DISPLAY STUDPROGRAM
        		DISPLAY ""
        	END-READ.