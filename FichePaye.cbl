      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-----------------------
       WORKING-STORAGE SECTION.

      *CLIENT
       01 WS-client.
           02 WS-nom PIC X(15).
           02 WS-prenom PIC X(15).
           02 WS-adresse.
               03 WS-rue PIC X(20).
               03 WS-codePostal PIC 9(5).
               03 WS-ville PIC X(15).
           02 WS-produitFinancier.
               03 WS-intitule PIC X(50).
               03 WS-somme PIC $***,999.
               03 WS-dateCreation PIC 99/99/9999.
      *-----------------------
      *
      *
      *    OSKUR !
      *
      *
      *-----------------------
      *CHOIX MAIN
       01 WS-choix PIC 9.
       01 BOOL PIC 9 VALUE 1.

       SCREEN SECTION.
       01 CLEAR-SCREEN BLANK SCREEN.
       01 s-Client.
           02 ss-nom.
               03 LINE 6 COL 8 VALUE 'Nom :'.
               03 s-nom PIC x(15) TO WS-nom REQUIRED.
           02 ss-prenom.
               03 LINE 7 COL 8 VALUE'Prenom :'.
               03 s-prenom PIC X(15) TO WS-prenom REQUIRED.
       01 s-Adresse.
           02 ss-rue.
               03 LINE 10 COL 8 VALUE 'Rue :'.
               03 s-rue PIC X(20) TO WS-rue REQUIRED.
           02 ss-codePostal.
               03 LINE 11 COL 8 VALUE 'Code Postal :'.
               03 s-codePostal PIC X(5) TO WS-codePostal REQUIRED.
           02 ss-ville.
               03 LINE 12 COL 8 VALUE'Ville :'.
               03 s-ville PIC X(15) TO WS-ville REQUIRED.
       01 s-ProduitFinancier.
           02 ss-intitule.
               03 LINE 15 COL 8 VALUE 'Intitule :'.
               03 s-intiutle PIC X(50) TO WS-intitule REQUIRED.
           02 ss-somme.
               03 LINE 16 COL 8 VALUE 'Somme :'.
               03 s-somme PIC $***,99 TO WS-somme REQUIRED.
           02 ss-DateCrea.
               03 LINE 17 COL 8 VALUE 'Date de creation :'.
               03 s-dateCrea PIC 99/99/9999 TO WS-dateCreation REQUIRED.
       01 ss-choix.
           02 LINE 2 COL 8 VALUE 'FAITE VOTRE CHOIX'.
           02 LINE 3 COL 8 VALUE '- (0) Creation de CLIENT'.
           02 LINE 4 COL 8 VALUE '- (1) Affichage CLIENT'.
           02 LINE 5 COL 8 VALUE '- (2) List CLIENT'.
           02 LINE 6 COL 8 VALUE '- (3) QUIT'.
           02 LINE 8 COL 12.
           02 s-choix PIC 9 TO WS-choix REQUIRED.

       01  aff-fiche.
           02 LINE 5 COL 8 VALUE '-- CLIENT -- '.
           02 LINE 6 COL 13 PIC x(15) FROM WS-nom REQUIRED.
           02 LINE 7 COL 16 PIC x(15) FROM WS-prenom REQUIRED.
           02 LINE 9 COL 8 VALUE '-- ADRESSE -- '.
           02 LINE 10 COL 13 PIC X(20) FROM WS-rue REQUIRED.
           02 LINE 11 COL 21 PIC X(5) FROM WS-codePostal.
           02 LINE 12 COL 15 PIC X(15) FROM WS-ville.
           02 LINE 14 COL 8 VALUE'-- PRODUIT FINANCIER --'.
           02 LINE 15 COL 18 PIC X(50) FROM WS-intitule.
           02 LINE 16 COL 15 PIC $***,99 FROM WS-somme.
           02 LINE 17 COL 28 PIC 99/99/9999 FROM WS-dateCreation.





       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       MAIN.
       PERFORM UNTIL BOOL = 0
           DISPLAY CLEAR-SCREEN
           ACCEPT ss-choix

           if WS-choix = 0 THEN
               DISPLAY CLEAR-SCREEN
      *    CREATION D'UN CLIENT + ADRESSE + FINANCE
               ACCEPT ss-nom
               ACCEPT ss-prenom
               ACCEPT ss-rue
               ACCEPT ss-codePostal
               ACCEPT ss-ville
               ACCEPT ss-intitule
               ACCEPT ss-somme
               ACCEPT ss-dateCrea
           ELSE IF WS-choix = 1 THEN
               DISPLAY CLEAR-SCREEN
               DISPLAY aff-fiche
           ELSE IF WS-choix = 2 THEN
               DISPLAY 'ERROR'
           ELSE
               DISPLAY CLEAR-SCREEN
               DISPLAY 'EXIT'
               MOVE 0 TO BOOL
           END-IF
       END-PERFORM.

           STOP RUN.

      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
