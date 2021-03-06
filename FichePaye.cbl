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
       FILE-CONTROL.
           SELECT clientFile
               ASSIGN TO "client.txt" ORGANIZATION IS LINE SEQUENTIAL .
      *-----------------------
       DATA DIVISION.
       FILE SECTION.
       FD clientFile.
      *CLIENT
       01 f-client.
           02 f-infoClient.
               03 f-clientId PIC 9(5).
               03 f-nom PIC X(15).
               03 f-prenom PIC X(15).
               03 f-adresse.
                   04 f-rue PIC X(20).
                   04 f-codePostal PIC 9(5).
                   04 f-ville PIC X(15).
           02 f-produitFinancier.
               03 f-intitule PIC X(50).
               03 f-somme PIC $*****,999.
               03 f-dateCreation PIC 99/99/9999.
      *-----------------------
       WORKING-STORAGE SECTION.
      *CLIENT
       01 WS-client.
           02 infoClient.
               03 clientId PIC 9(5).
               03 nom PIC X(15).
               03 prenom PIC X(15).
               03 adresse.
                   04 rue PIC X(20).
                   04 codePostal PIC 9(5).
                   04 ville PIC X(15).
           02 produitFinancier.
               03 intitule PIC X(50).
               03 somme PIC $*****V999.
               03 dateCreation PIC 99/99/9999.

       01 End-Of-File PIC X.
           88 EOF VALUE "Y".

      *-----------------------
      *
      *
      *    OSKUR !
      *
      *     ######################################################################################
      *     #                                                                                    #
      *     #                            ,.--------._                                            #
      *     #                           /            ''.                                         #
      *     #                         ,'                \     |"\                /\          /\  #
      *     #                /"|     /                   \    |__"              ( \\        // ) #
      *     #               "_"|    /           z#####z   \  //                  \ \\      // /  #
      *     #                 \\  #####        ##------".  \//                    \_\\||||//_/   #
      *     #                  \\/-----\     /          ".  \                      \/ _  _ \     #
      *     #                   \|      \   |   ,,--..       \                    \/|(O)(O)|     #
      *     #                   | ,.--._ \  (  | ##   \)      \                  \/ |      |     #
      *     #                   |(  ##  )/   \ `-....-//       |///////////////_\/  \      /     #
      *     #                     '--'."      \                \              //     |____|      #
      *     #                  /'    /         ) --.            \            ||     /      \     #
      *     #               ,..|     \.________/    `-..         \   \       \|     \ 0  0 /     #
      *     #            _,##/ |   ,/   /   \           \         \   \       U    / \_//_/      #
      *     #          :###.-  |  ,/   /     \        /' ""\      .\        (     /              #
      *     #         /####|   |   (.___________,---',/    |       |\=._____|  |_/               #
      *     #        /#####|   |     \__|__|__|__|_,/             |####\    |  ||                #
      *     #       /######\   \      \__________/                /#####|   \  ||                #
      *     #      /|#######`. `\                                /#######\   | ||                #
      *     #     /++\#########\  \                      _,'    _/#########\ | ||                #
      *     #    /++++|#########|  \      .---..       ,/      ,'##########.\|_||  Donkey By     #
      *     #   //++++|#########\.  \.              ,-/      ,'########,+++++\\_\\ Hard'96       #
      *     #  /++++++|##########\.   '._        _,/       ,'######,''++++++++\                  #
      *     # |+++++++|###########|       -----."        _'#######' +++++++++++\                 #
      *     # |+++++++|############\.     \\     //      /#######/++++ S@yaN +++\                #
      *     #      ________________________\\___//______________________________________         #
      *     #     / ____________________________________________________________________)        #
      *     #    / /              _                                             _                #
      *     #    | |             | |                                           | |               #
      *     #     \ \            | | _           ____           ____           | |  _            #
      *     #      \ \           | || \         / ___)         / _  )          | | / )           #
      *     #  _____) )          | | | |        | |           (  __ /          | |< (            #
      *     # (______/           |_| |_|        |_|            \_____)         |_| \_)           #
      *     #                                                                           19.08.02 #
      *     ######################################################################################
      *CHOIX MAIN
       01 WS-choix PIC 9.
       01 nbLine PIC 99 VALUE 1.
       01 BOOL PIC 9 VALUE 1.

       SCREEN SECTION.
       01 CLEAR-SCREEN BLANK SCREEN.
       01 s-Client.
           02 ss-clientID.
               03 LINE 3 COL 8 VALUE 'Num CLient :'.
               03 s-clientId PIC 9(5) TO clientId REQUIRED.
           02 ss-nom.
               03 LINE 6 COL 8 VALUE 'Nom :'.
               03 s-nom PIC x(15) TO nom REQUIRED.
           02 ss-prenom.
               03 LINE 7 COL 8 VALUE'Prenom :'.
               03 s-prenom PIC X(15) TO prenom REQUIRED.
       01 s-Adresse.
           02 ss-rue.
               03 LINE 10 COL 8 VALUE 'Rue :'.
               03 s-rue PIC X(20) TO rue REQUIRED.
           02 ss-codePostal.
               03 LINE 11 COL 8 VALUE 'Code Postal :'.
               03 s-codePostal PIC X(5) TO codePostal REQUIRED.
           02 ss-ville.
               03 LINE 12 COL 8 VALUE'Ville :'.
               03 s-ville PIC X(15) TO ville REQUIRED.
       01 s-ProduitFinancier.
           02 ss-intitule.
               03 LINE 15 COL 8 VALUE 'Intitule :'.
               03 s-intiutle PIC X(50) TO intitule REQUIRED.
           02 ss-somme.
               03 LINE 16 COL 8 VALUE 'Somme (Format 0,00):'.
               03 s-somme PIC $*****V99 TO somme REQUIRED.
           02 ss-DateCrea.
               03 LINE 17 COL 8 VALUE 'Date de creation :'.
               03 s-dateCrea PIC 99/99/9999 TO dateCreation REQUIRED.
       01 ss-choix.
           02 LINE 2 COL 8 VALUE 'FAITE VOTRE CHOIX'.
           02 LINE 3 COL 8 VALUE '- (0) Creation de CLIENT'.
           02 LINE 4 COL 8 VALUE '- (1) Affichage CLIENT'.
           02 LINE 5 COL 8 VALUE '- (2) List CLIENT'.
           02 LINE 6 COL 8 VALUE '- (3) QUIT'.
           02 LINE 8 COL 12.
           02 s-choix PIC 9 TO WS-choix REQUIRED.

       01  aff-fiche.
           02 LINE 2 COL 8 VALUE '-- Num Client --'.
           02 LINE 3 COL 8 PIC 99999 FROM clientId REQUIRED.
           02 LINE 5 COL 8 VALUE '-- CLIENT -- '.
           02 LINE 6 COL 8 PIC x(15) FROM nom REQUIRED.
           02 LINE 7 COL 8 PIC x(15) FROM prenom REQUIRED.
           02 LINE 9 COL 8 VALUE '-- ADRESSE -- '.
           02 LINE 10 COL 8 PIC X(20) FROM rue REQUIRED.
           02 LINE 11 COL 8 PIC X(5) FROM codePostal.
           02 LINE 12 COL 8 PIC X(15) FROM ville.
           02 LINE 14 COL 8 VALUE'-- PRODUIT FINANCIER --'.
           02 LINE 15 COL 8 PIC X(50) FROM intitule.
           02 LINE 16 COL 8 PIC $*****V99 FROM somme.
           02 LINE 17 COL 8 PIC 99/99/9999 FROM dateCreation.




       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

       MAIN.
       PERFORM UNTIL BOOL = 0
           DISPLAY CLEAR-SCREEN
           ACCEPT ss-choix

           if WS-choix = 0 THEN
               DISPLAY CLEAR-SCREEN
      *    CREATION D'UN CLIENT + ADRESSE + FINANCE
               OPEN EXTEND clientFile

               PERFORM GetClientDetail
                       MOVE WS-client to f-client
                       Write f-client

               CLOSE clientFile

           ELSE IF WS-choix = 1 THEN
               DISPLAY CLEAR-SCREEN
               PERFORM ClientAff
               MOVE 0 TO BOOL

           ELSE IF WS-choix = 2 THEN
           DISPLAY CLEAR-SCREEN
           PERFORM ListAff
           MOVE 0 TO BOOL

           ELSE
             DISPLAY CLEAR-SCREEN
             DISPLAY 'EXIT'
             MOVE 0 TO BOOL
           END-IF
       END-PERFORM.

           STOP RUN.








      ** add other procedures here
       GetClientDetail.
           ACCEPT ss-clientID
           ACCEPT ss-nom
           ACCEPT ss-prenom
           ACCEPT ss-rue
           ACCEPT ss-codePostal
           ACCEPT ss-ville
           ACCEPT ss-intitule
           ACCEPT ss-somme
           ACCEPT ss-dateCrea.


       ListAff.
           OPEN INPUT clientFile
              READ clientFile AT END SET EOF TO TRUE
               END-READ
           PERFORM UNTIL EOF
               READ clientFile NEXT RECORD INTO f-client
                  AT END SET EOF TO TRUE
               END-READ
                   MOVE f-client TO WS-client
                   DISPLAY clientId SPACE nom SPACE prenom SPACE ville
               READ clientFile AT END SET EOF TO TRUE
               END-READ
           END-PERFORM
               CLOSE clientFile.



       ClientAff.
           ACCEPT ss-clientID
           OPEN INPUT clientFile

           PERFORM UNTIL EOF
               READ clientFile NEXT RECORD INTO f-client
                  AT END SET EOF TO TRUE
               END-READ
               IF clientId = f-clientId THEN
                   MOVE f-client TO WS-client
                   DISPLAY CLEAR-SCREEN
                   DISPLAY aff-fiche
               END-IF
           END-PERFORM
               CLOSE clientFile.

       END PROGRAM YOUR-PROGRAM-NAME.
