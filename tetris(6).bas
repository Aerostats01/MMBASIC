' =====================================================================
' TETRIS - MMBASIC (PICOCALC)
' Commandes : Fleche Gauche/Droite = deplacer, Fleche Bas = descendre,
'             Fleche Haut = tourner, ESC = quitter.
' Zone de securite haute active (3 lignes noires en haut, meme
' technique que pour le lecteur e-book : cette zone de l'ecran pose
' probleme sur le PicoCalc et reste donc toujours noire).
' Toutes les variables sont declarees UNE SEULE FOIS (Dim) en tete de
' programme : aucune boucle ni sous-routine rappelee ne re-declare de
' variable (source de "Duplicate Definition" sinon).
' =====================================================================

CLS RGB(BLACK)
COLOR RGB(WHITE), RGB(BLACK)
FONT 1, 1
Randomize Timer

' --- Zone morte : decalage vertical de securite (3 lignes ~12px) ---
Dim Y_Offset = 36

' --- Plateau de jeu ---
Dim BoardCols = 10
Dim BoardRows = 16
Dim CellTaille = 14
Dim PlateauX = 10
Dim PlateauY = Y_Offset
Dim Plateau(9, 15)

' --- Panneau lateral (score, niveau, prochaine piece) ---
Dim PanneauX = PlateauX + (BoardCols * CellTaille) + 16

' --- Couleurs des 7 pieces (index 1 a 7) ---
Dim CouleurPiece(7)

' --- Formes de base des 7 pieces, dans une boite 4x4 (ligne, colonne) ---
Dim PieceBaseR(6, 3)
Dim PieceBaseC(6, 3)

' --- Etat de la piece en cours ---
Dim PieceActuelle = 0
Dim ProchainePiece = 0
Dim RotationActuelle = 0
Dim PosX = 0
Dim PosY = 0
Dim PieceCellR(3)
Dim PieceCellC(3)

' --- Variables de travail (Dim unique, jamais redeclarees) ---
Dim k$ = ""
Dim p = 0
Dim cell = 0
Dim rot = 0
Dim RotR = 0
Dim RotC = 0
Dim RotNR = 0
Dim RotNC = 0
Dim TestX = 0
Dim TestY = 0
Dim CollisionDetectee = 0
Dim BR = 0
Dim BC = 0
Dim col = 0
Dim row = 0
Dim r2 = 0
Dim LigneComplete = 0
Dim LignesEffacees = 0
Dim RotationSauvegardee = 0
Dim NouvelleRotation = 0
Dim RotationReussie = 0
Dim GameOver = 0
Dim Score = 0
Dim Niveau = 1
Dim LignesTotal = 0
Dim VitesseChute = 500
Dim DernierTemps = 0
Dim X_Cellule = 0
Dim Y_Cellule = 0
Dim CouleurCellule = 0
Dim IllustrationX = 0
Dim IllustrationY = 0

' --- Musique d'introduction (jingle façon Atari ST) ---
Dim NbNotesIntro = 20
Dim NoteFreq(19)
Dim NoteDuree(19)
Dim NoteIndex = 0
Dim MusiqueInterrompue = 0

' =====================================================================
' DONNEES DES 7 PIECES (I, O, T, S, Z, J, L) DANS UNE BOITE 4x4
' =====================================================================
Data 1,0, 1,1, 1,2, 1,3
Data 1,1, 1,2, 2,1, 2,2
Data 1,0, 1,1, 1,2, 2,1
Data 1,1, 1,2, 2,0, 2,1
Data 1,0, 1,1, 2,1, 2,2
Data 1,0, 2,0, 2,1, 2,2
Data 1,2, 2,0, 2,1, 2,2

For p = 0 To 6
  For cell = 0 To 3
    Read PieceBaseR(p, cell), PieceBaseC(p, cell)
  Next cell
Next p

' Petit jingle chiptune façon Atari ST : arpege rapide C5-E5-G5-C6 x2,
' puis une descente rapide et une note finale tenue.
Data 523,100, 659,100, 784,100, 1047,100, 784,100, 659,100
Data 523,100, 659,100, 784,100, 1047,100, 784,100, 659,100
Data 1047,150, 988,90, 880,90, 784,90, 698,90, 659,90, 587,90, 523,300

For p = 0 To NbNotesIntro - 1
  Read NoteFreq(p), NoteDuree(p)
Next p

CouleurPiece(1) = RGB(CYAN)
CouleurPiece(2) = RGB(YELLOW)
CouleurPiece(3) = RGB(160, 32, 240)
CouleurPiece(4) = RGB(GREEN)
CouleurPiece(5) = RGB(RED)
CouleurPiece(6) = RGB(BLUE)
CouleurPiece(7) = RGB(255, 140, 0)

GoSub EcranTitre

' =====================================================================
' INITIALISATION DE LA PARTIE
' =====================================================================
For col = 0 To BoardCols - 1
  For row = 0 To BoardRows - 1
    Plateau(col, row) = 0
  Next row
Next col

ProchainePiece = Int(Rnd(1) * 7)
GoSub NouvellePiece
DernierTemps = Timer
GoSub RedessinerPlateauComplet

' =====================================================================
' BOUCLE PRINCIPALE
' =====================================================================
BoucleJeu:
  k$ = Inkey$

  If k$ = Chr$(27) Then
    CLS RGB(BLACK)
    Text MM.HRes/2, MM.VRes/2, "Partie quittee.", "CM", 1, 1, RGB(WHITE), RGB(BLACK)
    End
  EndIf

  If GameOver = 0 Then
    If k$ = Chr$(130) Or k$ = "q" Then
      TestX = PosX - 1 : TestY = PosY
      GoSub VerifierCollision
      If CollisionDetectee = 0 Then
        GoSub EffacerPieceCourante
        PosX = TestX
        GoSub DessinerPieceCourante
      EndIf
    EndIf

    If k$ = Chr$(131) Or k$ = "d" Then
      TestX = PosX + 1 : TestY = PosY
      GoSub VerifierCollision
      If CollisionDetectee = 0 Then
        GoSub EffacerPieceCourante
        PosX = TestX
        GoSub DessinerPieceCourante
      EndIf
    EndIf

    If k$ = Chr$(129) Or k$ = "s" Then
      TestX = PosX : TestY = PosY + 1
      GoSub VerifierCollision
      If CollisionDetectee = 0 Then
        GoSub EffacerPieceCourante
        PosY = TestY
        GoSub DessinerPieceCourante
      Else
        GoSub VerrouillerPiece
      EndIf
    EndIf

    If k$ = Chr$(128) Or k$ = "z" Then
      GoSub EffacerPieceCourante
      GoSub TournerPiece
      GoSub DessinerPieceCourante
    EndIf

    If Timer - DernierTemps >= VitesseChute Then
      DernierTemps = Timer
      TestX = PosX : TestY = PosY + 1
      GoSub VerifierCollision
      If CollisionDetectee = 0 Then
        GoSub EffacerPieceCourante
        PosY = TestY
        GoSub DessinerPieceCourante
      Else
        GoSub VerrouillerPiece
      EndIf
    EndIf
  Else
    If k$ <> "" Then
      CLS RGB(BLACK)
      Text MM.HRes/2, MM.VRes/2, "Fin de partie.", "CM", 1, 1, RGB(WHITE), RGB(BLACK)
      End
    EndIf
  EndIf
GoTo BoucleJeu

' =====================================================================
' CALCUL DES CELLULES REELLES DE LA PIECE (selon rotation)
' =====================================================================
CalculerPieceCells:
  For cell = 0 To 3
    RotR = PieceBaseR(PieceActuelle, cell)
    RotC = PieceBaseC(PieceActuelle, cell)
    For rot = 1 To RotationActuelle
      RotNR = RotC
      RotNC = 3 - RotR
      RotR = RotNR
      RotC = RotNC
    Next rot
    PieceCellR(cell) = RotR
    PieceCellC(cell) = RotC
  Next cell
Return

' =====================================================================
' TEST DE COLLISION (utilise TestX, TestY comme position a tester)
' =====================================================================
VerifierCollision:
  CollisionDetectee = 0
  For cell = 0 To 3
    BC = TestX + PieceCellC(cell)
    BR = TestY + PieceCellR(cell)
    If BC < 0 Or BC >= BoardCols Or BR >= BoardRows Then
      CollisionDetectee = 1
    ElseIf BR >= 0 Then
      If Plateau(BC, BR) <> 0 Then CollisionDetectee = 1
    EndIf
  Next cell
Return

' =====================================================================
' ROTATION AVEC TENTATIVE DE DECALAGE (wall-kick simplifie)
' =====================================================================
TournerPiece:
  RotationSauvegardee = RotationActuelle
  NouvelleRotation = RotationActuelle + 1
  If NouvelleRotation > 3 Then NouvelleRotation = 0
  RotationActuelle = NouvelleRotation
  GoSub CalculerPieceCells

  RotationReussie = 0

  TestX = PosX : TestY = PosY
  GoSub VerifierCollision
  If CollisionDetectee = 0 Then RotationReussie = 1

  If RotationReussie = 0 Then
    TestX = PosX - 1 : TestY = PosY
    GoSub VerifierCollision
    If CollisionDetectee = 0 Then
      PosX = TestX
      RotationReussie = 1
    EndIf
  EndIf

  If RotationReussie = 0 Then
    TestX = PosX + 1 : TestY = PosY
    GoSub VerifierCollision
    If CollisionDetectee = 0 Then
      PosX = TestX
      RotationReussie = 1
    EndIf
  EndIf

  If RotationReussie = 0 Then
    RotationActuelle = RotationSauvegardee
    GoSub CalculerPieceCells
  EndIf
Return

' =====================================================================
' VERROUILLE LA PIECE SUR LE PLATEAU, EFFACE LES LIGNES, PIECE SUIVANTE
' =====================================================================
VerrouillerPiece:
  For cell = 0 To 3
    BC = PosX + PieceCellC(cell)
    BR = PosY + PieceCellR(cell)
    If BR >= 0 And BR < BoardRows And BC >= 0 And BC < BoardCols Then
      Plateau(BC, BR) = PieceActuelle + 1
    EndIf
  Next cell

  GoSub EffacerLignes
  If LignesEffacees > 0 Then
    Score = Score + (LignesEffacees * LignesEffacees) * 100 * Niveau
    LignesTotal = LignesTotal + LignesEffacees
    Niveau = 1 + Int(LignesTotal / 10)
    VitesseChute = 500 - (Niveau * 30)
    If VitesseChute < 80 Then VitesseChute = 80
    GoSub JouerSonPoints
  EndIf

  GoSub NouvellePiece
  GoSub RedessinerPlateauComplet
  If GameOver = 1 Then GoSub AfficherGameOver
Return

' =====================================================================
' EFFACE LES LIGNES COMPLETES ET FAIT DESCENDRE LE RESTE
' =====================================================================
EffacerLignes:
  LignesEffacees = 0
  row = BoardRows - 1
  Do While row >= 0
    LigneComplete = 1
    For col = 0 To BoardCols - 1
      If Plateau(col, row) = 0 Then LigneComplete = 0
    Next col

    If LigneComplete = 1 Then
      For r2 = row To 1 Step -1
        For col = 0 To BoardCols - 1
          Plateau(col, r2) = Plateau(col, r2 - 1)
        Next col
      Next r2
      For col = 0 To BoardCols - 1
        Plateau(col, 0) = 0
      Next col
      LignesEffacees = LignesEffacees + 1
    Else
      row = row - 1
    EndIf
  Loop
Return

' =====================================================================
' PREPARE UNE NOUVELLE PIECE (utilise ProchainePiece comme piece a venir)
' =====================================================================
NouvellePiece:
  PieceActuelle = ProchainePiece
  ProchainePiece = Int(Rnd(1) * 7)
  RotationActuelle = 0
  PosX = Int((BoardCols - 4) / 2)
  PosY = 0
  GoSub CalculerPieceCells

  TestX = PosX : TestY = PosY
  GoSub VerifierCollision
  If CollisionDetectee = 1 Then GameOver = 1
Return

' =====================================================================
' ECRAN TITRE (generique de lancement)
' Affiche le nom du jeu et un petit apercu colore des 7 pieces, puis
' attend une touche avant de demarrer la partie.
' =====================================================================
EcranTitre:
  CLS RGB(BLACK)
  Box 0, 0, MM.HRes, Y_Offset, 1, RGB(BLACK), RGB(BLACK)

  GoSub DessinerClocher

  Text MM.HRes/2, 105, "TETRIS", "CM", 1, 4, RGB(CYAN), RGB(BLACK)
  Text MM.HRes/2, 137, "Edition PicoCalc - MMBasic", "CM", 1, 1, RGB(WHITE), RGB(BLACK)

  ' Illustration : un petit apercu des 7 pieces, chacune dans sa couleur
  IllustrationX = (MM.HRes / 2) - ((7 * 26) / 2)
  IllustrationY = 158
  For p = 0 To 6
    For cell = 0 To 3
      BR = PieceBaseR(p, cell)
      BC = PieceBaseC(p, cell)
      X_Cellule = IllustrationX + (p * 26) + (BC * 6)
      Y_Cellule = IllustrationY + (BR * 6)
      Box X_Cellule, Y_Cellule, 5, 5, 1, CouleurPiece(p + 1), CouleurPiece(p + 1)
    Next cell
  Next p

  Text MM.HRes/2, 200, "GAUCHE / DROITE : Deplacer", "CM", 1, 1, RGB(GRAY), RGB(BLACK)
  Text MM.HRes/2, 212, "BAS : Descendre   HAUT : Tourner", "CM", 1, 1, RGB(GRAY), RGB(BLACK)
  Text MM.HRes/2, 224, "ESC : Quitter", "CM", 1, 1, RGB(GRAY), RGB(BLACK)

  Text MM.HRes/2, MM.VRes - 30, "Appuyez sur une touche pour commencer", "CM", 1, 1, RGB(YELLOW), RGB(BLACK)

  GoSub JouerMusiqueIntro

  If MusiqueInterrompue = 0 Then
BoucleAttenteTitre:
    If Inkey$ = "" Then GoTo BoucleAttenteTitre
  EndIf
Return

' =====================================================================
' PETIT CLOCHER D'EGLISE ORTHODOXE (bulbe dore + croix a trois barres)
' Illustration decorative affichee au-dessus du titre.
' =====================================================================
DessinerClocher:
  ' Corps de la tour
  Box (MM.HRes / 2) - 8, 67, 16, 16, 1, RGB(GRAY), RGB(GRAY)

  ' Bulbe du dome (oignon), approxime avec des rectangles empiles en
  ' largeur decroissante - uniquement des Box, pas de Circle (non fiable)
  Box (MM.HRes / 2) - 9, 63, 18, 5, 1, RGB(255,215,0), RGB(255,215,0)
  Box (MM.HRes / 2) - 8, 60, 16, 4, 1, RGB(255,215,0), RGB(255,215,0)
  Box (MM.HRes / 2) - 6, 57, 12, 4, 1, RGB(255,215,0), RGB(255,215,0)
  Box (MM.HRes / 2) - 4, 54, 8, 4, 1, RGB(255,215,0), RGB(255,215,0)
  Box (MM.HRes / 2) - 2, 51, 4, 4, 1, RGB(255,215,0), RGB(255,215,0)

  ' Fleche au sommet du dome
  Line MM.HRes / 2, 51, MM.HRes / 2, 38, 1, RGB(255,215,0)

  ' Croix orthodoxe a trois barres
  Line (MM.HRes / 2) - 2, 41, (MM.HRes / 2) + 2, 41, 1, RGB(255,215,0)
  Line (MM.HRes / 2) - 4, 45, (MM.HRes / 2) + 4, 45, 1, RGB(255,215,0)
  Line (MM.HRes / 2) - 3, 50, (MM.HRes / 2) + 3, 48, 1, RGB(255,215,0)
Return

' =====================================================================
' SON DE POINTS (petit carillon ascendant) - joue a chaque ligne
' completee, apres la mise a jour du score.
' =====================================================================
JouerSonPoints:
  Play Tone 1047, 1047, 50
  Pause 50
  Play Tone 1319, 1319, 50
  Pause 50
  Play Tone 1568, 1568, 80
  Pause 80
Return

' =====================================================================
' MUSIQUE D'INTRODUCTION (jingle chiptune) - jouee note par note ; une
' touche pressee pendant la lecture interrompt la musique et lance la
' partie directement (pas besoin d'attendre la fin du jingle).
' =====================================================================
JouerMusiqueIntro:
  NoteIndex = 0
  MusiqueInterrompue = 0
  Do While NoteIndex < NbNotesIntro And MusiqueInterrompue = 0
    Play Tone NoteFreq(NoteIndex), NoteFreq(NoteIndex), NoteDuree(NoteIndex)
    Pause NoteDuree(NoteIndex)
    If Inkey$ <> "" Then MusiqueInterrompue = 1
    NoteIndex = NoteIndex + 1
  Loop
Return

' =====================================================================
' REDESSIN COMPLET (plateau + panneau) - appele au demarrage et a
' chaque verrouillage de piece seulement (PAS a chaque mouvement, pour
' eviter le scintillement d'un CLS trop frequent).
' =====================================================================
RedessinerPlateauComplet:
  CLS RGB(BLACK)
  Box 0, 0, MM.HRes, Y_Offset, 1, RGB(BLACK), RGB(BLACK)

  ' Cadre du plateau
  Box PlateauX - 2, PlateauY - 2, (BoardCols * CellTaille) + 4, (BoardRows * CellTaille) + 4, 1, RGB(WHITE), RGB(BLACK)

  ' Cellules deja posees
  For col = 0 To BoardCols - 1
    For row = 0 To BoardRows - 1
      X_Cellule = PlateauX + (col * CellTaille)
      Y_Cellule = PlateauY + (row * CellTaille)
      If Plateau(col, row) <> 0 Then
        CouleurCellule = CouleurPiece(Plateau(col, row))
        Box X_Cellule, Y_Cellule, CellTaille - 1, CellTaille - 1, 1, CouleurCellule, CouleurCellule
      EndIf
    Next row
  Next col

  ' --- Panneau lateral (texte statique + valeurs) ---
  Text PanneauX, Y_Offset, "SCORE", "L", 1, 1, RGB(WHITE), RGB(BLACK)
  Text PanneauX, Y_Offset + 12, Str$(Score), "L", 1, 1, RGB(YELLOW), RGB(BLACK)

  Text PanneauX, Y_Offset + 32, "NIVEAU", "L", 1, 1, RGB(WHITE), RGB(BLACK)
  Text PanneauX, Y_Offset + 44, Str$(Niveau), "L", 1, 1, RGB(YELLOW), RGB(BLACK)

  Text PanneauX, Y_Offset + 64, "LIGNES", "L", 1, 1, RGB(WHITE), RGB(BLACK)
  Text PanneauX, Y_Offset + 76, Str$(LignesTotal), "L", 1, 1, RGB(YELLOW), RGB(BLACK)

  Text PanneauX, Y_Offset + 100, "SUIVANT", "L", 1, 1, RGB(WHITE), RGB(BLACK)
  For cell = 0 To 3
    BC = PieceBaseC(ProchainePiece, cell)
    BR = PieceBaseR(ProchainePiece, cell)
    X_Cellule = PanneauX + (BC * 10)
    Y_Cellule = Y_Offset + 114 + (BR * 10)
    Box X_Cellule, Y_Cellule, 9, 9, 1, CouleurPiece(ProchainePiece + 1), CouleurPiece(ProchainePiece + 1)
  Next cell

  Text PanneauX, Y_Offset + 160, "GAUCHE/DROITE", "L", 1, 1, RGB(GRAY), RGB(BLACK)
  Text PanneauX, Y_Offset + 172, ": Deplacer", "L", 1, 1, RGB(GRAY), RGB(BLACK)
  Text PanneauX, Y_Offset + 184, "BAS : Descendre", "L", 1, 1, RGB(GRAY), RGB(BLACK)
  Text PanneauX, Y_Offset + 196, "HAUT : Tourner", "L", 1, 1, RGB(GRAY), RGB(BLACK)
  Text PanneauX, Y_Offset + 208, "ESC : Quitter", "L", 1, 1, RGB(GRAY), RGB(BLACK)

  ' Piece en cours (juste posee sur le plateau fraichement redessine)
  If GameOver = 0 Then GoSub DessinerPieceCourante
Return

' =====================================================================
' EFFACE LA PIECE COURANTE A SA POSITION ACTUELLE (avant un mouvement)
' =====================================================================
EffacerPieceCourante:
  For cell = 0 To 3
    BC = PosX + PieceCellC(cell)
    BR = PosY + PieceCellR(cell)
    If BR >= 0 Then
      X_Cellule = PlateauX + (BC * CellTaille)
      Y_Cellule = PlateauY + (BR * CellTaille)
      Box X_Cellule, Y_Cellule, CellTaille - 1, CellTaille - 1, 1, RGB(BLACK), RGB(BLACK)
    EndIf
  Next cell
Return

' =====================================================================
' DESSINE LA PIECE COURANTE A SA POSITION ACTUELLE (apres un mouvement)
' =====================================================================
DessinerPieceCourante:
  For cell = 0 To 3
    BC = PosX + PieceCellC(cell)
    BR = PosY + PieceCellR(cell)
    If BR >= 0 Then
      X_Cellule = PlateauX + (BC * CellTaille)
      Y_Cellule = PlateauY + (BR * CellTaille)
      Box X_Cellule, Y_Cellule, CellTaille - 1, CellTaille - 1, 1, CouleurPiece(PieceActuelle + 1), CouleurPiece(PieceActuelle + 1)
    EndIf
  Next cell
Return

' =====================================================================
' ECRAN DE FIN DE PARTIE
' =====================================================================
AfficherGameOver:
  Box PlateauX, PlateauY + 60, (BoardCols * CellTaille), 40, 1, RGB(RED), RGB(BLACK)
  Text PlateauX + (BoardCols * CellTaille) / 2, PlateauY + 74, "GAME OVER", "CM", 1, 1, RGB(WHITE), RGB(BLACK)
  Text PlateauX + (BoardCols * CellTaille) / 2, PlateauY + 90, "Appuyez sur une touche", "CM", 1, 1, RGB(WHITE), RGB(BLACK)
  GoSub JouerMusiqueDefaite
Return

' =====================================================================
' MUSIQUE DE DEFAITE - petite descente grave et lente, jouee des
' l'affichage du "GAME OVER", juste avant que le joueur ne quitte.
' =====================================================================
JouerMusiqueDefaite:
  Play Tone 523, 523, 160
  Pause 180
  Play Tone 466, 466, 160
  Pause 180
  Play Tone 392, 392, 160
  Pause 180
  Play Tone 330, 330, 400
  Pause 420
Return
