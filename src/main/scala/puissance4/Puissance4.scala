package puissance4

import scala.util.Random

object Puissance4 {

  // Initilisation des types
  type Plateau = Array[Array[Int]]
  type Couleur = String
  type Direction = (Int, Int)

  // Initilisation des constantes
  val rouge: Couleur = "Rouge"
  val jaune: Couleur = "Jaune"
  val couleurs: List[Couleur] = List(rouge, jaune)

  // Vecteur des directions
  val haut: Direction = (-1, 0)
  val bas: Direction = (1, 0)
  val gauche: Direction = (0, -1)
  val droite: Direction = (0, 1)
  val diagonalGaucheHaut: Direction = (-1, -1)
  val diagonalGaucheBas: Direction = (1, -1)
  val diagonalDroiteBas: Direction = (1, 1)
  val diagonalDroiteHaut: Direction = (-1, 1)

  // Difficulté de l'ordinateur
  val difficulteIA = 6

  /**
    * Retourne le numéro de la couleur l'identifiant dans le plateau
    *
    * @param couleur la couleur de type Couleur
    * @return un entier correspondant à la couleur
    */
  def numeroCouleur(couleur: Couleur): Int = couleur match {
    case (`rouge`) => 0
    case (`jaune`) => 1
    case (_)       => -1
  }

  /**
    * Retourne la couleur associer à un numéro
    *
    * @param numero numero de la couleur
    * @return couleur correspondante
    */
  def couleurNumero(numero: Int): Couleur = numero match {
    case (0) => rouge
    case (1) => jaune
    case (_) => ""
  }

  /**
    * Retourne l'autre couleur
    *
    * @param couleur la couleur
    * @return autre couleur
    */
  def couleurAdversaire(couleur: Couleur) = couleur match {
    case (`rouge`) => jaune
    case (`jaune`) => rouge
  }

  /**
    * Récupère l'entrée d'un utilisateur et vérifie que cette entrée est
    * bien un entier compris entre les bornes définies en paramètre.
    * la fonction demande l'entier tant que l'entrée n'est pas correct.
    *
    * @param msg message à afficher
    * @param min borne inférieur
    * @param max borne supérieur
    * @return entier entré par l'utilisateur
    */
  def lireEntier(msg: String, min: Int = 0, max: Int = Int.MaxValue): Int = {
    var x = 0
    try {
      print(msg)
      System.out.flush
      x = scala.io.StdIn.readInt()

      // Vérification du format et de la valeur entrée
      if (x < min || x > max) throw new NumberFormatException("")
      else x
    } catch {
      // Affiche l'erreur et redemande un entier à l'utilisateur
      case e: NumberFormatException => {
        if (max != Int.MaxValue)
          println(s"Veuillez entrer un entier entre $min et $max !")
        else
          println(s"Veuillez entrer un entier au dessus de $min")
        lireEntier(msg, min, max)
      }
    }
  }

  /**
    * Initialise un plateau vide
    *
    * @param lignes nombre de lignes
    * @param colonnes nombre de colonnes
    * @return Plateau de taille lignes * colonnes
    */
  def initialisePlateau(lignes: Int, colonnes: Int): Plateau =
    Array.fill(lignes)(Array.fill(colonnes)(-1))

  /**
    * Crée un plateau en fonction de la taille entrée par l'utilisateur
    * Le plateau peut au minimum avoir une taille de 4*4
    *
    * @return Plateau de taille lignes * colonnes
    */
  def creePlateau(): Plateau = {
    // Récupération de la taille auprès de l'utilisateur
    var lignes = lireEntier("Nombre de lignes du plateau : ", 4)
    var colonnes = lireEntier("Nombre de colonnes du plateau : ", 4)

    println(s"Initialisation du plateau $lignes x $colonnes\n")
    initialisePlateau(lignes, colonnes)
  }

  /**
    * Affiche le plateau avec son contenu et l'indice de chaque colonne
    */
  def affichePlateau(plateau: Plateau): Unit = {
    Range(0, plateau.length * 2).map(i => {
      if (i == 0) {
        print("+")
        Range(0, plateau(0).length * 2).map(k => {
          k % 2 match {
            case (0) => print("---")
            case (1) => print("+")
          }
        })
        println()
      }

      Range(0, plateau(0).length * 2).map(j => {
        if (j == 0) {
          i % 2 match {
            case (0) => print("|")
            case (1) => print("+")
          }
        }

        (i % 2, j % 2) match {
          case (0, 0) => {
            plateau(i / 2)(j / 2) match {
              case (0) =>
                print(" " + Console.RED + "X" + Console.RESET + " ")
              case (1) =>
                print(" " + Console.YELLOW + "O" + Console.RESET + " ")
              case (_) =>
                print("   ")
            }
          }
          case (1, 1) => print("+")
          case (1, _) => print("---")
          case (_, 1) => print("|")
          case (_, _) => print("")
        }
      })

      if (i == plateau.length * 2 - 1) {
        println()
        Range(0, plateau(0).length).map(k => {
          print(s"  $k ")
        })
      }
      println()

    })
  }

  /**
    * Compte le nombre de pion identique aligné par rapport à une cordonnée et
    * une direction de manière récursive
    *
    * @param plateau plateau du jeu
    * @param i coordonnée de la ligne
    * @param j coordonnée de la colonne
    * @param direction direction où compter
    * @return Nombre de pion aligné
    */
  def compteAligne(
      plateau: Plateau,
      i: Int,
      j: Int,
      direction: Direction
  ): Int = {
    // Cordonnées suivantes
    var iSuivant = i + direction._1
    var jSuivant = j + direction._2

    // Vérification qu'on ne sorte pas du plateau
    if (iSuivant >= 0 &&
        iSuivant < plateau.length &&
        jSuivant >= 0 &&
        jSuivant < plateau(0).length) {
      // Vérification que le pion suivant est de la même couleur que le pion actuelle
      if (plateau(i)(j) == plateau(iSuivant)(jSuivant) && plateau(i)(j) != -1)
        1 + compteAligne(plateau, iSuivant, jSuivant, direction)
      else 0
    } else 0
  }

  /**
    * Compte le nombre de pion identique aligné à l'horizontale par rapport
    * à une cordonnée
    *
    * @param plateau plateau du jeu
    * @param i coordonnée de la ligne
    * @param j coordonnée de lacolonne
    * @return Nombre de pion aligné
    */
  def compteHorizontale(plateau: Plateau, i: Int, j: Int): Int =
    compteAligne(plateau, i, j, gauche) + compteAligne(plateau, i, j, droite) + 1

  /**
    * Compte le nombre de pion identique aligné à la verticale par rapport
    * à une cordonnée
    *
    * @param plateau plateau du jeu
    * @param i coordonnée de la ligne
    * @param j coordonnée de lacolonne
    * @return Nombre de pion aligné
    */
  def compteVerticale(plateau: Plateau, i: Int, j: Int): Int =
    compteAligne(plateau, i, j, haut) + compteAligne(plateau, i, j, bas) + 1

  /**
    * Compte le nombre de pion identique aligné sur la première diagonale
    * par rapport à une cordonnée
    *
    * @param plateau plateau du jeu
    * @param i coordonnée de la ligne
    * @param j coordonnée de lacolonne
    * @return Nombre de pion aligné
    */
  def compteDiagonale1(plateau: Plateau, i: Int, j: Int): Int =
    compteAligne(plateau, i, j, diagonalGaucheHaut) +
      compteAligne(plateau, i, j, diagonalDroiteBas) + 1

  /**
    * Compte le nombre de pion identique aligné sur la deuxième diagonale
    * par rapport à une cordonnée
    *
    * @param plateau plateau du jeu
    * @param i coordonnée de la ligne
    * @param j coordonnée de lacolonne
    * @return Nombre de pion aligné
    */
  def compteDiagonale2(plateau: Plateau, i: Int, j: Int): Int =
    compteAligne(plateau, i, j, diagonalGaucheBas) +
      compteAligne(plateau, i, j, diagonalDroiteHaut) + 1

  /**
    * Vérifie s'il exsite un gagnant sur le plateau (4 pions alignés) ou
    * s'il y a une égalité pleine (0 case vide)
    *
    * @param plateau plateau du jeu
    */
  def existeGagnant(
      plateau: Plateau
  ): (Boolean, Couleur) = {

    /**
      * Fonction recursive vérifiant s'il exsite un gagnant sur le plateau (4 pions alignés) ou
      * s'il y a une égalité pleine (0 case vide)
      *
      * @param plateau plateau du jeu
      * @param i coordonnée de la ligne
      * @param j coordonnée de lacolonne
      * @param nombreCaseVide accumulateur pour le nombre de cases vides
      * @return (Booléen, String) résultat du parcours du plateau
      */
    def existeGagnantRec(
        plateau: Plateau,
        i: Int,
        j: Int,
        nombreCaseVide: Int
    ): (Boolean, Couleur) = {
      // Vérification aux cordonnées s'il y a 4 pion aligné dans toutes les directions
      if (compteHorizontale(plateau, i, j) >= 4 ||
          compteVerticale(plateau, i, j) >= 4 ||
          compteDiagonale1(plateau, i, j) >= 4 ||
          compteDiagonale2(plateau, i, j) >= 4)
        (true, couleurNumero(plateau(i)(j)))
      else {
        var lignes = plateau.length - 1
        var colonnes = plateau(0).length - 1
        var nouveauNombreCaseVide = nombreCaseVide

        // Si la case est vide
        if (plateau(i)(j) == -1)
          nouveauNombreCaseVide += 1

        // Si on est à la fin du plateau
        if (i == lignes && j == colonnes) {
          nouveauNombreCaseVide match {
            case 0 => (false, "Égalité")
            case _ => (false, "")
          }
        } else if (i == lignes) {
          // Passage à la colonne suivante
          existeGagnantRec(plateau, 0, j + 1, nouveauNombreCaseVide)
        } else {
          // Passage à la ligne suivante
          existeGagnantRec(plateau, i + 1, j, nouveauNombreCaseVide)
        }
      }
    }
    existeGagnantRec(plateau, 0, 0, 0)
  }

  /**
    * Vérifie si une colonne n'est pas pleine
    *
    * @param plateau plateau du jeu
    * @param numeroColonne numéro de la colonne à vérifier
    * @return Booléen
    */
  def colonneJouable(plateau: Plateau, numeroColonne: Int): Boolean = {
    plateau(0)(numeroColonne) == -1
  }

  /**
    * Permette à un utilisateur de choisir une colonne en vérifiant
    * qu'elle est jouable. Redemande sinon.
    *
    * @param plateau plateau du jeu
    * @return Entier correspondant à la colonne choisie
    */
  def choixColonne(plateau: Plateau): Int = {
    var choix = lireEntier(
      "Sur quelle colonne voulez-vous jouer ? ",
      0,
      plateau(0).length - 1
    )

    // Vérification si la colonne est jouable
    if (colonneJouable(plateau, choix)) {
      choix
    } else {
      println("Cette colonne est pleine ! Veuillez en choisir une autre.")
      // Demande à l'utilisateur de choisir une autre colonne
      choixColonne(plateau)
    }
  }

  /**
    * Donne le nombre de case restante sur une colonne donnée
    *
    * @param plateau plateau du jeu
    * @param colonne dont où calcul le nombre de case restante
    * @return Entier correspondant au nombre de case restante
    */
  def caseRestante(plateau: Plateau, colonne: Int): Int = {

    /**
      * Vérifie à une ligne donnée si la case est vide ou non
      *
      * @param plateau plateau du jeu
      * @param colonne dont où calcul le nombre de case restante
      * @param ligne index de la ligne à évaluer
      * @return entier 1 ou 0 si on compte la case
      */
    def casRestanteRec(plateau: Plateau, colonne: Int, ligne: Int): Int = {
      // Vérification qu'on ne sorte pas du plateau
      if (ligne < plateau.length) {
        // Compte ou non la case à la ligne
        plateau(ligne)(colonne) match {
          case (-1) => 1 + casRestanteRec(plateau, colonne, ligne + 1)
          case (_)  => 0
        }
      } else 0
    }
    casRestanteRec(plateau, colonne, 0)
  }

  /**
    * Joue un pion de la couleur et à la colonne indiquées en paramètre
    *
    * @param plateau plateau du jeu
    * @param colonne numéro de la colonne où jouer
    * @param couleur du pion à jouer
    * @return plateau avec le pion placé
    */
  def joue(plateau: Plateau, colonne: Int, couleur: Couleur): Plateau = {
    var ligne = caseRestante(plateau, colonne) - 1
    plateau(ligne)(colonne) = numeroCouleur(couleur)
    plateau
  }

  /**
    * Joue un pion de la couleur et à la colonne indiquées en paramètre pour un humain
    *
    * @param plateau plateau du jeu
    * @param colonne numéro de la colonne où jouer
    * @param couleur du pion à jouer
    * @return plateau avec le pion placé
    */
  def joueCoupHumain(
      plateau: Plateau,
      colonne: Int,
      couleurHumain: Couleur
  ): Plateau = {
    joue(plateau, colonne, couleurHumain)
  }

  /**
    * Joue une partie joueur contre joueur où chaque appel correspond à un tour
    *
    * @param plateau plateau du jeu
    * @param joueur couleur du joueur en train de jouer
    * @param fichier fihcier de sauvegarde de la partie
    * @return plateau avec le pion placé
    */
  def partieContreJoueur(
      plateau: Plateau,
      joueur: Couleur,
      fichier: String
  ): Unit = {
    // Sauvegarde de la partie
    Sauvegarde.sauvegarde(plateau, fichier)

    // Vérification si c'est la fin de la partie
    existeGagnant(plateau) match {
      // S'il y a un gagnant (4 pions alignés)
      case (true, couleurGagnante) => {
        affichePlateau(plateau)
        println(s"\nLe joueur $couleurGagnante gagne la partie !\n")
      }

      // S'il y a une égalité
      case (false, "Égalité") => {
        affichePlateau(plateau)
        println(s"\nAucun joueur ne remporte la partie !\n")
      }

      // Si aucun gagnant, ni égalité
      case (false, _) => {
        affichePlateau(plateau)
        println(s"\nAu tour du joueur $joueur !")

        // Choix de la colonne où jouer par le joueur
        var colonne = choixColonne(plateau)
        // Placement du pion à la colonne choisie
        var nouveauPlateau = joueCoupHumain(plateau, colonne, joueur)
        println("\n")

        // Tour suivant
        partieContreJoueur(nouveauPlateau, couleurAdversaire(joueur), fichier)
      }
    }
  }

  /**
    * Joue un pion de la couleur indiquée en paramètre pour un ordi
    *
    * @param plateau plateau du jeu
    * @param couleurOrdi du pion à jouer
    * @return plateau avec le pion placé
    */
  def joueCoupOrdi(plateau: Plateau, couleurOrdi: Couleur): Plateau = {
    // Recherche de la meilleure colonne à jouer par L'IA
    var colonne = IA.MinMaxAlphaBeta(plateau, difficulteIA, couleurOrdi)
    println(s"L'ordinateur joue sur la colonne $colonne")
    // Placement du pion
    joue(plateau, colonne, couleurOrdi)
  }

  /**
    * Joue une partie joueur contre ordinateur où chaque appel correspond à un tour
    *
    * @param plateau plateau du jeu
    * @param couleurHumain couleur du joueur humain
    * @param couleurOrdi couleur du joueur ordi
    * @param humainJoue booléen pour savoir s'il l'humain ou l'ordi joue
    * @param fichier fihcier de sauvegarde de la partie
    * @return plateau avec le pion placé
    */
  def partieContreOrdi(
      plateau: Plateau,
      couleurHumain: Couleur,
      couleurOrdi: Couleur,
      humainJoue: Boolean,
      fichier: String
  ): Unit = {
    // Sauvegarde de la partie
    Sauvegarde.sauvegarde(plateau, fichier)

    // Vérification si c'est la fin de la partie
    existeGagnant(plateau) match {
      // S'il y a un gagnant (4 pions alignés)
      case (true, couleurGagnante) => {
        affichePlateau(plateau)
        // Affichage du résultat en fonction de la couleur gagnante
        if (couleurGagnante == couleurHumain)
          println(s"\nLe joueur $couleurGagnante gagne la partie !\n")
        else
          println(s"\nL'ordinateur $couleurGagnante gagne la partie !\n")
      }

      // S'il y a une égalité
      case (false, "Égalité") => {
        affichePlateau(plateau)
        println(s"\nAucun joueur ne remporte la partie !\n")
      }

      // Si aucun gagnant, ni égalité
      case (false, _) => {
        // Si c'est à l'humain de jouer
        if (humainJoue) {
          affichePlateau(plateau)
          println(s"\nAu tour du joueur $couleurHumain !")

          // Choix de la colonne où jouer par le joueur
          var colonne = choixColonne(plateau)
          // Placement du pion à la colonne choisie
          var nouveauPlateau = joueCoupHumain(plateau, colonne, couleurHumain)
          println("\n")

          // Tour suivant
          partieContreOrdi(
            nouveauPlateau,
            couleurHumain,
            couleurOrdi,
            false,
            fichier
          )

          // Si c'est à l'ordinateur de jouer
        } else {
          affichePlateau(plateau)
          println(s"\nAu tour de l'ordinateur $couleurOrdi !")

          // Placement du pion par l'ordi
          var nouveauPlateau = joueCoupOrdi(plateau, couleurOrdi)
          println("\n")

          // Tour suivant
          partieContreOrdi(
            nouveauPlateau,
            couleurHumain,
            couleurOrdi,
            true,
            fichier
          )
        }
      }
    }
  }

  /**
    * Joue une partie ordinateur contre ordinateur où chaque appel correspond
    * à un tour
    *
    * @param plateau plateau du jeu
    * @param Couleur couleur du joueur qui doit jouer
    * @param fichier fihcier de sauvegarde de la partie
    * @return plateau avec le pion placé
    */
  def partieOrdiContreOrdi(
      plateau: Plateau,
      couleur: Couleur,
      fichier: String
  ): Unit = {
    // Vérification si c'est la fin de la partie
    existeGagnant(plateau) match {
      // S'il y a un gagnant (4 pions alignés)
      case (true, couleurGagnante) => {
        affichePlateau(plateau)
        println(s"\nL'ordinateur $couleurGagnante gagne la partie !\n")
      }

      // S'il y a une égalité
      case (false, "Égalité") => {
        affichePlateau(plateau)
        println(s"\nAucun joueur ne remporte la partie !\n")
      }

      // Si aucun gagnant, ni égalité
      case (false, _) => {
        println(s"Au tour de l'ordinateur $couleur !")

        // Placement du pion par l'ordi
        var nouveauPlateau = joueCoupOrdi(plateau, couleur)
        affichePlateau(plateau)

        // Sauvegarde de la partie
        Sauvegarde.sauvegarde(plateau, fichier)

        // Attente de l'appuie de la touche par l'utilisateur pour passer au tour suivant
        println("\nAppuyez sur entrer pour passer au tour suivant\n")
        System.out.flush
        var pause = scala.io.StdIn.readLine()

        // Tour suivant
        partieOrdiContreOrdi(
          nouveauPlateau,
          couleurAdversaire(couleur),
          fichier
        )
      }
    }
  }

  /**
    * Affiche et gère un menu. Gère aussi l'initialisation des parties.
    */
  def menu(): Unit = {
    println("\n### PUISSANCE 4 ###\n")
    println("(1) Jouer contre un joueur")
    println("(2) Jouer contre un ordinateur")
    println("(3) Ordinateur contre un ordinateur")
    println("(4) Quitter\n")

    var choix = lireEntier("Que voulez-vous faire ? ", 1, 4)
    println()

    choix match {
      // Utilisateur vs Utilisateur
      case 1 => {
        println("Lancement du jeu contre un joueur !\n")

        // Choix de charger une partie
        println("Voulez-cous charger une partie ?")
        println("(1) Oui")
        println("(2) Non\n")
        var choixImport = lireEntier("Votre choix : ", 1, 2)
        println()

        choixImport match {
          case 1 => {
            // Récupération de la sauvegarde
            var tmp = Sauvegarde.charge()
            var plateau = tmp._1
            var premierJoueur = tmp._2
            var sauvegarde = tmp._3

            // Lancement de la partie
            partieContreJoueur(
              plateau,
              premierJoueur,
              sauvegarde
            )
          }
          case 2 => {
            // Demande quel fichier où sauvegarder
            var sauvegarde =
              Sauvegarde.nomFichier("Nom du fichier de sauvegarde : ")
            println()

            // Initialisation et création du plateau
            var plateau = creePlateau()

            // Choix alétoire de la couleur du premier joueur
            var premierJoueur = couleurs(Random.nextInt(couleurs.size))

            // Lancement de la partie
            partieContreJoueur(
              plateau,
              premierJoueur,
              sauvegarde
            )
          }
        }

        // Retour vers le menu une fois la partie terminé
        menu()
      }

      // Utilisateur vs Ordinateur
      case 2 => {
        println("Lancement du jeu contre un ordi !\n")

        // Choix de la couleur de l'humain
        println("Liste des couleurs")
        println("(1) Rouge")
        println("(2) Jaune\n")
        var choixCouleur = lireEntier("Choissisez votre couleur : ", 1, 2)
        println()

        // Récupération de la sauvegarde
        var couleurHumain = couleurNumero(choixCouleur - 1)
        var couleurOrdi = couleurAdversaire(couleurHumain)

        // Choix de charger une partie
        println("Voulez-cous charger une partie ?")
        println("(1) Oui")
        println("(2) Non\n")
        var choixImport = lireEntier("Votre choix : ", 1, 2)
        println()

        choixImport match {
          case 1 => {
            var tmp = Sauvegarde.charge()
            var plateau = tmp._1
            var premierJoueur = tmp._2
            var sauvegarde = tmp._3

            var estHumainPremier = couleurHumain == premierJoueur

            // Lancement de la partie
            partieContreOrdi(
              plateau,
              couleurHumain,
              couleurOrdi,
              estHumainPremier,
              sauvegarde
            )
          }
          case 2 => {
            // Demande quel fichier où sauvegarder
            var sauvegarde =
              Sauvegarde.nomFichier("Nom du fichier de sauvegarde : ")
            println()

            // Initialisation et création du plateau
            var plateau = creePlateau()

            // Choix alétoire de la couleur du premier joueur
            var premierJoueur = couleurs(Random.nextInt(couleurs.size))

            var estHumainPremier = couleurHumain == premierJoueur

            // Lancement de la partie
            partieContreOrdi(
              plateau,
              couleurHumain,
              couleurOrdi,
              estHumainPremier,
              sauvegarde
            )
          }
        }

        // Retour vers le menu une fois la partie terminé
        menu()
      }

      // Ordinateur vs Ordinateur
      case 3 => {
        println("Lancement du jeu ordi contre un ordi !\n")

        // Choix de charger une partie
        println("Voulez-cous charger une partie ?")
        println("(1) Oui")
        println("(2) Non\n")
        var choixImport = lireEntier("Votre choix : ", 1, 2)
        println()

        choixImport match {
          case 1 => {
            // Récupération de la sauvegarde
            var tmp = Sauvegarde.charge()
            var plateau = tmp._1
            var premierJoueur = tmp._2
            var sauvegarde = tmp._3

            // Lancement de la partie
            partieOrdiContreOrdi(
              plateau,
              premierJoueur,
              sauvegarde
            )
          }
          case 2 => {
            // Demande quel fichier où sauvegarder
            var sauvegarde =
              Sauvegarde.nomFichier("Nom du fichier de sauvegarde : ")
            println()

            // Initialisation et création du plateau
            var plateau = creePlateau()

            // Choix alétoire de la couleur du premier joueur
            var premierJoueur = couleurs(Random.nextInt(couleurs.size))

            // Lancement de la partie
            partieOrdiContreOrdi(
              plateau,
              premierJoueur,
              sauvegarde
            )
          }
        }

        // Retour vers le menu une fois la partie terminé
        menu()
      }

      // Quiter le programme
      case 4 => {
        println("FIN DU PROGRAMME!\n")
      }

      // Autre
      case _ => menu()
    }
  }

  /**
    * Lance le puissance 4
    *
    * @param args Arguments du programme
    */
  def main(args: Array[String]): Unit = {
    // Appel du menu
    menu()
  }
}
