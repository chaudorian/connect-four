package puissance4

import scala.util.Random
import scala.util.matching.Regex
import java.io._
import scala.io.Source

object Sauvegarde {

  // Constante pour le dossier
  var dossierSauvegarde = "./sauvegarde/"

  /**
    * Récupère une chaîne entrée par l'utilisateur
    *
    * @param msg message à afficher
    * @return chaîne de caractère
    */
  def lireChaine(msg: String): String = {
    print(msg)
    System.out.flush
    scala.io.StdIn.readLine()
  }

  /**
    * Demande à l'utilisateur de rentrer un nom de fichier correct.
    * Sinon redemande.
    *
    * @param msg message à afficher
    * @return chaîne de caractère correspondant au nom du fichier
    */
  def nomFichier(msg: String): String = {
    var nom = lireChaine(msg)
    var regex = "[A-Za-z0-9_-]+([.][A-Za-z0-9]{3,4})?"

    // Vérification de l'entrée de l'utilisateur
    if (nom.matches(regex)) nom
    else {
      println("Nom de fichier incorrect !")
      nomFichier(msg)
    }
  }

  /**
    * Écrit une chaîne de caractère dans un fichier
    *
    * @param ficher fichier où écrire
    * @param msg chaîne à écrire dans le fichier
    */
  def ecrireFichier(fichier: String, msg: String): Unit = {
    // Ouverture du fichier
    val writer = new PrintWriter(new File(dossierSauvegarde + fichier))
    // Écriture
    writer.write(msg)
    // Fermeture du fichier
    writer.close()
  }

  /**
    * Lis un fichier et récupère son contenu
    *
    * @param ficher fichier à lire
    * @return Contenu du fichier
    */
  def lireFichier(fichier: String): String = {
    Source.fromFile(dossierSauvegarde + fichier).mkString

  }

  /**
    * Convertis un plateau en chaîne de caractère
    *
    * @param plateau plateau du jeu
    * @return la conversion du tableau
    */
  def exportPlateau(plateau: Puissance4.Plateau): String = {
    var chainePlateau = ""
    Range(0, plateau.length).map(i => {
      Range(0, plateau(0).length).map(j => {
        plateau(i)(j) match {
          case 0 => chainePlateau += "R"
          case 1 => chainePlateau += "J"
          case _ => chainePlateau += "V"
        }
      })
      chainePlateau += "\n"
    })
    chainePlateau
  }

  /**
    * Sauvegarde le plateau dans un fichier
    *
    * @param plateau plateau du jeu
    * @param fichier fichier où sauvegarder
    */
  def sauvegarde(plateau: Puissance4.Plateau, ficher: String): Unit = {
    var chainePlateau = exportPlateau(plateau)
    ecrireFichier(ficher, chainePlateau)
  }

  /**
    * Cherche qui doit jouer à une position de plateau donner
    * Si le joueur rouge a plus de pion, c'est au jaune de jouer
    * Si le joueur jaune a plus de pion, c'est au rouge de jouer
    * S'il y a autant de pions de chaque couleur, on retourne une couleur alétoire
    *
    * @param plateau plateau du jeu
    * @return couleur du joueur qui doit jouer
    */
  def premierJoueur(plateau: Puissance4.Plateau): Puissance4.Couleur = {

    /**
      * Compte le nombre de pion d'une couleur sur un plateau
      *
      * @param plateau plateau du jeu
      * @param couleur couleur du pion à compter
      * @return total de pions
      */
    def comptePion(
        plateau: Puissance4.Plateau,
        couleur: Puissance4.Couleur
    ): Int = {
      var total = 0
      Range(0, plateau.length).map(i => {
        Range(0, plateau(0).length).map(j => {
          if (plateau(i)(j) == Puissance4.numeroCouleur(couleur)) total += 1
        })
      })
      total
    }

    // Calcul du total de pion de chaque couleur
    var totalRouge = comptePion(plateau, Puissance4.rouge)
    var totalJaune = comptePion(plateau, Puissance4.jaune)

    // Retour de la couleur qui doit jouer
    if (totalRouge > totalJaune) Puissance4.jaune
    else if (totalJaune > totalRouge) Puissance4.rouge
    else Puissance4.couleurs(Random.nextInt(Puissance4.couleurs.size))
  }

  /**
    * Transforme une chaine de caractère en plateau de jeu
    *
    * @param chaine plateau en chaîne de caractère
    * @return couleur du joueur qui doit jouer
    */
  def importPlateau(chaine: String): Puissance4.Plateau = {
    var lignes = 0
    var colonnes = 0

    // Vérification de la cohérence au niveau de la taille des lignes
    chaine
      .split("\n")
      .map(i => {
        if (colonnes == 0) colonnes = i.length()
        if (i.length() != colonnes)
          throw new Exception(
            "Fichiers invalides ! Veuillez saisir un fichier valide."
          )
        lignes += 1;
      })

    // Vérification de la taille du plateau
    if (lignes < 4 && colonnes < 4)
      throw new Exception(
        "Fichiers invalides ! Veuillez saisir un fichier valide."
      )

    // Initilisation du plateau
    var plateau = Puissance4.initialisePlateau(lignes, colonnes)

    // Remplissage du plateau
    Range(0, lignes).map(i => {
      Range(0, colonnes).map(j => {
        chaine(i * colonnes + j + i) match {
          case 'R' => plateau(i)(j) = 0
          case 'J' => plateau(i)(j) = 1
          case _   => plateau(i)(j) = -1
        }
      })
    })

    plateau
  }

  /**
    * Charge une sauvegarde
    *
    * @return le plateau chargé, la couleur du joueur qui doit jouer, le nom du fichier où sauvegarder la partie
    */
  def charge(): (Puissance4.Plateau, Puissance4.Couleur, String) = {
    // Demande du nom du fichier à charger
    var fichier = nomFichier("Nom du fichier de sauvegarde : ")
    try {
      var chainePlateau = lireFichier(fichier)
      var plateau = importPlateau(chainePlateau)
      var couleur = premierJoueur(plateau)
      (plateau, couleur, fichier)
    } catch {
      case e: FileNotFoundException => {
        println(
          "Fichier introuvable ! Veuillez saisir un fichier valide."
        )
        charge()
      }
      case e: Exception => {
        println(e.getMessage())
        charge()
      }
    }
  }
}
