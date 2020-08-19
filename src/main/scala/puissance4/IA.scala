package puissance4

import scala.util.Random

object IA {

  /**
    * Compte le nombre de séquence de pions d'une certaine couleur et
    * d'une certaine longeur dans le plateau
    *
    * @param plateau plateau du jeu
    * @param longueur longeur de la séquence à compter
    * @param couleur couleur du pion à compter
    * @return Le nombre de séquence compté
    */
  def compteSequence(
      plateau: Puissance4.Plateau,
      longueur: Int,
      couleur: Puissance4.Couleur
  ): Int = {
    var total = 0
    Range(0, plateau.length)
      .map(
        i => {
          Range(0, plateau(0).length)
            .map(
              j => {
                // Si le pion sur la case est de la bonne couleur
                if (plateau(i)(j) == Puissance4.numeroCouleur(couleur)) {
                  // Pion aligné sur la droite
                  if (Puissance4
                        .compteAligne(plateau, i, j, Puissance4.droite) + 1 >= longueur)
                    total += 1

                  // Pion aligné sur le bas
                  if (Puissance4
                        .compteAligne(plateau, i, j, Puissance4.bas) + 1 >= longueur)
                    total += 1

                  // Pion aligné sur la diagonal en haut à droite
                  if (Puissance4
                        .compteAligne(
                          plateau,
                          i,
                          j,
                          Puissance4.diagonalDroiteHaut
                        ) + 1 >= longueur)
                    total += 1

                  // Pion aligné sur la diagonal en bas à droite
                  if (Puissance4
                        .compteAligne(
                          plateau,
                          i,
                          j,
                          Puissance4.diagonalDroiteBas
                        ) + 1 >= longueur)
                    total += 1
                }
              }
            )
        }
      )
    total
  }

  /**
    * Évalue le plateau et lui donne un score par rapport à un joueur
    *
    * @param plateau plateau du jeu
    * @param couleurJoueur couleur du joueur à évaluer le score du plateau
    * @return Entier correspondant au score du plateau
    */
  def scorePlateau(
      plateau: Puissance4.Plateau,
      couleurJoueur: Puissance4.Couleur
  ): Int = {

    var joueurQuatre = compteSequence(plateau, 4, couleurJoueur)
    var joueurTrois = compteSequence(plateau, 3, couleurJoueur)
    var joueurDeux = compteSequence(plateau, 2, couleurJoueur)
    var scoreJoueur = joueurQuatre * 99999 + joueurTrois * 999 + joueurDeux * 99

    var couleurAdversaire = Puissance4.couleurAdversaire(couleurJoueur)

    var adversaireQuatre = compteSequence(plateau, 4, couleurAdversaire)
    var adversaireTrois = compteSequence(plateau, 3, couleurAdversaire)
    var adversaireDeux = compteSequence(plateau, 2, couleurAdversaire)
    var scoreAdversaire = adversaireQuatre * 99999 + adversaireTrois * 999 + adversaireDeux * 99

    // Si l'adversaire à gagner
    if (adversaireQuatre > 0) Int.MinValue
    else scoreJoueur - scoreAdversaire
  }

  /**
    * Retourne une liste contenant les colonnes jouables sur le plateau
    *
    * @param plateau plateau du jeu
    * @return Liste d'index du plateau
    */
  def coupPossible(plateau: Puissance4.Plateau): IndexedSeq[Int] = {
    Range(0, plateau(0).length).filter(x => plateau(0)(x) == -1)
  }

  /**
    * Vérifie si c'est la fin du jeu pour un plateau donné
    *
    * @param plateau plateau du jeu
    * @return Booléen
    */
  def finDuJeu(plateau: Puissance4.Plateau): Boolean = {
    Puissance4.existeGagnant(plateau) match {
      case (true, _)          => true
      case (false, "Égalité") => true
      case _                  => false
    }
  }

  /**
    * Minimise Beta = minimise score de l'adversaire
    *
    * @param plateau plateau du jeu
    * @param niveau Profondeur de l'exploration
    * @param a alpha
    * @param b beta
    * @param couleurJoueur couleur du joueur
    * @param couleurAdversaire couleur de l'adversaire
    * @return Score du plateau
    */
  def minimiseBeta(
      plateau: Puissance4.Plateau,
      niveau: Int,
      a: Int,
      b: Int,
      couleurJoueur: Puissance4.Couleur,
      couleurAdversaire: Puissance4.Couleur
  ): Int = {
    var coups = coupPossible(plateau)

    if (niveau == 0 || coups.length == 0 || finDuJeu(plateau)) {
      scorePlateau(plateau, couleurJoueur)
    } else {
      var beta = b

      // Recherche du coup qui minimise le score de l'adversaire
      coups.map(i => {
        var score = Int.MaxValue

        if (a < beta) {
          var nouveauPlateau =
            Puissance4.joue(plateau.map(_.clone), i, couleurAdversaire)

          score = maximiseAlpha(
            nouveauPlateau,
            niveau - 1,
            a,
            beta,
            couleurJoueur,
            couleurAdversaire
          )
        }

        if (score < beta) beta = score
      })

      beta
    }
  }

  /**
    * Maximise Alpha = meilleur score du joueur
    *
    * @param plateau plateau du jeu
    * @param niveau Profondeur de l'exploration
    * @param a alpha
    * @param b beta
    * @param couleurJoueur couleur du joueur
    * @param couleurAdversaire couleur de l'adversaire
    * @return Score du plateau
    */
  def maximiseAlpha(
      plateau: Puissance4.Plateau,
      niveau: Int,
      a: Int,
      b: Int,
      couleurJoueur: Puissance4.Couleur,
      couleurAdversaire: Puissance4.Couleur
  ): Int = {
    var coups = coupPossible(plateau)

    if (niveau == 0 || coups.length == 0 || finDuJeu(plateau)) {
      scorePlateau(plateau, couleurJoueur)
    } else {
      var alpha = a

      // Recherche du coup qui maximise le score du joueur
      coups.map(i => {

        var score = Int.MinValue

        if (alpha < b) {
          var nouveauPlateau =
            Puissance4.joue(plateau.map(_.clone), i, couleurJoueur)

          score = minimiseBeta(
            nouveauPlateau,
            niveau - 1,
            alpha,
            b,
            couleurJoueur,
            couleurAdversaire
          )
        }

        if (score > alpha) alpha = score
      })

      alpha
    }
  }

  /**
    * Algorithme AlphaBeta cherchant à trouver le meilleur coup à faire sur
    * un plateau. Il détermine ce dernier en évaluant le score des plateaux avec
    * les prochains coups des joueurs.
    *
    * @param plateau plateau du jeu
    * @param niveau Profondeur de l'exploration
    * @param couleurJoueur couleur du joueur
    * @return Score du plateau
    */
  def MinMaxAlphaBeta(
      plateau: Puissance4.Plateau,
      niveau: Int,
      couleur: Puissance4.Couleur
  ): Int = {
    var coups = Random.shuffle(coupPossible(plateau))

    var meilleurCoup = coups(0)
    var meilleurScore = Int.MinValue

    var alpha = Int.MinValue
    var beta = Int.MaxValue

    var couleurAdversaire = Puissance4.couleurAdversaire(couleur)

    // Évaluation du meilleur coups
    coups.map(i => {
      var nouveauPlateau = Puissance4.joue(plateau.map(_.clone), i, couleur)

      var score = minimiseBeta(
        nouveauPlateau,
        niveau - 1,
        alpha,
        beta,
        couleur,
        couleurAdversaire
      )

      // println(s"Colonne $i : $score")

      // Si meilleur score trouvé
      if (score > meilleurScore) {
        meilleurScore = score
        meilleurCoup = i
      }
    })

    meilleurCoup
  }
}
