package org.adamnew123456.scrabble

import scala.collection.mutable.HashMap
import scala.util.{ Try, Success, Failure, Random }

/**
 * An exception for when somebody tries to take or swap more tiles than are
 * available.
 */
case class NoTilesError(tilesRequested: Int, tilesLeft: Int)
    extends Exception {
  override def toString = s"NoTilesError(requested: $tilesRequested, left: $tilesLeft)"
}

/**
 * An exception for when somebody tries to remove tiles from a TileGroup that
 * aren't there.
 */
case class NotInTileGroup(tiles: TileGroup) extends Exception {
  override def toString = s"NotInTileGroup(${tiles.asList.mkString(" ")})"
}

/**
 * A group of tiles. Internally, this is represented as a Map[Char, Int], but
 * it can be converted for convenience.
 */
class TileGroup(tiles: Map[Char, Int]) {
  /**
   * Checks to see if the tile group has one of the given type of tile.
   */
  val contains = tiles.contains(_)

  /**
   * Counts the number of the given type of tile in the group.
   */
  val count = tiles(_)

  /**
   * Checks for equality between this and another TileGroup.
   */
  override def equals(other: Any) = other match {
    case otherGroup: TileGroup =>
      otherGroup.asMap == this.asMap
    case _ => false
  }

  /**
   * Gets this TileGroup as a list of tiles.
   */
  def asList: List[Char] =
    tiles.flatMap {
      case (tile, count) =>
        List.fill(count)(tile)
    }.toList

  /**
   * Get this TileGroup as a map of tile -> count.
   */
  def asMap: Map[Char, Int] = tiles.filter {case (tile, count) => count > 0}

  /**
   * Gets the number of tiles in this group.
   */
  def size = tiles.values.sum

  /**
   * Merges this TileGroup with another, to produce a TileGroup containing
   * tiles from both groups.
   */
  def merge(other: TileGroup): TileGroup = {
    val total = new HashMap[Char, Int]

    tiles.foreach {
      case (tile, count) =>
        if (!total.contains(tile)) {
          total(tile) = 0
        }

        total(tile) += count
    }

    other.asMap.foreach {
      case (tile, count) =>
        if (!total.contains(tile)) {
          total(tile) = 0
        }

        total(tile) += count
    }

    new TileGroup(total.toMap)
  }

  /**
   * Removes the contents of another TileGroup from this one.
   */
  def remove(other: TileGroup): Try[TileGroup] = {
    val total = new HashMap[Char, Int]
    total ++= tiles

    val nonexistentTiles = new HashMap[Char, Int]

    other.asMap.foreach {
      case (tile, count) =>
        if (!total.contains(tile) || count > total(tile)) {
          nonexistentTiles(tile) = count
        } else {
          total(tile) -= count
        }
    }

    if (nonexistentTiles.isEmpty) {
      Success(new TileGroup(total.toMap))
    } else {
      val extraTiles = new TileGroup(nonexistentTiles.toMap)
      Failure(NotInTileGroup(extraTiles))
    }
  }
}

object TileGroup {
  /**
   * Makes a tile group from a list of tiles.
   */
  def fromTraversable(in: Traversable[Char]): TileGroup = {
    // [a, b, a] --> {a -> [a, a], b -> [b]}
    val countedTiles = in.groupBy { tile: Char => tile }.mapValues(_.size)
    new TileGroup(countedTiles)
  }

  /**
   * Makes a tile group from a map of tile -> count.
   */
  def fromMap(in: Map[Char, Int]): TileGroup =
    new TileGroup(in)
}

/**
 * The tile bag is responsible for holding the tiles in the game. It starts
 * off with a particular distribution of tiles, and can then have tiles taken
 * out, or swapped.
 */
class TileBag(tileDistribution: Map[Char, Int], randomGen: Random) {
  def this(tileDistribution: Map[Char, Int]) =
    this(tileDistribution, new Random)

  val tiles = new HashMap[Char, Int]()
  tiles ++= tileDistribution

  /**
   * Computes the number of remaining tiles.
   */
  def tilesLeft = tiles.values.sum

  /**
   * Whether or not any tiles are left.
   */
  def isEmpty = tilesLeft > 0

  /**
   * Draws the given number of tiles out of the bag. If there are too few, then
   * this produces a NoTilesError.
   */
  def drawTiles(numTiles: Int): Try[TileGroup] = {
    if (numTiles > tilesLeft) {
      Failure(NoTilesError(numTiles, tilesLeft))
    } else {
      val drawnTiles = (1 to numTiles).map { _ =>
        val tilesCounts = tiles.toList

        val expandedTiles = TileGroup.fromMap(tiles.toMap).asList
        val shuffledTiles = randomGen.shuffle(expandedTiles).toList

        val tileIndex = Math.abs(randomGen.nextInt % shuffledTiles.length)
        val letterPicked = shuffledTiles(tileIndex)
        tiles(letterPicked) -= 1
        letterPicked
      }

      Success(TileGroup.fromTraversable(drawnTiles))
    }
  }

  /**
   * Puts the given tiles back into the bag, and pulls out new ones. Note that
   * the new tiles are pulled out before the old, to avoid getting the very
   * same tiles as before.
   */
  def replaceTiles(toReplace: TileGroup): Try[TileGroup] = {
    if (toReplace.size > tilesLeft) {
      Failure(NoTilesError(toReplace.size, tilesLeft))
    } else {
      val tilesToReturn = drawTiles(toReplace.size) match {
        case Success(tiles) => tiles

        // This cannot fail - the only way for this to occur is if there aren't
        // enough tiles, and we checked for that. This is really just to satisfy
        // the compiler.
        case Failure(_)     => null
      }

      toReplace.asList.foreach(tiles(_) += 1)
      Success(tilesToReturn)
    }
  }
}
