package org.adamnew123456.scrabble

import scala.collection.mutable.HashSet

/**
 * An object which contains some state, and which notifies other objects to
 * changes in its internal state.
 */
trait BaseObservable[T] {
  val observers = HashSet[T => Unit]()
  
  /**
   * Attaches an observer function, called whenever the observable state of
   * this object changes.
   */
  def attachObserver(fn: T => Unit) =
    observers += fn
    
  /**
   * Detaches an observer function.
   */
  def detachObserver(fn: T => Unit) =
    observers -= fn
    
  /**
   * Notifies all observers about the change in state.
   */
  protected def notifyObservers(state: T) =
    observers.foreach(_(state))
}