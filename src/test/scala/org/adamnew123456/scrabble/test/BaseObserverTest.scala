package org.adamnew123456.scrabble.test

import scala.collection.mutable.HashMap
import org.adamnew123456.scrabble.BaseObservable

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

/**
 * This ensures that the BaseObserver works as intended.
 */
class BaseObserableTest extends TestCase {
  class ObservableBox extends BaseObservable[Int] {
    private var value: Int = _
    
    def get = value
    def set(newValue: Int) = {
      value = newValue
      notifyObservers(newValue)
    }
  }
  
  def testSingleObserver {
    val observable = new ObservableBox()
    
    var observerTriggered = false
    var observerValue: Option[Int] = None
    val observer = { value: Int =>
      observerTriggered = true
      observerValue = Some(value)
    }
    observable.attachObserver(observer)
    
    observable.set(5)
    assertEquals(observable.get, 5)
    assertTrue(observerTriggered)
    assertEquals(observerValue, Some(5))
  }
  
  def testMultipleObservers {
    val observerValues = HashMap[Int => Unit, Int]()
    
    val observables = new ObservableBox()
    
    lazy val observerA: Int => Unit = {
      value: Int => observerValues(observerA) = value
    }
      
    lazy val observerB: Int => Unit = {
      value: Int => observerValues(observerB) = value
    }
    
    val observable = new ObservableBox()
    observable.attachObserver(observerA)
    observable.attachObserver(observerB)
    observable.set(5)
    
    assertEquals(observerValues(observerA), 5)
    assertEquals(observerValues(observerB), 5)
  }
  
  def testObserverRemoval {
    val observerValues = HashMap[Int => Unit, Int]()
    
    val observables = new ObservableBox()
    
    lazy val observerA: Int => Unit = {
      value: Int => observerValues(observerA) = value
    }
      
    lazy val observerB: Int => Unit = {
      value: Int => observerValues(observerB) = value
    }
    
    val observable = new ObservableBox()
    observable.attachObserver(observerA)
    observable.attachObserver(observerB)
    
    observable.detachObserver(observerB)
    observable.set(5)
    
    assertEquals(observerValues(observerA), 5)
    assertFalse(observerValues.contains(observerB))
  }
}