package iota  //#=cats
package iotaz //#=scalaz

package object internal {

  /** An internal type used for type list operations that don't exist on
    * all shapes of type lists.
    */
  private[internal] sealed trait Disregard

}
