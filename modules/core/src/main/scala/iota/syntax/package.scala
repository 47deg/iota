package iota  //#=cats
package iotaz //#=scalaz

package object syntax {

  object inject  extends InjectSyntax
  object injectK extends InjectKSyntax

  object all
      extends InjectSyntax
      with    InjectKSyntax
}
