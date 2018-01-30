package iota  //#=cats
package iotaz //#=scalaz

package object syntax {

  object evidence extends EvidenceSyntax
  object inject  extends InjectSyntax
  object injectK extends InjectKSyntax

  object all
      extends EvidenceSyntax
      with    InjectSyntax
      with    InjectKSyntax
}
