package iota

package object gen {

  type Index[I <: SingletonInt]
  type Name[N <: SingletonString]

  type Meta[
    I <: SingletonInt,
    N <: SingletonString
  ] = Index[I] with Name[N]

}
