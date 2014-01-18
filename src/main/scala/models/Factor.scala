package org.dennybritz.sampler

case class FactorVariable(id: Int, isPositive: Boolean)
case class Factor(id: Int, variables: List[FactorVariable], weightId: Int, 
  function : FactorFunction)

