package org.dennybritz.sampler

trait InputParser[A] {
  def parse(input: A) : DataInput
}