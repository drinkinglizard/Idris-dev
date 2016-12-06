module Control.Algebra.Lattice.Dropped

import Control.Algebra.Lattice

data Dropped a = Top
               | Drop a

JoinSemiLattice a => JoinSemiLattice (Dropped a) where
  join Top       _       = Top
  join _         Top     = Top
  join (Drop x) (Drop y) = Drop (x `join` y)

MeetSemiLattice a => MeetSemiLattice (Dropped a) where
  meet Top      drop_y   = drop_y
  meet drop_x   Top      = drop_x
  meet (Drop x) (Drop y) = Drop (x `meet` y)

Lattice a => Lattice (Dropped a) where

BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Dropped a) where
  bottom = Drop bottom

BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Dropped a) where
  top = Top

BoundedLattice a => BoundedLattice (Dropped a) where
