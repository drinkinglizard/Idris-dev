module Control.Algebra.Lattice.Lifted

import Control.Algebra.Lattice

data Lifted a = Lift a
               | Bottom

JoinSemiLattice a => JoinSemiLattice (Lifted a) where
  join (Lift x) (Lift y) = Lift (x `join` y)
  join Bottom   lift_y   = lift_y
  join lift_x   Bottom   = lift_x

MeetSemiLattice a => MeetSemiLattice (Lifted a) where
  meet (Lift x) (Lift y) = Lift (x `meet` y)
  meet Bottom   _        = Bottom
  meet _        Bottom   = Bottom

Lattice a => Lattice (Lifted a) where

BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Lifted a) where
  bottom = Bottom

BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Lifted a) where
  top = Lift top

BoundedLattice a => BoundedLattice (Lifted a) where
