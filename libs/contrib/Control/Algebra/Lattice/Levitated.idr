module Control.Algebra.Lattice.Levitated

import Control.Algebra.Lattice

data Levitated a = Top
                 | Levitate a
                 | Bottom

implementation JoinSemiLattice a => JoinSemiLattice (Levitated a) where
  join Top          _            = Top
  join _            Top          = Top
  join (Levitate x) (Levitate y) = Levitate (x `join` y)
  join Bottom       lev_y        = lev_y
  join lev_x        Bottom       = lev_x

MeetSemiLattice a => MeetSemiLattice (Levitated a) where
  meet Top          lev_y        = lev_y
  meet lev_x        Top          = lev_x
  meet (Levitate x) (Levitate y) = Levitate (x `meet` y)
  meet Bottom       _            = Bottom
  meet _            Bottom       = Bottom

Lattice a => Lattice (Levitated a) where

BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Levitated a) where
  bottom = Bottom

BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Levitated a) where
  top = Top

BoundedLattice a => BoundedLattice (Levitated a) where
