package net.cyndeline.rlgraph.cartogram.rectangular.common

sealed trait Side

case object Left extends Side
case object Right extends Side
case object Top extends Side
case object Bottom extends Side
