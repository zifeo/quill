package io.getquill.monad

sealed trait Effect
sealed trait Read extends Effect
sealed trait Write extends Effect
