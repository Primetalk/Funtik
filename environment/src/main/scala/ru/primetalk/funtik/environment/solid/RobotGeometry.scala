package ru.primetalk.funtik.environment.solid

/** @param maxOmega - angular velocity that corresponds to 1.0 (100%, max speed) */
case class  RobotGeometry(width: Double, wheelRadius: Double, maxOmega: Double){

  /**
   * Robot has two wheels. We know their radius and the distance between them.
   * Rotational speed of wheels - radians per second - is given.
   * We want to find linear (tangential) speed and rotational speed of the robot.
   * leftLinearSpeed = wheelRadius*leftOmega
   *
   * radius is positive when robot rotates to left (counter clockwise)
   * otherwise it is negative. The reason for this it is because
   * radius is being used for calculating orthogonal acceleration and subsequently, omega.
   * Omega is positive when rotation is counter clockwise.
   * @return
   */
  def convertTwoWheelsSpeedToSpeedAndOmega(leftFraction: Double, rightFraction: Double): SolidBodyRotationParameters = {
    val leftLinearSpeed = wheelRadius * leftFraction * maxOmega
    val rightLinearSpeed = wheelRadius * rightFraction * maxOmega
    val linearVelocity1 = (leftLinearSpeed + rightLinearSpeed) / 2
    if(leftFraction == rightFraction) {
      SolidBodyRotationParameters(linearVelocity1, 0.0)
    } else {
      // Proportion:
      // rLeft / (rLeft + wheelsDistance) = left / right
      // rLeft (1 - left/right) = left/right * wheelsDistance
      // rLeft = left/(right - left) * wheelsDistance

      // r = leftOmega / (rightOmega - leftOmega) * wheelsDistance

      // rCenter = rLeft + wheelsDistance / 2 = (left + right) / (right - left)* wheelsDistance
      // a = V^2/rCenter = V^2 * (right - left) / (left + right) / wheelsDistance
      val acceleration = linearVelocity1 * linearVelocity1 * (rightFraction - leftFraction) / (leftFraction + rightFraction) / width
      SolidBodyRotationParameters(
        tangentialVelocity = linearVelocity1,
        orthogonalAcceleration = acceleration
      )
    }
  }
}
