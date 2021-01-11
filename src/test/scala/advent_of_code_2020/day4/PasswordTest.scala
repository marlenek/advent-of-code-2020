package advent_of_code_2020.day4

import advent_of_code_2020.day4.Passport.{isValidExtended, validateByr, validateEcl, validateEyr, validateHcl, validateHgt, validateIyr, validatePid}
import org.scalatest.{FunSuite, _}

class PasswordTest extends FlatSpec with Matchers {

  "passport" should "be validated as correct" in {

    val passport = Passport(
      Some("1920"),
      Some("2015"),
      Some("2022"),
      Some("65in"),
      Some("#9ab7ef"),
      Some("gry"),
      Some("023598710"),
      None,
    )
    validateByr(passport) should be (true)
    validateIyr(passport) should be (true)
    validateEyr(passport) should be (true)
    validateHgt(passport) should be (true)
    validateHcl(passport) should be (true)
    validateEcl(passport) should be (true)
    validatePid(passport) should be (true)
    isValidExtended(passport) should be (true)
  }

  "passport" should "be validated as incorrect" in {

    val passport = Passport(
      Some("2023"),
      Some("1915"),
      Some("2077"),
      Some("200cm"),
      Some("#9gher"),
      Some("gryz"),
      Some("0235987412"),
      None,
    )
    validateByr(passport) should be (false)
    validateIyr(passport) should be (false)
    validateEyr(passport) should be (false)
    validateHgt(passport) should be (false)
    validateHcl(passport) should be (false)
    validateEcl(passport) should be (false)
    validatePid(passport) should be (false)
    isValidExtended(passport) should be (false)
  }
}
