package advent_of_code_2020.day4

import scala.util.{Failure, Success, Try}

case class Passport(
    byr: Option[String],
    iyr: Option[String],
    eyr: Option[String],
    hgt: Option[String],
    hcl: Option[String],
    ecl: Option[String],
    pid: Option[String],
    cid: Option[String]
)

object Passport {

  def isValid(passport: Passport): Boolean = {
    passport.byr.isDefined && passport.iyr.isDefined && passport.eyr.isDefined && passport.hgt.isDefined && passport.hcl.isDefined && passport.ecl.isDefined && passport.pid.isDefined
  }

  def isValidExtended(passport: Passport): Boolean = {
    validateByr(passport) && validateIyr(passport) && validateEyr(passport) && validateHgt(passport) && validateHcl(passport) && validateEcl(passport) && validatePid(
      passport)

  }

  //byr (Birth Year) - four digits; at least 1920 and at most 2002.
  def validateByr(passport: Passport): Boolean = {
    validateYear(passport.byr, 1920, 2002)
  }

  //iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  def validateIyr(passport: Passport): Boolean = {
    validateYear(passport.iyr, 2010, 2020)
  }

  //eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  def validateEyr(passport: Passport): Boolean = {
    validateYear(passport.eyr, 2020, 2030)
  }

  def validateYear(passportEntry: Option[String], min: Int, max: Int): Boolean = {
    Try { passportEntry.get.toInt } match {
      case Success(v) => v >= min && v <= max
      case Failure(_) => false
    }
  }

  //hgt (Height) - a number followed by either cm or in:
  //If cm, the number must be at least 150 and at most 193.
  //If in, the number must be at least 59 and at most 76.
  def validateHgt(passport: Passport): Boolean = {
    Try {
      val height = passport.hgt.get
      (height.take(height.length - 2).toInt, height.takeRight(2))
    } match {
      case Success(v) => {
        if (v._2 == "cm") v._1 >= 150 && v._1 <= 193
        else if (v._2 == "in") v._1 >= 59 && v._1 <= 76
        else false

      }
      case Failure(_) => false
    }
  }

  //hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  def validateHcl(passport: Passport): Boolean = {
    Try {
      passport.hcl.get
    } match {
      case Success(v) => {
        v.length == 7 && v.matches("#[a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9][a-f0-9]")
      }
      case Failure(_) => false
    }
  }

  //ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  def validateEcl(passport: Passport): Boolean = {
    Try {
      passport.ecl.get
    } match {
      case Success(v) => {
        v.length == 3 && List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(v)
      }
      case Failure(_) => false
    }
  }

  //pid (Passport ID) - a nine-digit number, including leading zeroes.
  def validatePid(passport: Passport): Boolean = {
    Try {
      passport.pid.get
    } match {
      case Success(v) => {
        v.length == 9 && v.matches("[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]")
      }
      case Failure(_) => false
    }
  }
}
