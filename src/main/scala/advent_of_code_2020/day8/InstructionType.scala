package advent_of_code_2020.day8

sealed trait InstructionType

case object ACC extends InstructionType
case object JMP extends InstructionType
case object NOP extends InstructionType

object InstructionType {

  def getInstructionTypeFromString(s: String): InstructionType = {
    s match {
      case "acc" => ACC
      case "jmp" => JMP
      case "nop" => NOP
    }
  }

  def switchNopToJmpType(instructionType: InstructionType): InstructionType = {
    instructionType match {
      case JMP => NOP
      case NOP => JMP
    }
  }
}

case class Instruction(
    instructionType: InstructionType,
    nr: Int
)
