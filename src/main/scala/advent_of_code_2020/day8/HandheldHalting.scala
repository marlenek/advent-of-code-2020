package advent_of_code_2020.day8

import advent_of_code_2020.Solution
import advent_of_code_2020.day8.InstructionType.switchNopToJmpType

object HandheldHalting extends Solution[Int] {

  override def day: Int = 8

  override def part1(): Int = {
    visitInstruction(readInput(), false).get
  }

  def readInput(): IndexedSeq[Instruction] = {
    inputAsStringLines
      .map(
        line =>
          Instruction(InstructionType.getInstructionTypeFromString(line.take(3)),
                      line.split(" ")(1).toInt))
      .toIndexedSeq
  }

  def visitInstruction(instructions: IndexedSeq[Instruction],
                       isFullVisitRequired: Boolean): Option[Int] = {
    val visited     = Array.fill(instructions.length)(0)
    val arrayLength = visited.length - 1
    var acc         = 0
    var index       = 0
    while (visited(index) == 0 && index < arrayLength) {
      visited(index) = 1
      val instruction = instructions(index)
      instruction.instructionType match {
        case ACC => {
          acc = acc + instruction.nr
          index = index + 1
        }
        case NOP => index = index + 1
        case JMP => index = index + instruction.nr
      }
    }
    if (isFullVisitRequired && index != arrayLength)
      None
    else Some(acc)
  }

  override def part2(): Int = {

    val instructionWithChangedElement = readInput()

    readInput()
      .filter(instruction =>
        instruction.instructionType == JMP || instruction.instructionType == NOP)
      .map(jmpNopInstruction =>
        instructionWithChangedElement.updated(
          readInput().indexOf(jmpNopInstruction),
          Instruction(switchNopToJmpType(jmpNopInstruction.instructionType), jmpNopInstruction.nr)))
      .map(newInstruction => visitInstruction(newInstruction, true))
      .filter(_.isDefined)
      .head
      .get
  }

}
