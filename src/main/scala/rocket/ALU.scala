// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ALU
{
  val SZ_ALU_FN = 4
  def FN_X    = BitPat("b????")
  def FN_ADD  = UInt(0)
  def FN_SL   = UInt(1)
  def FN_SEQ  = UInt(2)
  def FN_SNE  = UInt(3)
  def FN_XOR  = UInt(4)
  def FN_SR   = UInt(5)
  def FN_OR   = UInt(6)
  def FN_AND  = UInt(7)
  def FN_SUB  = UInt(10)
  def FN_SRA  = UInt(11)
  def FN_SLT  = UInt(12)
  def FN_SGE  = UInt(13)
  def FN_SLTU = UInt(14)
  def FN_SGEU = UInt(15)

  def FN_DIV  = UInt(4)
  def FN_DIVU = UInt(5)
  def FN_REM  = UInt(6)
  def FN_REMU = UInt(7)

  def FN_MUL    = UInt(0)
  def FN_MULH   = UInt(1)
  def FN_MULHSU = UInt(2)
  def FN_MULHU  = UInt(3)
  def FN_MULU   = UInt(4) // For vector
  def FN_MULSU  = UInt(5) // For vector
  def FN_MACC   = UInt(6) // For vector
  def FN_MACCU  = UInt(7) // For vector
  def FN_MACCSU = UInt(8) // For vector
  def FN_MACCUS = UInt(9) // For vector

  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
}

import ALU._

class ALU(withCarryIO: Boolean = false)(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val dw = Bits(INPUT, SZ_DW)
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in2 = UInt(INPUT, xLen)
    val in1 = UInt(INPUT, xLen)
    val out = UInt(OUTPUT, xLen)
    val adder_out = UInt(OUTPUT, xLen)
    val cmp_out = Bool(OUTPUT)
    val ci = if (withCarryIO) Bool(INPUT) else null
    val co = if (withCarryIO) Bool(OUTPUT) else null
  }

  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)
  if (withCarryIO) {
    val adder: UInt = io.in1 +& in2_inv + (isSub(io.fn) ^ io.ci)
    io.adder_out := adder(xLen-1, 0)
    io.co := adder(xLen)
  }

  // SLT, SLTU
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === UInt(0), slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4,0), io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, UInt(0)) |
              Mux(io.fn === FN_SL,                     shout_l, UInt(0))

  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, UInt(0)) |
              Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, UInt(0))
  val shift_logic = (isCmp(io.fn) && slt) | logic | shout
  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)

  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}
