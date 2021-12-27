// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import Chisel.ImplicitConversions._
import chisel3.{DontCare, WireInit, withClock, VecInit}
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental.{chiselName, NoChiselNamePrefix}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

case class FPUParams(
  minFLen: Int = 16,
  fLen: Int = 64,
  divSqrt: Boolean = true,
  zfhExt: Boolean = true,
  hfmaLatency: Int = 3,
  sfmaLatency: Int = 3,
  dfmaLatency: Int = 4
)

object FPConstants
{
  val RM_SZ = 3
  val FLAGS_SZ = 5
}

/**
 * Set all bits at or below the highest order '1'.
 */
object Masklower
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => in >> i.U).reduce(_|_)
  }
}

trait HasFPUCtrlSigs {
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val typeTagIn = UInt(2.W)
  val typeTagOut = UInt(2.W)
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val wflags = Bool()
}

class FPUCtrlSigs extends Bundle with HasFPUCtrlSigs

class FPUDecoder(implicit p: Parameters) extends FPUModule()(p) {
  val io = new Bundle {
    val inst = Bits(INPUT, 32)
    val sigs = new FPUCtrlSigs().asOutput
  }

  val default =       List(X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)
  val h: Array[(BitPat, List[BitPat])] =
    Array(FLH      -> List(Y,Y,N,N,N,X,X,X,X,N,N,N,N,N,N,N),
          FSH      -> List(Y,N,N,Y,N,Y,X,I,H,N,Y,N,N,N,N,N),
          FMV_H_X  -> List(N,Y,N,N,N,X,X,H,I,Y,N,N,N,N,N,N),
          FCVT_H_W -> List(N,Y,N,N,N,X,X,H,H,Y,N,N,N,N,N,Y),
          FCVT_H_WU-> List(N,Y,N,N,N,X,X,H,H,Y,N,N,N,N,N,Y),
          FCVT_H_L -> List(N,Y,N,N,N,X,X,H,H,Y,N,N,N,N,N,Y),
          FCVT_H_LU-> List(N,Y,N,N,N,X,X,H,H,Y,N,N,N,N,N,Y),
          FMV_X_H  -> List(N,N,Y,N,N,N,X,I,H,N,Y,N,N,N,N,N),
          FCLASS_H -> List(N,N,Y,N,N,N,X,H,H,N,Y,N,N,N,N,N),
          FCVT_W_H -> List(N,N,Y,N,N,N,X,H,X,N,Y,N,N,N,N,Y),
          FCVT_WU_H-> List(N,N,Y,N,N,N,X,H,X,N,Y,N,N,N,N,Y),
          FCVT_L_H -> List(N,N,Y,N,N,N,X,H,X,N,Y,N,N,N,N,Y),
          FCVT_LU_H-> List(N,N,Y,N,N,N,X,H,X,N,Y,N,N,N,N,Y),
          FCVT_S_H -> List(N,Y,Y,N,N,N,X,H,S,N,N,Y,N,N,N,Y),
          FCVT_H_S -> List(N,Y,Y,N,N,N,X,S,H,N,N,Y,N,N,N,Y),
          FEQ_H    -> List(N,N,Y,Y,N,N,N,H,H,N,Y,N,N,N,N,Y),
          FLT_H    -> List(N,N,Y,Y,N,N,N,H,H,N,Y,N,N,N,N,Y),
          FLE_H    -> List(N,N,Y,Y,N,N,N,H,H,N,Y,N,N,N,N,Y),
          FSGNJ_H  -> List(N,Y,Y,Y,N,N,N,H,H,N,N,Y,N,N,N,N),
          FSGNJN_H -> List(N,Y,Y,Y,N,N,N,H,H,N,N,Y,N,N,N,N),
          FSGNJX_H -> List(N,Y,Y,Y,N,N,N,H,H,N,N,Y,N,N,N,N),
          FMIN_H   -> List(N,Y,Y,Y,N,N,N,H,H,N,N,Y,N,N,N,Y),
          FMAX_H   -> List(N,Y,Y,Y,N,N,N,H,H,N,N,Y,N,N,N,Y),
          FADD_H   -> List(N,Y,Y,Y,N,N,Y,H,H,N,N,N,Y,N,N,Y),
          FSUB_H   -> List(N,Y,Y,Y,N,N,Y,H,H,N,N,N,Y,N,N,Y),
          FMUL_H   -> List(N,Y,Y,Y,N,N,N,H,H,N,N,N,Y,N,N,Y),
          FMADD_H  -> List(N,Y,Y,Y,Y,N,N,H,H,N,N,N,Y,N,N,Y),
          FMSUB_H  -> List(N,Y,Y,Y,Y,N,N,H,H,N,N,N,Y,N,N,Y),
          FNMADD_H -> List(N,Y,Y,Y,Y,N,N,H,H,N,N,N,Y,N,N,Y),
          FNMSUB_H -> List(N,Y,Y,Y,Y,N,N,H,H,N,N,N,Y,N,N,Y),
          FDIV_H   -> List(N,Y,Y,Y,N,N,N,H,H,N,N,N,N,Y,N,Y),
          FSQRT_H  -> List(N,Y,Y,N,N,N,X,H,H,N,N,N,N,N,Y,Y))
  val f: Array[(BitPat, List[BitPat])] =
    Array(FLW      -> List(Y,Y,N,N,N,X,X,X,X,N,N,N,N,N,N,N),
          FSW      -> List(Y,N,N,Y,N,Y,X,I,S,N,Y,N,N,N,N,N),
          FMV_S_X  -> List(N,Y,N,N,N,X,X,S,I,Y,N,N,N,N,N,N),
          FCVT_S_W -> List(N,Y,N,N,N,X,X,S,S,Y,N,N,N,N,N,Y),
          FCVT_S_WU-> List(N,Y,N,N,N,X,X,S,S,Y,N,N,N,N,N,Y),
          FCVT_S_L -> List(N,Y,N,N,N,X,X,S,S,Y,N,N,N,N,N,Y),
          FCVT_S_LU-> List(N,Y,N,N,N,X,X,S,S,Y,N,N,N,N,N,Y),
          FMV_X_S  -> List(N,N,Y,N,N,N,X,I,S,N,Y,N,N,N,N,N),
          FCLASS_S -> List(N,N,Y,N,N,N,X,S,S,N,Y,N,N,N,N,N),
          FCVT_W_S -> List(N,N,Y,N,N,N,X,S,X,N,Y,N,N,N,N,Y),
          FCVT_WU_S-> List(N,N,Y,N,N,N,X,S,X,N,Y,N,N,N,N,Y),
          FCVT_L_S -> List(N,N,Y,N,N,N,X,S,X,N,Y,N,N,N,N,Y),
          FCVT_LU_S-> List(N,N,Y,N,N,N,X,S,X,N,Y,N,N,N,N,Y),
          FEQ_S    -> List(N,N,Y,Y,N,N,N,S,S,N,Y,N,N,N,N,Y),
          FLT_S    -> List(N,N,Y,Y,N,N,N,S,S,N,Y,N,N,N,N,Y),
          FLE_S    -> List(N,N,Y,Y,N,N,N,S,S,N,Y,N,N,N,N,Y),
          FSGNJ_S  -> List(N,Y,Y,Y,N,N,N,S,S,N,N,Y,N,N,N,N),
          FSGNJN_S -> List(N,Y,Y,Y,N,N,N,S,S,N,N,Y,N,N,N,N),
          FSGNJX_S -> List(N,Y,Y,Y,N,N,N,S,S,N,N,Y,N,N,N,N),
          FMIN_S   -> List(N,Y,Y,Y,N,N,N,S,S,N,N,Y,N,N,N,Y),
          FMAX_S   -> List(N,Y,Y,Y,N,N,N,S,S,N,N,Y,N,N,N,Y),
          FADD_S   -> List(N,Y,Y,Y,N,N,Y,S,S,N,N,N,Y,N,N,Y),
          FSUB_S   -> List(N,Y,Y,Y,N,N,Y,S,S,N,N,N,Y,N,N,Y),
          FMUL_S   -> List(N,Y,Y,Y,N,N,N,S,S,N,N,N,Y,N,N,Y),
          FMADD_S  -> List(N,Y,Y,Y,Y,N,N,S,S,N,N,N,Y,N,N,Y),
          FMSUB_S  -> List(N,Y,Y,Y,Y,N,N,S,S,N,N,N,Y,N,N,Y),
          FNMADD_S -> List(N,Y,Y,Y,Y,N,N,S,S,N,N,N,Y,N,N,Y),
          FNMSUB_S -> List(N,Y,Y,Y,Y,N,N,S,S,N,N,N,Y,N,N,Y),
          FDIV_S   -> List(N,Y,Y,Y,N,N,N,S,S,N,N,N,N,Y,N,Y),
          FSQRT_S  -> List(N,Y,Y,N,N,N,X,S,S,N,N,N,N,N,Y,Y))
  val d: Array[(BitPat, List[BitPat])] =
    Array(FLD      -> List(Y,Y,N,N,N,X,X,X,X,N,N,N,N,N,N,N),
          FSD      -> List(Y,N,N,Y,N,Y,X,I,D,N,Y,N,N,N,N,N),
          FMV_D_X  -> List(N,Y,N,N,N,X,X,D,I,Y,N,N,N,N,N,N),
          FCVT_D_W -> List(N,Y,N,N,N,X,X,D,D,Y,N,N,N,N,N,Y),
          FCVT_D_WU-> List(N,Y,N,N,N,X,X,D,D,Y,N,N,N,N,N,Y),
          FCVT_D_L -> List(N,Y,N,N,N,X,X,D,D,Y,N,N,N,N,N,Y),
          FCVT_D_LU-> List(N,Y,N,N,N,X,X,D,D,Y,N,N,N,N,N,Y),
          FMV_X_D  -> List(N,N,Y,N,N,N,X,I,D,N,Y,N,N,N,N,N),
          FCLASS_D -> List(N,N,Y,N,N,N,X,D,D,N,Y,N,N,N,N,N),
          FCVT_W_D -> List(N,N,Y,N,N,N,X,D,X,N,Y,N,N,N,N,Y),
          FCVT_WU_D-> List(N,N,Y,N,N,N,X,D,X,N,Y,N,N,N,N,Y),
          FCVT_L_D -> List(N,N,Y,N,N,N,X,D,X,N,Y,N,N,N,N,Y),
          FCVT_LU_D-> List(N,N,Y,N,N,N,X,D,X,N,Y,N,N,N,N,Y),
          FCVT_S_D -> List(N,Y,Y,N,N,N,X,D,S,N,N,Y,N,N,N,Y),
          FCVT_D_S -> List(N,Y,Y,N,N,N,X,S,D,N,N,Y,N,N,N,Y),
          FEQ_D    -> List(N,N,Y,Y,N,N,N,D,D,N,Y,N,N,N,N,Y),
          FLT_D    -> List(N,N,Y,Y,N,N,N,D,D,N,Y,N,N,N,N,Y),
          FLE_D    -> List(N,N,Y,Y,N,N,N,D,D,N,Y,N,N,N,N,Y),
          FSGNJ_D  -> List(N,Y,Y,Y,N,N,N,D,D,N,N,Y,N,N,N,N),
          FSGNJN_D -> List(N,Y,Y,Y,N,N,N,D,D,N,N,Y,N,N,N,N),
          FSGNJX_D -> List(N,Y,Y,Y,N,N,N,D,D,N,N,Y,N,N,N,N),
          FMIN_D   -> List(N,Y,Y,Y,N,N,N,D,D,N,N,Y,N,N,N,Y),
          FMAX_D   -> List(N,Y,Y,Y,N,N,N,D,D,N,N,Y,N,N,N,Y),
          FADD_D   -> List(N,Y,Y,Y,N,N,Y,D,D,N,N,N,Y,N,N,Y),
          FSUB_D   -> List(N,Y,Y,Y,N,N,Y,D,D,N,N,N,Y,N,N,Y),
          FMUL_D   -> List(N,Y,Y,Y,N,N,N,D,D,N,N,N,Y,N,N,Y),
          FMADD_D  -> List(N,Y,Y,Y,Y,N,N,D,D,N,N,N,Y,N,N,Y),
          FMSUB_D  -> List(N,Y,Y,Y,Y,N,N,D,D,N,N,N,Y,N,N,Y),
          FNMADD_D -> List(N,Y,Y,Y,Y,N,N,D,D,N,N,N,Y,N,N,Y),
          FNMSUB_D -> List(N,Y,Y,Y,Y,N,N,D,D,N,N,N,Y,N,N,Y),
          FDIV_D   -> List(N,Y,Y,Y,N,N,N,D,D,N,N,N,N,Y,N,Y),
          FSQRT_D  -> List(N,Y,Y,N,N,N,X,D,D,N,N,N,N,N,Y,Y))
  val fcvt_hd: Array[(BitPat, List[BitPat])] =
    Array(FCVT_H_D -> List(N,Y,Y,N,N,N,X,D,H,N,N,Y,N,N,N,Y),
          FCVT_D_H -> List(N,Y,Y,N,N,N,X,H,D,N,N,Y,N,N,N,Y))

  val insns = (minFLen, fLen) match {
    case (32, 32) => f
    case (16, 32) => h ++ f
    case (32, 64) => f ++ d
    case (16, 64) => h ++ f ++ d ++ fcvt_hd

    case other => throw new Exception(s"minFLen = ${minFLen} & fLen = ${fLen} is an unsupported configuration")
  }
  val decoder = DecodeLogic(io.inst, default, insns)
  val s = io.sigs
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.typeTagIn, s.typeTagOut, s.fromint, s.toint,
                 s.fastpipe, s.fma, s.div, s.sqrt, s.wflags)
  sigs zip decoder map {case(s,d) => s := d}
}

class FPUCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val hartid = Input(UInt(hartIdLen.W))
  val time = Input(UInt(xLen.W))

  val inst = Bits(INPUT, 32)
  val fromint_data = Bits(INPUT, xLen)

  val fcsr_rm = Bits(INPUT, FPConstants.RM_SZ)
  val fcsr_flags = Valid(Bits(width = FPConstants.FLAGS_SZ))

  val store_data = Bits(OUTPUT, fLen)
  val toint_data = Bits(OUTPUT, xLen)

  val dmem_resp_val = Bool(INPUT)
  val dmem_resp_type = Bits(INPUT, 3)
  val dmem_resp_tag = UInt(INPUT, 5)
  val dmem_resp_data = Bits(INPUT, fLen)

  val valid = Bool(INPUT)
  val fcsr_rdy = Bool(OUTPUT)
  val nack_mem = Bool(OUTPUT)
  val illegal_rm = Bool(OUTPUT)
  val killx = Bool(INPUT)
  val killm = Bool(INPUT)
  val dec = new FPUCtrlSigs().asOutput
  val sboard_set = Bool(OUTPUT)
  val sboard_clr = Bool(OUTPUT)
  val sboard_clra = UInt(OUTPUT, 5)

  val keep_clock_enabled = Bool(INPUT)
}

class FPUIO(implicit p: Parameters) extends FPUCoreIO ()(p) {
  val cp_req = Decoupled(new FPInput()).flip //cp doesn't pay attn to kill sigs
  val cp_resp = Decoupled(new FPResult())
}

class FPResult(implicit p: Parameters) extends CoreBundle()(p) {
  val data = Bits(width = fLen+1)
  val exc = Bits(width = FPConstants.FLAGS_SZ)
}

class IntToFPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(width = FPConstants.RM_SZ)
  val typ = Bits(width = 2)
  val in1 = Bits(width = xLen)
}

class FPInput(val vector: Boolean = false)(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(width = FPConstants.RM_SZ)
  val fmaCmd = Bits(width = 2)
  val typ = Bits(width = 2)
  val fmt = Bits(width = 2)
  val in1 = if (vector) Bits(width = vLen) else Bits(width = fLen+1)
  val in2 = if (vector) Bits(width = vLen) else Bits(width = fLen+1)
  val in3 = if (vector) Bits(width = vLen) else Bits(width = fLen+1)

  override def cloneType = new FPInput(vector).asInstanceOf[this.type]
}

case class FType(exp: Int, sig: Int) {
  def ieeeWidth = exp + sig
  def recodedWidth = ieeeWidth + 1

  def ieeeQNaN = UInt((BigInt(1) << (ieeeWidth - 1)) - (BigInt(1) << (sig - 2)), ieeeWidth)
  def qNaN = UInt((BigInt(7) << (exp + sig - 3)) + (BigInt(1) << (sig - 2)), recodedWidth)
  def isNaN(x: UInt) = x(sig + exp - 1, sig + exp - 3).andR
  def isSNaN(x: UInt) = isNaN(x) && !x(sig - 2)

  def classify(x: UInt) = {
    val sign = x(sig + exp)
    val code = x(exp + sig - 1, exp + sig - 3)
    val codeHi = code(2, 1)
    val isSpecial = codeHi === UInt(3)

    val isHighSubnormalIn = x(exp + sig - 3, sig - 1) < UInt(2)
    val isSubnormal = code === UInt(1) || codeHi === UInt(1) && isHighSubnormalIn
    val isNormal = codeHi === UInt(1) && !isHighSubnormalIn || codeHi === UInt(2)
    val isZero = code === UInt(0)
    val isInf = isSpecial && !code(0)
    val isNaN = code.andR
    val isSNaN = isNaN && !x(sig-2)
    val isQNaN = isNaN && x(sig-2)

    Cat(isQNaN, isSNaN, isInf && !sign, isNormal && !sign,
        isSubnormal && !sign, isZero && !sign, isZero && sign,
        isSubnormal && sign, isNormal && sign, isInf && sign)
  }

  // convert between formats, ignoring rounding, range, NaN
  def unsafeConvert(x: UInt, to: FType) = if (this == to) x else {
    val sign = x(sig + exp)
    val fractIn = x(sig - 2, 0)
    val expIn = x(sig + exp - 1, sig - 1)
    val fractOut = fractIn << to.sig >> sig
    val expOut = {
      val expCode = expIn(exp, exp - 2)
      val commonCase = (expIn + (1 << to.exp)) - (1 << exp)
      Mux(expCode === 0 || expCode >= 6, Cat(expCode, commonCase(to.exp - 3, 0)), commonCase(to.exp, 0))
    }
    Cat(sign, expOut, fractOut)
  }

  private def ieeeBundle = {
    val expWidth = exp
    class IEEEBundle extends Bundle {
      val sign = Bool()
      val exp = UInt(expWidth.W)
      val sig = UInt((ieeeWidth-expWidth-1).W)
      override def cloneType = new IEEEBundle().asInstanceOf[this.type]
    }
    new IEEEBundle
  }

  def unpackIEEE(x: UInt) = x.asTypeOf(ieeeBundle)

  def recode(x: UInt) = hardfloat.recFNFromFN(exp, sig, x)
  def ieee(x: UInt) = hardfloat.fNFromRecFN(exp, sig, x)
}

object FType {
  val H = new FType(5, 11)
  val S = new FType(8, 24)
  val D = new FType(11, 53)

  val all = List(H, S, D)
}

trait HasFPUParameters {
  require(fLen == 0 || FType.all.exists(_.ieeeWidth == fLen))
  val minFLen: Int
  val fLen: Int
  def xLen: Int
  val minXLen = 32
  val nIntTypes = log2Ceil(xLen/minXLen) + 1
  val floatTypes = FType.all.filter(t => minFLen <= t.ieeeWidth && t.ieeeWidth <= fLen)
  def minType = floatTypes.head
  def maxType = floatTypes.last
  def prevType(t: FType) = floatTypes(typeTag(t) - 1)
  def maxExpWidth = maxType.exp
  def maxSigWidth = maxType.sig
  def typeTag(t: FType) = floatTypes.indexOf(t)
  def typeTagWbOffset = UInt(FType.all.indexOf(minType) + 1)
  def typeTagGroup(t: FType) = UInt(if (floatTypes.contains(t)) typeTag(t) else typeTag(maxType))
  // typeTag
  def H = typeTagGroup(FType.H)
  def S = typeTagGroup(FType.S)
  def D = typeTagGroup(FType.D)
  def I = UInt(typeTag(maxType))

  private def isBox(x: UInt, t: FType): Bool = x(t.sig + t.exp, t.sig + t.exp - 4).andR

  private def box(x: UInt, xt: FType, y: UInt, yt: FType): UInt = {
    require(xt.ieeeWidth == 2 * yt.ieeeWidth)
    val swizzledNaN = Cat(
      x(xt.sig + xt.exp, xt.sig + xt.exp - 3),
      x(xt.sig - 2, yt.recodedWidth - 1).andR,
      x(xt.sig + xt.exp - 5, xt.sig),
      y(yt.recodedWidth - 2),
      x(xt.sig - 2, yt.recodedWidth - 1),
      y(yt.recodedWidth - 1),
      y(yt.recodedWidth - 3, 0))
    Mux(xt.isNaN(x), swizzledNaN, x)
  }

  // implement NaN unboxing for FU inputs
  def unbox(x: UInt, tag: UInt, exactType: Option[FType], isInMaxType: Boolean = true): UInt = {
    val outType = exactType.getOrElse(maxType)
    def helper(x: UInt, t: FType): Seq[(Bool, UInt)] = {
      val prev =
        if (t == minType) {
          Seq()
        } else {
          val prevT = prevType(t)
          val unswizzled = Cat(
            x(prevT.sig + prevT.exp - 1),
            x(t.sig - 1),
            x(prevT.sig + prevT.exp - 2, 0))
          val prev = helper(unswizzled, prevT)
          val isbox = isBox(x, t)
          prev.map(p => (isbox && p._1, p._2))
        }
      prev :+ (true.B, t.unsafeConvert(x, outType))
    }

    val (oks, floats) = if(isInMaxType) helper(x, maxType).unzip else helper(x, outType).unzip
    //val (oks, floats) = helper(x, outType).unzip
    if (exactType.isEmpty || floatTypes.size == 1) {
      Mux(oks(tag), floats(tag), maxType.qNaN)
    } else {
      val t = exactType.get
      //floats(typeTag(t)) | Mux(oks(typeTag(t)), 0.U, t.qNaN)
      floats(tag) | Mux(oks(tag), 0.U, t.qNaN)
    }
  }

  def unboxh(x: UInt, tag: UInt, exactType: Option[FType]): UInt = {
    val outType = exactType.getOrElse(FType.S)
    def helper(x: UInt, t: FType): Seq[(Bool, UInt)] = {
      val prev =
        if (t == minType) {
          Seq()
        } else {
          val prevT = prevType(t)
          val unswizzled = Cat(
            x(prevT.sig + prevT.exp - 1),
            x(t.sig - 1),
            x(prevT.sig + prevT.exp - 2, 0))
          val prev = helper(unswizzled, prevT)
          val isbox = isBox(x, t)
          prev.map(p => (isbox && p._1, p._2))
        }
      prev :+ (true.B, t.unsafeConvert(x, outType))
    }

    val (oks, floats) = helper(x, FType.S).unzip
    if (exactType.isEmpty || floatTypes.size == 1) {
      Mux(oks(tag), floats(tag), FType.S.qNaN)
    } else {
      val t = exactType.get
      floats(typeTag(t)) | Mux(oks(typeTag(t)), 0.U, t.qNaN)
    }
  }

  // make sure that the redundant bits in the NaN-boxed encoding are consistent
  def consistent(x: UInt): Bool = {
    def helper(x: UInt, t: FType): Bool = if (typeTag(t) == 0) true.B else {
      val prevT = prevType(t)
      val unswizzled = Cat(
        x(prevT.sig + prevT.exp - 1),
        x(t.sig - 1),
        x(prevT.sig + prevT.exp - 2, 0))
      val prevOK = !isBox(x, t) || helper(unswizzled, prevT)
      val curOK = !t.isNaN(x) || x(t.sig + t.exp - 4) === x(t.sig - 2, prevT.recodedWidth - 1).andR
      prevOK && curOK
    }
    helper(x, maxType)
  }

  // generate a NaN box from an FU result
  def box(x: UInt, t: FType): UInt = {
    if (t == maxType) {
      x
    } else {
      val nt = floatTypes(typeTag(t) + 1)
      val bigger = box(UInt((BigInt(1) << nt.recodedWidth)-1), nt, x, t)
      bigger | UInt((BigInt(1) << maxType.recodedWidth) - (BigInt(1) << nt.recodedWidth))
    }
  }

  // generate a NaN box from an FU result
  def box(x: UInt, tag: UInt): UInt = {
    val opts = floatTypes.map(t => box(x, t))
    opts(tag)
  }

  // zap bits that hardfloat thinks are don't-cares, but we do care about
  def sanitizeNaN(x: UInt, t: FType): UInt = {
    if (typeTag(t) == 0) {
      x
    } else {
      val maskedNaN = x & ~UInt((BigInt(1) << (t.sig-1)) | (BigInt(1) << (t.sig+t.exp-4)), t.recodedWidth)
      Mux(t.isNaN(x), maskedNaN, x)
    }
  }

  // implement NaN boxing and recoding for FL*/fmv.*.x
  def recode(x: UInt, tag: UInt): UInt = {
    def helper(x: UInt, t: FType): UInt = {
      if (typeTag(t) == 0) {
        t.recode(x)
      } else {
        val prevT = prevType(t)
        box(t.recode(x), t, helper(x, prevT), prevT)
      }
    }

    // fill MSBs of subword loads to emulate a wider load of a NaN-boxed value
    val boxes = floatTypes.map(t => UInt((BigInt(1) << maxType.ieeeWidth) - (BigInt(1) << t.ieeeWidth)))
    helper(boxes(tag) | x, maxType)
  }

  def recode(x: UInt, xtag: UInt, toType: Option[FType]): UInt = {
    val outType = toType.getOrElse(maxType)
    def helper(x: UInt, t: FType): UInt = {
      if (typeTag(t) == 0) {
        t.recode(x)
      } else {
        val prevT = prevType(t)
        box(t.recode(x), t, helper(x, prevT), prevT)
      }
    }

    // fill MSBs of subword loads to emulate a wider load of a NaN-boxed value
    val boxes = floatTypes.filter(t => t.ieeeWidth <= outType.ieeeWidth)
                          .map(t => UInt((BigInt(1) << outType.ieeeWidth) - (BigInt(1) << t.ieeeWidth)))
    helper(boxes(xtag) | x, outType)
  }

  // implement NaN unboxing and un-recoding for FS*/fmv.x.*
  def ieee(x: UInt, t: FType = maxType): UInt = {
    if (typeTag(t) == 0) {
      t.ieee(x)
    } else {
      val unrecoded = t.ieee(x)
      val prevT = prevType(t)
      val prevRecoded = Cat(
        x(prevT.recodedWidth-2),
        x(t.sig-1),
        x(prevT.recodedWidth-3, 0))
      val prevUnrecoded = ieee(prevRecoded, prevT)
      Cat(unrecoded >> prevT.ieeeWidth, Mux(t.isNaN(x), prevUnrecoded, unrecoded(prevT.ieeeWidth-1, 0)))
    }
  }
}

abstract class FPUModule(implicit val p: Parameters) extends Module with HasCoreParameters with HasFPUParameters

class FPToInt(vector: Boolean = false)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  class Output extends Bundle {
    val in = new FPInput
    val lt = Bool()
    val store = Bits(width = fLen)
    val toint = Bits(width = xLen)
    val exc = Bits(width = FPConstants.FLAGS_SZ)
    override def cloneType = new Output().asInstanceOf[this.type]
  }
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new Output)
  }

  val in = RegEnable(io.in.bits, io.in.valid)
  val valid = Reg(next=io.in.valid)

  val dcmp = Module(new hardfloat.CompareRecFN(maxExpWidth, maxSigWidth))
  dcmp.io.a := in.in1
  dcmp.io.b := in.in2
  dcmp.io.signaling := !in.rm(1)

  val tag = in.typeTagOut
  val store = (floatTypes.map(t => if (t == FType.H) Fill(maxType.ieeeWidth / minXLen,   ieee(in.in1)(15, 0).sextTo(minXLen))
                                   else              Fill(maxType.ieeeWidth / t.ieeeWidth, ieee(in.in1)(t.ieeeWidth - 1, 0))): Seq[UInt])(tag)
  val toint = Wire(init = store)
  val intType = Wire(init = in.fmt(0))
  io.out.bits.store := store
  io.out.bits.toint := ((0 until nIntTypes).map(i => toint((minXLen << i) - 1, 0).sextTo(xLen)): Seq[UInt])(intType)
  io.out.bits.exc := Bits(0)

  when (in.rm(0)) {
    val classify_out = (floatTypes.map(t => t.classify(maxType.unsafeConvert(in.in1, t))): Seq[UInt])(tag)
    toint := classify_out | (store >> minXLen << minXLen)
    intType := 0
  }

  when (in.wflags) { // feq/flt/fle, fcvt
    toint := (~in.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR | (store >> minXLen << minXLen)
    io.out.bits.exc := dcmp.io.exceptionFlags
    intType := 0

    when (!in.ren2) { // fcvt
      val cvtType = in.typ.extract(log2Ceil(nIntTypes), 1)
      val excSign = in.in1(maxExpWidth + maxSigWidth) && !maxType.isNaN(in.in1)
      intType := cvtType
      val conv = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, xLen))
      conv.io.in := in.in1
      conv.io.roundingMode := in.rm
      conv.io.signedOut := ~in.typ(0)
      toint := conv.io.out
      io.out.bits.exc := Cat(conv.io.intExceptionFlags(2, 1).orR, UInt(0, 3), conv.io.intExceptionFlags(0))

      val conv32 = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, 32))
      conv32.io.in := in.in1
      conv32.io.roundingMode := in.rm
      conv32.io.signedOut    := ~in.typ(0)
      when(cvtType === 0.U) {
        val excOut  = Cat(conv.io.signedOut === excSign, Fill(31, !excSign))
        val invalid = conv.io.intExceptionFlags(2) || conv32.io.intExceptionFlags(1)
        when(invalid) { toint := Cat(conv.io.out >> 32, excOut) }
        io.out.bits.exc := Cat(invalid, UInt(0, 3), !invalid && conv.io.intExceptionFlags(0))
      }

      val conv16 = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, 16))
      conv16.io.in := in.in1
      conv16.io.roundingMode := in.rm
      conv16.io.signedOut    := ~in.typ(0)
      when(cvtType === 0.U && tag === H && !in.wen) {
        val excOut  = Cat(conv.io.signedOut === excSign, Fill(15, !excSign))
        val invalid = conv.io.intExceptionFlags(2) || conv32.io.intExceptionFlags(1) || conv16.io.intExceptionFlags(1)
        when(invalid) { toint := Mux(!conv16.io.intExceptionFlags(1) || in.typeTagIn(0), conv.io.out >> 48, Cat(conv.io.out >> 32, excOut)) }
        io.out.bits.exc := Cat(invalid, UInt(0, 3), !invalid && conv.io.intExceptionFlags(0))
      }

      if(vector) {
        val conv8 = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, 8))
        conv8.io.in := in.in1
        conv8.io.roundingMode := in.rm
        conv8.io.signedOut    := ~in.typ(0)
        when(cvtType === 0.U && tag === H && in.wen) {
          toint := conv8.io.out
          io.out.bits.exc := Cat(conv8.io.intExceptionFlags(2, 1).orR, 0.U(3.W),  conv8.io.intExceptionFlags(0))
        }
      }

      /*
      for (i <- 0 until nIntTypes-1) {
        val w = minXLen << i
        when (cvtType === i) {
          val narrow = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, w))
          narrow.io.in := in.in1
          narrow.io.roundingMode := in.rm
          narrow.io.signedOut := ~in.typ(0)
		   
		      val cvtint16 = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, 16))
          cvtint16.io.in := in.in1
          cvtint16.io.roundingMode := in.rm
          cvtint16.io.signedOut := ~in.typ(0)
 
		      val excSign = in.in1(maxExpWidth + maxSigWidth) && !maxType.isNaN(in.in1)
          val excOut = Cat(conv.io.signedOut === excSign, Mux(tag === H, Fill(15, !excSign), Fill(w-1, !excSign)))
          val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1) || (tag === H) && cvtint16.io.intExceptionFlags(1)
          when (invalid) { toint := Mux(tag === H && (!cvtint16.io.intExceptionFlags(1) || in.typeTagIn(0)), conv.io.out >> 48, Cat(conv.io.out >> w, excOut)) }
          io.out.bits.exc := Cat(invalid, UInt(0, 3), !invalid && conv.io.intExceptionFlags(0))
        }
      }*/
    }
  }

  io.out.valid := valid
  io.out.bits.lt := dcmp.io.lt || (dcmp.io.a.asSInt < 0.S && dcmp.io.b.asSInt >= 0.S)
  io.out.bits.in := in
}

class IntToFP(
  val latency: Int,
  val supportD: Boolean = true,
  val supportS: Boolean = false,
  val vector: Boolean = false
)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = new Bundle {
    val in = Valid(new IntToFPInput).flip
    val out = Valid(new FPResult)
  }

  val in = Pipe(io.in)
  val tag = in.bits.typeTagIn

  val mux = Wire(new FPResult)
  mux.exc := Bits(0)
  mux.data := recode(in.bits.in1, tag)

  val intValue = {
    val res = Wire(init = in.bits.in1.asSInt)
    for (i <- 0 until nIntTypes-1) {
      val smallInt = in.bits.in1((minXLen << i) - 1, 0)
      when (in.bits.typ.extract(log2Ceil(nIntTypes), 1) === i) {
        res := Mux(in.bits.typ(0), smallInt.zext, smallInt.asSInt)
      }
    }
    res.asUInt
  }

  var supportTypes: List[FType] = null
  if(supportD) {
    supportTypes = FType.all
  } else if(supportS) {
    supportTypes = FType.all.take(2)
  } else {
    supportTypes = FType.all.take(1)
  }

  when (in.bits.wflags) { // fcvt
    // could be improved for RVD/RVQ with a single variable-position rounding
    // unit, rather than N fixed-position ones
    val i2fResults = for (t <- supportTypes) yield {
      val i2f = Module(new hardfloat.INToRecFN(xLen, t.exp, t.sig))
      i2f.io.signedIn := ~in.bits.typ(0)
      i2f.io.in := intValue
      i2f.io.roundingMode := in.bits.rm
      i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
      (sanitizeNaN(i2f.io.out, t), i2f.io.exceptionFlags)
    }

    val (data, exc) = i2fResults.unzip
    val dataPadded = data.init.map(d => Cat(data.last >> d.getWidth, d)) :+ data.last
    mux.data := dataPadded(tag)
    mux.exc := exc(tag)
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP(val latency: Int)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
    val lt = Bool(INPUT) // from FPToInt
  }

  val in = Pipe(io.in)

  val signNum = Mux(in.bits.rm(1), in.bits.in1 ^ in.bits.in2, Mux(in.bits.rm(0), ~in.bits.in2, in.bits.in2))
  val fsgnj = Cat(signNum(fLen), in.bits.in1(fLen-1, 0))

  val fsgnjMux = Wire(new FPResult)
  fsgnjMux.exc := UInt(0)
  fsgnjMux.data := fsgnj

  when (in.bits.wflags) { // fmin/fmax
    val isnan1 = maxType.isNaN(in.bits.in1)
    val isnan2 = maxType.isNaN(in.bits.in2)
    val isInvalid = maxType.isSNaN(in.bits.in1) || maxType.isSNaN(in.bits.in2)
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || in.bits.rm(0) =/= io.lt && !isnan1
    fsgnjMux.exc := isInvalid << 4
    fsgnjMux.data := Mux(isNaNOut, maxType.qNaN, Mux(isLHS, in.bits.in1, in.bits.in2))
  }

  val inTag = in.bits.typeTagIn
  val outTag = in.bits.typeTagOut
  val mux = Wire(init = fsgnjMux)
  for (t <- floatTypes.init) {
    when (outTag === typeTag(t)) {
      mux.data := Cat(fsgnjMux.data >> t.recodedWidth, maxType.unsafeConvert(fsgnjMux.data, t))
    }
  }

  when (in.bits.wflags && !in.bits.ren2) { // fcvt
    if (floatTypes.size > 1) {
      // widening conversions simply canonicalize NaN operands
      val widened = Mux(maxType.isNaN(in.bits.in1), maxType.qNaN, in.bits.in1)
      fsgnjMux.data := widened
      fsgnjMux.exc := maxType.isSNaN(in.bits.in1) << 4

      // narrowing conversions require rounding (for RVQ, this could be
      // optimized to use a single variable-position rounding unit, rather
      // than two fixed-position ones)
      for (outType <- floatTypes.init) when (outTag === typeTag(outType) && (typeTag(outType) == 0 || outTag < inTag)) {
        val narrower = Module(new hardfloat.RecFNToRecFN(maxType.exp, maxType.sig, outType.exp, outType.sig))
        narrower.io.in := in.bits.in1
        narrower.io.roundingMode := in.bits.rm
        narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
        val narrowed = sanitizeNaN(narrower.io.out, outType)
        mux.data := Cat(fsgnjMux.data >> narrowed.getWidth, narrowed)
        mux.exc := narrower.io.exceptionFlags
      }
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class MulAddRecFNPipe(latency: Int, expWidth: Int, sigWidth: Int) extends Module
{
    require(latency<=2)

    val io = new Bundle {
        val validin = Bool(INPUT)
        val op = Bits(INPUT, 2)
        val a = Bits(INPUT, expWidth + sigWidth + 1)
        val b = Bits(INPUT, expWidth + sigWidth + 1)
        val c = Bits(INPUT, expWidth + sigWidth + 1)
        val roundingMode   = UInt(INPUT, 3)
        val detectTininess = UInt(INPUT, 1)
        val out = Bits(OUTPUT, expWidth + sigWidth + 1)
        val exceptionFlags = Bits(OUTPUT, 5)
        val validout = Bool(OUTPUT)
    }

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val mulAddRecFNToRaw_preMul = Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
    val mulAddRecFNToRaw_postMul = Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

    mulAddRecFNToRaw_preMul.io.op := io.op
    mulAddRecFNToRaw_preMul.io.a  := io.a
    mulAddRecFNToRaw_preMul.io.b  := io.b
    mulAddRecFNToRaw_preMul.io.c  := io.c

    val mulAddResult =
        (mulAddRecFNToRaw_preMul.io.mulAddA *
             mulAddRecFNToRaw_preMul.io.mulAddB) +&
            mulAddRecFNToRaw_preMul.io.mulAddC

    val valid_stage0 = Wire(Bool())
    val roundingMode_stage0 = Wire(UInt(width=3))
    val detectTininess_stage0 = Wire(UInt(width=1))

    val postmul_regs = if(latency>0) 1 else 0
    mulAddRecFNToRaw_postMul.io.fromPreMul   := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    roundingMode_stage0                      := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    detectTininess_stage0                    := Pipe(io.validin, io.detectTininess, postmul_regs).bits
    valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val roundRawFNToRecFN = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))

    val round_regs = if(latency==2) 1 else 0
    roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    roundRawFNToRecFN.io.in                 := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
    roundRawFNToRecFN.io.roundingMode       := Pipe(valid_stage0, roundingMode_stage0, round_regs).bits
    roundRawFNToRecFN.io.detectTininess     := Pipe(valid_stage0, detectTininess_stage0, round_regs).bits
    io.validout                             := Pipe(valid_stage0, false.B, round_regs).valid

    roundRawFNToRecFN.io.infiniteExc := Bool(false)

    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}

class FPUFMAPipe(val latency: Int, val t: FType)
                (implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed {
  require(latency>0)

  val io = new Bundle {
    val in = Valid(new FPInput).flip
    val out = Valid(new FPResult)
  }

  val valid = Reg(next=io.in.valid)
  val in = Reg(new FPInput)
  when (io.in.valid) {
    val one = UInt(1) << (t.sig + t.exp - 1)
    val zero = (io.in.bits.in1 ^ io.in.bits.in2) & (UInt(1) << (t.sig + t.exp))
    val cmd_fma = io.in.bits.ren3
    val cmd_addsub = io.in.bits.swap23
    in := io.in.bits
    when (cmd_addsub) { in.in2 := one }
    when (!(cmd_fma || cmd_addsub)) { in.in3 := zero }
  }

  val fma = Module(new MulAddRecFNPipe((latency-1) min 2, t.exp, t.sig))
  fma.io.validin := valid
  fma.io.op := in.fmaCmd
  fma.io.roundingMode := in.rm
  fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  fma.io.a := in.in1
  fma.io.b := in.in2
  fma.io.c := in.in3

  val res = Wire(new FPResult)
  res.data := sanitizeNaN(fma.io.out, t)
  res.exc := fma.io.exceptionFlags

  io.out := Pipe(fma.io.validout, res, (latency-3) max 0)
}

@chiselName
class FPU(cfg: FPUParams)(implicit p: Parameters) extends FPUModule()(p) {
  val io = new FPUIO

  val useClockGating = coreParams match {
    case r: RocketCoreParams => r.clockGate
    case _ => false
  }
  val clock_en_reg = Reg(Bool())
  val clock_en = clock_en_reg || io.cp_req.valid
  val gated_clock =
    if (!useClockGating) clock
    else ClockGate(clock, clock_en, "fpu_clock_gate")

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.inst
  val id_ctrl = fp_decoder.io.sigs

  val ex_reg_valid = Reg(next=io.valid, init=Bool(false))
  val ex_reg_inst = RegEnable(io.inst, io.valid)
  val ex_reg_ctrl = RegEnable(id_ctrl, io.valid)
  val ex_ra = List.fill(3)(Reg(UInt()))

  // load response
  val load_wb = Reg(next=io.dmem_resp_val)
  val load_wb_typeTag = RegEnable(io.dmem_resp_type(1,0) - typeTagWbOffset, io.dmem_resp_val)
  val load_wb_data = RegEnable(io.dmem_resp_data, io.dmem_resp_val)
  val load_wb_tag = RegEnable(io.dmem_resp_tag, io.dmem_resp_val)

  @chiselName class FPUImpl extends NoChiselNamePrefix { // entering gated-clock domain

  val req_valid = ex_reg_valid || io.cp_req.valid
  val ex_cp_valid = io.cp_req.fire()
  val mem_cp_valid = Reg(next=ex_cp_valid, init=Bool(false))
  val wb_cp_valid = Reg(next=mem_cp_valid, init=Bool(false))
  val mem_reg_valid = RegInit(false.B)
  val killm = (io.killm || io.nack_mem) && !mem_cp_valid
  // Kill X-stage instruction if M-stage is killed.  This prevents it from
  // speculatively being sent to the div-sqrt unit, which can cause priority
  // inversion for two back-to-back divides, the first of which is killed.
  val killx = io.killx || mem_reg_valid && killm
  mem_reg_valid := ex_reg_valid && !killx || ex_cp_valid
  val mem_reg_inst = RegEnable(ex_reg_inst, ex_reg_valid)
  val wb_reg_valid = Reg(next=mem_reg_valid && (!killm || mem_cp_valid), init=Bool(false))

  val cp_ctrl = Wire(new FPUCtrlSigs)
  cp_ctrl <> io.cp_req.bits
  io.cp_resp.valid := Bool(false)
  io.cp_resp.bits.data := UInt(0)

  val ex_ctrl = Mux(ex_cp_valid, cp_ctrl, ex_reg_ctrl)
  val mem_ctrl = RegEnable(ex_ctrl, req_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // CoreMonitorBundle to monitor fp register file writes
  val frfWriteBundle = Seq.fill(2)(WireInit(new CoreMonitorBundle(xLen, fLen), DontCare))
  frfWriteBundle.foreach { i =>
    i.clock := clock
    i.reset := reset
    i.hartid := io.hartid
    i.timer := io.time(31,0)
    i.valid := false.B
    i.wrenx := false.B
    i.wrenf := false.B
    i.excpt := false.B
  }

  // regfile
  val regfile = Mem(32, Bits(width = fLen+1))
  when (load_wb) {
    val wdata = recode(load_wb_data, load_wb_typeTag)
    regfile(load_wb_tag) := wdata
    assert(consistent(wdata))
    if (enableCommitLog)
      printf("f%d p%d 0x%x\n", load_wb_tag, load_wb_tag + 32, load_wb_data)
    frfWriteBundle(0).wrdst := load_wb_tag
    frfWriteBundle(0).wrenf := true.B
    frfWriteBundle(0).wrdata := ieee(wdata)
  }

  val ex_rs = ex_ra.map(a => regfile(a))
  when (io.valid) {
    when (id_ctrl.ren1) {
      when (!id_ctrl.swap12) { ex_ra(0) := io.inst(19,15) }
      when (id_ctrl.swap12) { ex_ra(1) := io.inst(19,15) }
    }
    when (id_ctrl.ren2) {
      when (id_ctrl.swap12) { ex_ra(0) := io.inst(24,20) }
      when (id_ctrl.swap23) { ex_ra(2) := io.inst(24,20) }
      when (!id_ctrl.swap12 && !id_ctrl.swap23) { ex_ra(1) := io.inst(24,20) }
    }
    when (id_ctrl.ren3) { ex_ra(2) := io.inst(31,27) }
  }
  val ex_rm = Mux(ex_reg_inst(14,12) === Bits(7), io.fcsr_rm, ex_reg_inst(14,12))

  def fuInput(minT: Option[FType]): FPInput = {
    val req = Wire(new FPInput)
    val tag = ex_ctrl.typeTagIn
    req := ex_ctrl
    req.rm := ex_rm
    req.in1 := unbox(ex_rs(0), tag, minT)
    req.in2 := unbox(ex_rs(1), tag, minT)
    req.in3 := unbox(ex_rs(2), tag, minT)
    req.typ := ex_reg_inst(21,20)
    req.fmt := ex_reg_inst(26,25)
    req.fmaCmd := ex_reg_inst(3,2) | (!ex_ctrl.ren3 && ex_reg_inst(27))
    when (ex_cp_valid) {
      req := io.cp_req.bits
      when (io.cp_req.bits.swap23) {
        req.in2 := io.cp_req.bits.in3
        req.in3 := io.cp_req.bits.in2
      }
    }
    req
  }

  val sfma = Module(new FPUFMAPipe(cfg.sfmaLatency, FType.S))
  sfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === S
  sfma.io.in.bits := fuInput(Some(sfma.t))

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := req_valid && (ex_ctrl.toint || ex_ctrl.div || ex_ctrl.sqrt || (ex_ctrl.fastpipe && ex_ctrl.wflags))
  fpiu.io.in.bits := fuInput(None)
  io.store_data := fpiu.io.out.bits.store
  io.toint_data := fpiu.io.out.bits.toint
  when(fpiu.io.out.valid && mem_cp_valid && mem_ctrl.toint){
    io.cp_resp.bits.data := fpiu.io.out.bits.toint
    io.cp_resp.valid := Bool(true)
  }

  val ifpu = Module(new IntToFP(2))
  ifpu.io.in.valid := req_valid && ex_ctrl.fromint
  ifpu.io.in.bits := fpiu.io.in.bits
  ifpu.io.in.bits.in1 := Mux(ex_cp_valid, io.cp_req.bits.in1, io.fromint_data)

  val fpmu = Module(new FPToFP(2))
  fpmu.io.in.valid := req_valid && ex_ctrl.fastpipe
  fpmu.io.in.bits := fpiu.io.in.bits
  fpmu.io.lt := fpiu.io.out.bits.lt

  val divSqrt_wen = Wire(init = false.B)
  val divSqrt_inFlight = Wire(init = false.B)
  val divSqrt_waddr = Reg(UInt(width = 5))
  val divSqrt_typeTag = Wire(UInt(width = log2Up(floatTypes.size)))
  val divSqrt_wdata = Wire(UInt(width = fLen+1))
  val divSqrt_flags = Wire(UInt(width = FPConstants.FLAGS_SZ))

  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, res: FPResult)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === S, sfma.io.out.bits)) ++
    (fLen > 32).option({
          val dfma = Module(new FPUFMAPipe(cfg.dfmaLatency, FType.D))
          dfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === D
          dfma.io.in.bits := fuInput(Some(dfma.t))
          Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === D, dfma.io.out.bits)
        }) ++
    (minFLen == 16).option({
          val hfma = Module(new FPUFMAPipe(cfg.sfmaLatency, FType.H))
          hfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === H
          hfma.io.in.bits := fuInput(Some(hfma.t))
          Pipe(hfma, hfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === H, hfma.io.out.bits)
        })
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  class WBInfo extends Bundle {
    val rd = UInt(width = 5)
    val typeTag = UInt(width = log2Up(floatTypes.size))
    val cp = Bool()
    val pipeid = UInt(width = log2Ceil(pipes.size))
    override def cloneType: this.type = new WBInfo().asInstanceOf[this.type]
  }

  val wen = Reg(init=Bits(0, maxLatency-1))
  val wbInfo = Reg(Vec(maxLatency-1, new WBInfo))
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint)
  val write_port_busy = RegEnable(mem_wen && (memLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, req_valid)
  ccover(mem_reg_valid && write_port_busy, "WB_STRUCTURAL", "structural hazard on writeback")

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { wbInfo(i) := wbInfo(i+1) }
  }
  wen := wen >> 1
  when (mem_wen) {
    when (!killm) {
      wen := wen >> 1 | memLatencyMask
    }
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && memLatencyMask(i)) {
        wbInfo(i).cp := mem_cp_valid
        wbInfo(i).typeTag := mem_ctrl.typeTagOut
        wbInfo(i).pipeid := pipeid(mem_ctrl)
        wbInfo(i).rd := mem_reg_inst(11,7)
      }
    }
  }

  val waddr = Mux(divSqrt_wen, divSqrt_waddr, wbInfo(0).rd)
  val wtypeTag = Mux(divSqrt_wen, divSqrt_typeTag, wbInfo(0).typeTag)
  val wdata = box(Mux(divSqrt_wen, divSqrt_wdata, (pipes.map(_.res.data): Seq[UInt])(wbInfo(0).pipeid)), wtypeTag)
  val wexc = (pipes.map(_.res.exc): Seq[UInt])(wbInfo(0).pipeid)
  when ((!wbInfo(0).cp && wen(0)) || divSqrt_wen) {
    assert(consistent(wdata))
    regfile(waddr) := wdata
    if (enableCommitLog) {
      printf("f%d p%d 0x%x\n", waddr, waddr + 32, ieee(wdata))
    }
    frfWriteBundle(1).wrdst := waddr
    frfWriteBundle(1).wrenf := true.B
    frfWriteBundle(1).wrdata := ieee(wdata)
  }
  when (wbInfo(0).cp && wen(0)) {
    io.cp_resp.bits.data := wdata
    io.cp_resp.valid := Bool(true)
  }
  io.cp_req.ready := !ex_reg_valid

  val wb_toint_valid = wb_reg_valid && wb_ctrl.toint
  val wb_toint_exc = RegEnable(fpiu.io.out.bits.exc, mem_ctrl.toint)
  io.fcsr_flags.valid := wb_toint_valid || divSqrt_wen || wen(0)
  io.fcsr_flags.bits :=
    Mux(wb_toint_valid, wb_toint_exc, UInt(0)) |
    Mux(divSqrt_wen, divSqrt_flags, UInt(0)) |
    Mux(wen(0), wexc, UInt(0))

  val divSqrt_write_port_busy = (mem_ctrl.div || mem_ctrl.sqrt) && wen.orR
  io.fcsr_rdy := !(ex_reg_valid && ex_ctrl.wflags || mem_reg_valid && mem_ctrl.wflags || wb_reg_valid && wb_ctrl.toint || wen.orR || divSqrt_inFlight)
  io.nack_mem := write_port_busy || divSqrt_write_port_busy || divSqrt_inFlight
  io.dec <> fp_decoder.io.sigs
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(Bool(false))(_||_)
  io.sboard_set := wb_reg_valid && !wb_cp_valid && Reg(next=useScoreboard(_._1.cond(mem_ctrl)) || mem_ctrl.div || mem_ctrl.sqrt)
  io.sboard_clr := !wb_cp_valid && (divSqrt_wen || (wen(0) && useScoreboard(x => wbInfo(0).pipeid === UInt(x._2))))
  io.sboard_clra := waddr
  ccover(io.sboard_clr && load_wb, "DUAL_WRITEBACK", "load and FMA writeback on same cycle")
  // we don't currently support round-max-magnitude (rm=4)
  io.illegal_rm := io.inst(14,12).isOneOf(5, 6) || io.inst(14,12) === 7 && io.fcsr_rm >= 5

  if (cfg.divSqrt) {
    val divSqrt_killed = Reg(Bool())
    ccover(divSqrt_inFlight && divSqrt_killed, "DIV_KILLED", "divide killed after issued to divider")
    ccover(divSqrt_inFlight && mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt), "DIV_BUSY", "divider structural hazard")
    ccover(mem_reg_valid && divSqrt_write_port_busy, "DIV_WB_STRUCTURAL", "structural hazard on division writeback")

    for (t <- floatTypes) {
      val tag = mem_ctrl.typeTagOut
      val divSqrt = Module(new hardfloat.DivSqrtRecFN_small(t.exp, t.sig, 0))
      divSqrt.io.inValid := mem_reg_valid && tag === typeTag(t) && (mem_ctrl.div || mem_ctrl.sqrt) && !divSqrt_inFlight
      divSqrt.io.sqrtOp := mem_ctrl.sqrt
      divSqrt.io.a := maxType.unsafeConvert(fpiu.io.out.bits.in.in1, t)
      divSqrt.io.b := maxType.unsafeConvert(fpiu.io.out.bits.in.in2, t)
      divSqrt.io.roundingMode := fpiu.io.out.bits.in.rm
      divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding

      when (!divSqrt.io.inReady) { divSqrt_inFlight := true } // only 1 in flight

      when (divSqrt.io.inValid && divSqrt.io.inReady) {
        divSqrt_killed := killm
        divSqrt_waddr := mem_reg_inst(11,7)
      }

      when (divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt) {
        divSqrt_wen := !divSqrt_killed
        divSqrt_wdata := sanitizeNaN(divSqrt.io.out, t)
        divSqrt_flags := divSqrt.io.exceptionFlags
        divSqrt_typeTag := typeTag(t)
      }
    }
  } else {
    when (id_ctrl.div || id_ctrl.sqrt) { io.illegal_rm := true }
  }

  // gate the clock
  clock_en_reg := !useClockGating ||
    io.keep_clock_enabled || // chicken bit
    io.valid || // ID stage
    req_valid || // EX stage
    mem_reg_valid || mem_cp_valid || // MEM stage
    wb_reg_valid || wb_cp_valid || // WB stage
    wen.orR || divSqrt_inFlight || // post-WB stage
    io.dmem_resp_val // load writeback

  } // leaving gated-clock domain
  val fpuImpl = withClock (gated_clock) { new FPUImpl }

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    cover(cond, s"FPU_$label", "Core;;" + desc)
}

class FR7(
  val latency: Int, 
  val supportD: Boolean = false,
  val supportS: Boolean = false
)(implicit p: Parameters) extends FPUModule()(p) with ShouldBeRetimed{
  val io = new Bundle {
    val in     = Valid(new FPInput).flip
    val out    = Valid(new FPResult)
    val active = Input(Bool())
  }

  // address look-up table
  val frsqrt7_seq = Seq(
     52.U,  51.U,  50.U,  48.U,  47.U,  46.U,  44.U,  43.U,
     42.U,  41.U,  40.U,  39.U,  38.U,  36.U,  35.U,  34.U,
     33.U,  32.U,  31.U,  30.U,  30.U,  29.U,  28.U,  27.U,
     26.U,  25.U,  24.U,  23.U,  23.U,  22.U,  21.U,  20.U,
     19.U,  19.U,  18.U,  17.U,  16.U,  16.U,  15.U,  14.U,
     14.U,  13.U,  12.U,  12.U,  11.U,  10.U,  10.U,   9.U,
      9.U,   8.U,   7.U,   7.U,   6.U,   6.U,   5.U,   4.U,
      4.U,   3.U,   3.U,   2.U,   2.U,   1.U,   1.U,   0.U,
    127.U, 125.U, 123.U, 121.U, 119.U, 118.U, 116.U, 114.U,
    113.U, 111.U, 109.U, 108.U, 106.U, 105.U, 103.U, 102.U,
    100.U,  99.U,  97.U,  96.U,  95.U,  93.U,  92.U,  91.U,
     90.U,  88.U,  87.U,  86.U,  85.U,  84.U,  83.U,  82.U,
     80.U,  79.U,  78.U,  77.U,  76.U,  75.U,  74.U,  73.U,
     72.U,  71.U,  70.U,  70.U,  69.U,  68.U,  67.U,  66.U,
     65.U,  64.U,  63.U,  63.U,  62.U,  61.U,  60.U,  59.U,
     59.U,  58.U,  57.U,  56.U,  56.U,  55.U,  54.U,  53.U
  )
  val frsqrt7_table = VecInit(frsqrt7_seq)

  val frec7_seq = Seq(
    127.U, 125.U, 123.U, 121.U, 119.U, 117.U, 116.U, 114.U,
    112.U, 110.U, 109.U, 107.U, 105.U, 104.U, 102.U, 100.U,
     99.U,  97.U,  96.U,  94.U,  93.U,  91.U,  90.U,  88.U,
     87.U,  85.U,  84.U,  83.U,  81.U,  80.U,  79.U,  77.U,
     76.U,  75.U,  74.U,  72.U,  71.U,  70.U,  69.U,  68.U,
     66.U,  65.U,  64.U,  63.U,  62.U,  61.U,  60.U,  59.U,
     58.U,  57.U,  56.U,  55.U,  54.U,  53.U,  52.U,  51.U,
     50.U,  49.U,  48.U,  47.U,  46.U,  45.U,  44.U,  43.U,
     42.U,  41.U,  40.U,  40.U,  39.U,  38.U,  37.U,  36.U,
     35.U,  35.U,  34.U,  33.U,  32.U,  31.U,  31.U,  30.U,
     29.U,  28.U,  28.U,  27.U,  26.U,  25.U,  25.U,  24.U,
     23.U,  23.U,  22.U,  21.U,  21.U,  20.U,  19.U,  19.U,
     18.U,  17.U,  17.U,  16.U,  15.U,  15.U,  14.U,  14.U,
     13.U,  12.U,  12.U,  11.U,  11.U,  10.U,   9.U,   9.U,
      8.U,   8.U,   7.U,   7.U,   6.U,   5.U,   5.U,   4.U,
      4.U,   3.U,   3.U,   2.U,   2.U,   1.U,   1.U,   0.U
  )
  val frec7_table = VecInit(frec7_seq)

  val in = Pipe(io.in)
  val orig_data = Pipe(io.in.valid, io.in.bits.in3, latency)
  val v_active  = Pipe(io.in.valid, io.active, latency)

  // classify (recoded) FP input
  // fclass[0] == 1, negative infinite
  // fclass[1] == 1, negative normal
  // fclass[2] == 1, negative subnormal
  // fclass[3] == 1, negative zero
  // fclass[4] == 1, positive zero
  // fclass[5] == 1, positive subnormal
  // fclass[6] == 1, positive normal
  // fclass[7] == 1, positive infinite
  // fclass[8] == 1, signaling NaN
  // fclass[9] == 1, quiet NaN
  val tag    = in.bits.typeTagOut
  var supportTypes: List[FType] = null
  if(supportD) {
    supportTypes = FType.all
  } else if(supportS) {
    supportTypes = FType.all.take(2)
  } else {
    supportTypes = FType.all.take(1)
  }
  val fclass = (supportTypes.map(t => t.classify(maxType.unsafeConvert(in.bits.in1, t))): Seq[UInt])(tag)

  // frsqrt7, bias = 3 * exp_bias - 1
  val bias_frsqrt = Mux(tag === D, 3068.S, Mux(tag === S, 380.S, 44.S))
  // frec7, bias = 2 * exp_bias - 1
  val bias_frec   = Mux(tag === D, 2045.S, Mux(tag === S, 253.S, 29.S))

  // extract sign, exp, sig(nificand) from ieee FP input
  val in_ieee = in.bits.in2
  val sign = (supportTypes.map(t => in_ieee(t.exp+t.sig-1)) : Seq[UInt])(tag)
  val exp  = (supportTypes.map(t => Cat(Wire(Fill(maxType.exp-t.exp, 0.U(1.W))), in_ieee(t.exp+t.sig-2, t.sig-1))) : Seq[UInt])(tag)
  val sig  = (supportTypes.map(t => Cat(in_ieee(t.sig-2, 0), Wire(Fill(maxType.sig-t.sig, 0.U(1.W))))): Seq[UInt])(tag)

  // normalize inputs when subnormal
  val valid_norm  = RegNext(in.valid)
  val typ_norm    = RegNext(in.bits.typ(0))
  val frm_norm    = RegNext(in.bits.rm)
  val tag_norm    = RegNext(in.bits.typeTagOut)
  val fclass_norm = RegNext(fclass)
  val sign_norm   = RegNext(sign)
  val exp_norm    = RegInit(0.U((maxType.exp+1).W))
  val sig_norm    = RegInit(0.U(7.W))
  val lz_norm     = RegInit(0.U(log2Ceil(maxType.sig-1).W))
  val count_mask  = PopCount(Masklower(sig))
  val sht_left    = maxType.sig-count_mask
  when(fclass(2) || fclass(5)) {
    exp_norm := sht_left - 1.U
    sig_norm := (sig << sht_left)(maxType.sig-2, maxType.sig-8)
    lz_norm  := sht_left - 1.U
  } .otherwise {
    exp_norm := exp
    sig_norm := sig(maxType.sig-2, maxType.sig-8)
    lz_norm  := 0.U
  }
 

  val lut_addr_frsqrt = Cat(exp_norm(0), sig_norm(6, 1))
  val lut_addr_frec   = sig_norm

  // output logic
  val valid_out  = RegNext(valid_norm)
  val typ_out    = RegNext(typ_norm)
  val frm_out    = RegNext(frm_norm)
  val tag_out    = RegNext(tag_norm)
  val fclass_out = RegNext(fclass_norm)
  val sign_out   = RegNext(sign_norm)
  val lz_out     = RegNext(lz_norm)
  //--------------------------------------------------
  // frsqrt7
  val exp_frsqrt = RegInit(0.S((maxType.exp+1).W))
  val sig_frsqrt = RegInit(0.U(7.W))
  when(fclass_norm(2) || fclass_norm(5)) {
    exp_frsqrt := (bias_frsqrt + exp_norm.asSInt) >> 1
  } .otherwise {
    exp_frsqrt := (bias_frsqrt - exp_norm.asSInt) >> 1
  }
  sig_frsqrt := frsqrt7_table(lut_addr_frsqrt)

  val h_frsqrt = WireInit(0.U(16.W))
  var s_frsqrt = WireInit(0.U(32.W))
  var d_frsqrt = WireInit(0.U(64.W))
  // output selection based on fclass results
  when(fclass_out(5) || fclass_out(6)) {
    h_frsqrt := Cat(sign_out, exp_frsqrt( 4, 0), sig_frsqrt, Fill( 3, 0.U(1.W)))
  } .elsewhen(fclass_out(7)) {
    // zero
    h_frsqrt := Fill(16, 0.U(1.W))
  } .elsewhen(fclass_out(3) || fclass_out(4)) {
    // infinite
    h_frsqrt := Cat(sign_out, Fill( 5, 1.U(1.W)), Fill(10, 0.U(1.W)))
  } .otherwise {
    // canonical NaN
    h_frsqrt := Cat(sign_out, Fill( 6, 1.U(1.W)), Fill( 9, 0.U(1.W)))
  }
  // for single-precision
  if(supportD || supportS) {
    // output selection based on fclass results
    when(fclass_out(5) || fclass_out(6)) {
      s_frsqrt := Cat(sign_out, exp_frsqrt( 7, 0), sig_frsqrt, Fill(16, 0.U(1.W)))
    } .elsewhen(fclass_out(7)) {
      // zero
      s_frsqrt := Fill(32, 0.U(1.W))
    } .elsewhen(fclass_out(3) || fclass_out(4)) {
      // infinite
      s_frsqrt := Cat(sign_out, Fill( 8, 1.U(1.W)), Fill(23, 0.U(1.W)))
    } .otherwise {
      // canonical NaN
      s_frsqrt := Cat(sign_out, Fill( 9, 1.U(1.W)), Fill(22, 0.U(1.W)))
    }
  }
  // for double-precision
  if(supportD) {
    when(fclass_out(5) || fclass_out(6)) {
      d_frsqrt := Cat(sign_out, exp_frsqrt(10, 0), sig_frsqrt, Fill(45, 0.U(1.W)))
    } .elsewhen(fclass_out(7)) {
      // zero
      d_frsqrt := Fill(64, 0.U(1.W))
    } .elsewhen(fclass_out(3) || fclass_out(4)) {
      // infinite
      d_frsqrt := Cat(sign_out, Fill(11, 1.U(1.W)), Fill(52, 0.U(1.W)))
    } .otherwise {
      // canonical NaN
      d_frsqrt := Cat(sign_out, Fill(12, 1.U(1.W)), Fill(51, 0.U(1.W)))
    }
  }

  val frsqrt7Mux = Wire(new FPResult)
  if(supportD) {
    frsqrt7Mux.data := Mux(tag_out === H, Fill(4, h_frsqrt),
                       Mux(tag_out === S, Fill(2, s_frsqrt), d_frsqrt))
  } else if(supportS) {
    frsqrt7Mux.data := Mux(tag_out === H, Fill(4, h_frsqrt), Fill(2, s_frsqrt))
  } else {
    frsqrt7Mux.data := Fill(4, h_frsqrt)
  }
  frsqrt7Mux.exc  := Mux(fclass_out(0) || fclass_out(1) || fclass_out(2) || fclass_out(8), 16.U,
                     Mux(fclass_out(3) || fclass_out(4), 8.U, 0.U))
  
  //---------------------------------------------------
  // frec7
  val exp_frec = RegInit(0.S((maxType.exp+1).W))
  val sig_frec = RegInit(0.U(7.W))
  exp_frec := bias_frec - exp_norm.asSInt
  sig_frec := frec7_table(lut_addr_frec)

  // adjust exponents and significands
  val exp_frec_out = Mux(exp_frec === 0.S || exp_frec === -1.S, 0.S, exp_frec)
  val sig_frec_out = Mux(exp_frec === 0.S,  Cat(1.U(1.W), sig_frec, 0.U(1.W)),
                     Mux(exp_frec === -1.S, Cat(0.U(1.W), 1.U(1.W), sig_frec),
                     Cat(sig_frec, Fill(2, 0.U(1.W)))))

  val h_frec = WireInit(0.U(16.W))
  var s_frec = WireInit(0.U(32.W))
  var d_frec = WireInit(0.U(64.W))
  when(fclass_out(0) || fclass_out(7)) {
    // 0.0
    h_frec := Cat(sign_out, Fill(15, 0.U(1.W)))
  } .elsewhen(fclass_out(2) && lz_out > 1.U && (frm_out === 1.U || frm_out === 3.U)) {
    // RTZ || RUP -> greatest-mag, negative
    h_frec := Cat(1.U(1.W), Fill( 4, 1.U(1.W)), 0.U(1.W), Fill(10, 1.U(1.W)))
  } .elsewhen((fclass_out(2) && lz_out > 1.U) || fclass_out(3)) {
    // negative infinite
    h_frec := Cat(1.U(1.W), Fill( 5, 1.U(1.W)), Fill(10, 0.U(1.W)))
  } .elsewhen(fclass_out(5) && lz_out > 1.U && (frm_out === 1.U || frm_out === 2.U)) {
    // RTZ || RDN -> greatest-mag, positive
    h_frec := Cat(0.U(1.W), Fill( 4, 1.U(1.W)), 0.U(1.W), Fill(10, 1.U(1.W)))
  } .elsewhen((fclass_out(5) && lz_out > 1.U) || fclass_out(4)) {
    // positive infinite
    h_frec := Cat(0.U(1.W), Fill( 5, 1.U(1.W)), Fill(10, 0.U(1.W)))
  } .elsewhen(fclass_out(8) || fclass_out(9)) {
    // canonical NaN
    h_frec := Cat(sign_out, Fill( 6, 1.U(1.W)), Fill( 9, 0.U(1.W)))
  } .otherwise {
    h_frec := Cat(sign_out, exp_frec_out( 4, 0), sig_frec_out, Fill( 1, 0.U(1.W)))
  }
  // for single-precision
  if(supportD || supportS) {
    when(fclass_out(0) || fclass_out(7)) {
      // 0.0
      s_frec := Cat(sign_out, Fill(31, 0.U(1.W)))
    } .elsewhen(fclass_out(2) && lz_out > 1.U && (frm_out === 1.U || frm_out === 3.U)) {
      // RTZ || RUP -> greatest-mag, negative
      s_frec := Cat(1.U(1.W), Fill( 7, 1.U(1.W)), 0.U(1.W), Fill(23, 1.U(1.W)))
    } .elsewhen((fclass_out(2) && lz_out > 1.U) || fclass_out(3)) {
      // negative infinite
      s_frec := Cat(1.U(1.W), Fill( 8, 1.U(1.W)), Fill(23, 0.U(1.W)))
    } .elsewhen(fclass_out(5) && lz_out > 1.U && (frm_out === 1.U || frm_out === 2.U)) {
      // RTZ || RDN -> greatest-mag, positive
      s_frec := Cat(0.U(1.W), Fill( 7, 1.U(1.W)), 0.U(1.W), Fill(23, 1.U(1.W)))
    } .elsewhen((fclass_out(5) && lz_out > 1.U) || fclass_out(4)) {
      // positive infinite
      s_frec := Cat(0.U(1.W), Fill( 8, 1.U(1.W)), Fill(23, 0.U(1.W)))
    } .elsewhen(fclass_out(8) || fclass_out(9)) {
      // canonical NaN
      s_frec := Cat(sign_out, Fill( 9, 1.U(1.W)), Fill(22, 0.U(1.W)))
    } .otherwise {
      s_frec := Cat(sign_out, exp_frec_out( 7, 0), sig_frec_out, Fill(14, 0.U(1.W)))
    }
  }
  // for double-precision
  if(supportD) {
    when(fclass_out(0) || fclass_out(7)) {
      // 0.0
      d_frec := Cat(sign_out, Fill(63, 0.U(1.W)))
    } .elsewhen(fclass_out(2) && lz_out > 1.U && (frm_out === 1.U || frm_out === 3.U)) {
      // RTZ || RUP -> greatest-mag, negative
      d_frec := Cat(1.U(1.W), Fill(10, 1.U(1.W)), 0.U(1.W), Fill(52, 1.U(1.W)))
    } .elsewhen((fclass_out(2) && lz_out > 1.U) || fclass_out(3)) {
      // negative infinite
      d_frec := Cat(1.U(1.W), Fill(11, 1.U(1.W)), Fill(52, 0.U(1.W)))
    } .elsewhen(fclass_out(5) && lz_out > 1.U && (frm_out === 1.U || frm_out === 2.U)) {
      // RTZ || RDN -> greatest-mag, positive
      d_frec := Cat(0.U(1.W), Fill(10, 1.U(1.W)), 0.U(1.W), Fill(52, 1.U(1.W)))
    } .elsewhen((fclass_out(5) && lz_out > 1.U) || fclass_out(4)) {
      // positive infinite
      d_frec := Cat(0.U(1.W), Fill(11, 1.U(1.W)), Fill(52, 0.U(1.W)))
    } .elsewhen(fclass_out(8) || fclass_out(9)) {
      // canonical NaN
      d_frec := Cat(sign_out, Fill(12, 1.U(1.W)), Fill(51, 0.U(1.W)))
    } .otherwise {
      d_frec := Cat(sign_out, exp_frec_out(10, 0), sig_frec_out, Fill(43, 0.U(1.W)))
    }
  }

  val frec7Mux = Wire(new FPResult)
  if(supportD){
    frec7Mux.data := Mux(tag_out === H, Fill(4, h_frec),
                     Mux(tag_out === S, Fill(2, s_frec), d_frec))  
  } else if(supportS) {
    frec7Mux.data := Mux(tag_out === H, Fill(4, h_frec), Fill(2, s_frec))
  } else {
    frec7Mux.data := Fill(4, h_frec)
  }
  frec7Mux.exc  := Mux((fclass_out(2) || fclass_out(5)) && lz_out > 1.U, 5.U,
                   Mux( fclass_out(3) || fclass_out(4), 8.U, 
                   Mux( fclass_out(8), 16.U, 0.U)))

  require(latency >= 3)
  val fr7Mux = Mux(typ_out, frec7Mux, frsqrt7Mux)
  io.out <> Pipe(valid_out, fr7Mux, latency-3)
  when(!v_active.bits) {
    io.out.bits.data := orig_data.bits
    io.out.bits.exc  := 0.U
  }
}
