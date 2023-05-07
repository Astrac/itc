package astrac.itc.simulator

import java.nio.ByteBuffer
import java.util.UUID

opaque type ShortUUID = String

object ShortUUID:
  extension (suuid: ShortUUID) def asString: String = suuid

  def random(): ShortUUID =
    val uuid = UUID.randomUUID()
    val bytes = ByteBuffer.wrap(uuid.toString().getBytes())
    java.lang.Long.toString(bytes.getLong(), Character.MAX_RADIX)
