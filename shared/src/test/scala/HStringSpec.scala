
import org.scalatest.{FlatSpec, Matchers}
import io.github.hamsters.HString._
/**
  * Created by psachathamak on 10/17/2017 AD.
  */
class HStringSpec extends FlatSpec with Matchers {

  "A none element of Option[String]" should "return empty string" in {
    val optString: Option[String] = None
    optString.orEmpty shouldEqual ""
  }
}