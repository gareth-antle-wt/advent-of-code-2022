package advent2022.day6

import scala.collection.mutable

object Day6 extends App {

  val sequence = "sbpbwwrlwrwggscsfcsshsvhshphttzctztfflddvbvcbcvbcbnbrrstsspsvpvllncccqssqdssnsmshmshhtssbzzlplffzppddmhhwnhntnjtnjtnnqggjdgjdjrjffsdfsfbsffhtthwwfssqpspllcvcdvvfvfzfbzzgpglpgpmppfsfdfbfvvfjfsjjvqqbvqbvqbvvlvglvglvljvjzvvqcvqcqjcqqpddqbqcqrqmmvnvbnnfqnncznccqjcqqjzzwlltrrmmlwlllbjlblzlddfdmmscmsspfssqggbsbsjjdqdqgqgqbqdqrqlrrltttghthrtrwrdrnrfnfvftvtsvtvwtvwttvqtvtccqtqmqggwhhhlzhhrwwbwqwbbfrrmddwhdwdnwdwbdwwsswtwnnvvdggbbtwbbwllgffqpffpgpgmpmmjqqmpmffjgfgrffdzzspzptzzdszszbsbsbsvsbbhsbsbddnhhrqqcwwblbwlwnnthtsshmshmhgmgqgjqqpccfvcvbbbnllgmlgmmbnmnlmlzlffrrrgssmcmddnpdndtntrrqdqldldbbtppvddgndnwwctwccpbpffngnsgshggphggtssgngtnthtllzflzflzfzrfftgtstppghgnnpggrdgdnnswwccljlflwwgzgcgrrhssbwbllblnlvlddsffgrrnbbmsmmmhjmjvmjvmjmqqpdqppltlflmfmssqcqsqvsvsggpglplslggqtggmsmmmgffrccrvvsdvdvfddprdprrwjrjcjjmlmrmqmggqgdqdwdsdwwwgfwfddrcdcvcmmjbbmrrhlrrwcwvcclttwbwttgffnmnjmjdmmmlqmqrrlblplccdbcbwcbcpphpqpqlqrllqwwjsjgjbjqbbmcmzmrrqtrqqmcmmnjmjbbtnnbbjdjzzvbzvzjzdjzjrzzfzfbbmnmdnnzppmbpbhbdbvvqnqqnqrnrzrhhrddbqdqpqqwgwlglplglslhhpjpdjdjgddnppmvppjddczccgsccdsccbcvczccnbnmmsnsmmrwmwbmbbcpbbwbppzggdnnnzmnzzvrvcrvcvcrvrnrzzqzmzttjhhnffqggnqqwzwfzfwzwfwcwnccwgcwgcgqcgqcggzgwgfwgfffjttjddwswqqnttqwqsqhhcmchcscbbnwnlwlrlppqnnwjnjtntrtdtbbmzzrmzrzvrzvrzzwqwqllccnffvmmfmvmzzfttnnzttrbttdvvhdhzhzhvzzfsfszsvsttrqqdlldflfjjnqjjbhhqhzhcchppzsznntdntdddqqwjqjqmmgnnhrhjhnjnznwwdpplzzfzztbzzbmzmbbgjgtjtqjqpqhqgqhhtztpzpmzmjmhhjwwjsspbssvdsvsgvssghgvglgtltgllmhmtmhttfgglwgllpvpgpspzzhnnzcnzzldzdmzmlzzwtwhhlfhhvthhjdjsdsmddgzzsjjnznjnmjjgdgtdtzdtdvtddzwzgwwqvqccwrrrvddbtdbbshbhssnvsnvssswcwjcjlltvvltvvdsdqdsswlwttzfzhzjjgjsgjgttbccsvcscllbfbzzwwwznwwtzwzjzwjjgqjqrjrdjrddzsznsznzpzszhzzspzzbzrzhhsbspsqpqqqpddhdcdldttlccjrjsrsfftfdfrrtntpnpsnnwjnwnznpzzlqqlmqlmmcddlqqzssglsglssprmnvltqhslvqmvszjtvtwqjcdngjmftnhwvjdvtwwhtnsdmvjdspnhnlmjgnmwlspcvpdmlsrnbbzlmwwrslssmcbggmfvgzsnpnlnzdqsbhcfjdccrspnzfmhbvwstvccvqqjlwhpnlrrwszjnrtdfzwrwlzwvdvbzbvltdpfwrjlslmrctwvbbvdrctgtgwtwpjjghhvdsqhplfrsjqlgsrbfgwdjlzdpdljtvjmpwqqbghndqnvjhngtpnpvzfbtchncwdqjhmzjlpdggbdcqrfjlwvczvpspljqmpgtrsvwwhqncfvfrwbnvsfjqlsjdlrqzmlqjgcpghhgzfhjcglllnhtmchrrptbzhqnfntgqbfstrvpsqsvqvcvpgjnchbvmgtgzqfrqcjrvldzghdfrvllrtfcwnsmmgdrcbjmdqgbfwmpwhfjmnbqrbvqvnmjlqtsjqvhzpdsgbjpjngmzbgnznvjqprfvwzjrfrwfdqrtlcgqqlvrzwmqwjbdvprvpwvdcrfdrcttnmnvjfrsrrmjgfjdmpdpnfsrwnprtdpvdmdwssvjrqtlcvpgrqgqqqffvvssbmghzjrzrzlcrnfjtdbvwsjzfvcrsmgqbcrdrjwdwbltffwbgjwtgtdblmlhvhlcpgdmpcztmpmgjghqpwzwtpnmnmgnqqrrtwczgmgtdbgdqtpnlbnzhshsfzsmrztffrmlsgqcprbjpqwjlqgwvctpmpshgzbzsjgqhzvsrjfwplvjbvltrlfldvsmlppcmsfrbbctggmqmjnhppstzrcfjtdgfwrrnmlvjphwtlqtjqcntjtzvgjtwvthjfbgpwhlrzdqncmggvgthmgjvrbnzbndsldnlcgtcqbqdnnbnqhvtpfnrclttfwcpnqscjbzdvbqrrsbzpdfhjllbjwsltjpmdnbrrzvhvzzqnlbglsjrnjbqffnnzmldfvtvmldfsztrnpcjgblsdhzfzmfqzlfrtslglhfvszppptjnjqcdmjcwqmfzhnqbfhslwhvtjfvcftzsphvghvtjjswpwghnfngmzddbwwqddsphvhcrwtthsjfswfqbvdsqghmrspdldfqmchnnrcdvjsclcnlsncsplchvzrwqbtvvqlqspftrwmjcbgpzbsmnfccbzgnhqsfjgmmsqsscdscfjbrmmtjbsphhlrlsgbllrptqrcgnqchzfddjwlldsbpcnzfbspfpchclqfbbtjpmtmtjthcdvwhrtqbgmgldcgcnmmhtbnqpzzcwlrscbzcqjzgztwjrnbmsnqtcllznlctzrntftspmnvhtfwbljnmrwsstvbmwclqrfpmwvjphrwddzdwtlfcvzlvqdmzhnvslfnfjhvdndlgbvvzbztpwvqzbzsbtqpqfmgqfgpzctfrqfjwmsnmlfqbgrlmncntcbshjhdcbqnvznhtcgcmmhnsbwpzbvtqbntwgflhjgmvvfhdbwfqmnfjlzdvvnpmvjrdfdnrhpbtllhbtbswwvrbwjgnqpbgnfrjtvbczbpmrcbwdlhztzssnwshjmmcqchptrtchrqncdgdtmwrlnmmwqlzqswwwvpngvwcphgnzrhpprjnbldscvwlqdjwnhjrnscdwlnhnsbwgzjtgvzdgqcjcgvrdhntszhdnjsbbrfphlmdlldjdslbjnnsfbmcnvtlczmtnhrwblnbrdptcpmsbwqptgmwzsqnmmchwnnrvrlfsrglfzzqbnzmpdtnhhbmfqvsrsdsctvhqwfgbtvhbbrsrqmrvvplrnbfnbdmrvzpgctdtglndhcqnllvvcppgfbwjrpqcbghlqdbmpzwrqpmvwddqgthlmzmdsvzdfsmgzltbsvphctzgjsmqvgjlsbgnvmgprbcsfhgrtbwtnnrsqcwfzrhlgjcwcfrjhffrvrvtnpczvwvjnnhfdgcppnnjjpttptcbmdqvgdbhdmlqgcqsrnbcrbtcgzbvgmhbnwzsgnwzbhdqqmvtpssvlvsttgnmcclqnjcgjnvtdggrcwsgbpjljgzgtllsnfvfshtbbpwrjhzvzswlfdvhbpngvgddcmhbzqcvnjhfsqpnvvsdvdtmqlqpzcgsnwlflnqprbqnwdqchjvsptbtrvtzvhrmrvznfpzmcsgnqtdvghhzwrrwvqwrztvdbjjtfchpftdcbthpfdczwchpptwzdpswvbhppdphgvpfzhprpqtnprgfmdnqrbrdlclcmhrdfrcdhwpcqhnbwmhrrgnctpvsqmphcwwvlmslszhdz"

  def hasUniqueChars(str: String, index: Int, numUnique: Int): Boolean = {
    val set = mutable.Set[Char]()
    for (x <- index until (index + numUnique)) {
      set.add(str(x))
    }
    set.size == numUnique
  }

  def findStartOfPacket(str: String): Int = {
    for (x <- 0 until (str.length - 3)) {
      if hasUniqueChars(str, x, 4) then return x
    }
    -1
  }

  def findStartOfMessage(string: String): Int = {
    for (x <- 0 until (string.length - 3)) {
      if hasUniqueChars(string, x, 14) then return x
    }
    -1
  }

  def part1() = {
    println(findStartOfPacket(sequence) + 4)
  }

  part1()

  def part2() = {
    println(findStartOfMessage(sequence) + 14)
  }

  part2()

}
