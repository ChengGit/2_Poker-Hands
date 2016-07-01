/*
* Distinguish the outcome by feature pattern:
*---all possible combinations for 2 distinct value------------
* e.g. Four of a kind: Pattern aaaab (2 distinct value, most frequent appearance is 4)
* e.g. Full House:     Pattern aaabb (2 distinct value, most frequent appearance is 3)
*
*
* ---all combinations for 3 distinct value--------------------
* e.g. Two Pair:      Pattern: aabbc
* e.g. Three of a kind: Pattern: aaabc
*
*
*  ---all combination for 4 distinct value--------------------
* e.g. One pair:     Pattern:aabcd
*
*
* ---all combination for 5 distinct value--------------------
* e.g. Straight         <---|
*      Flush Straight   <---| same in pattern
*      High Card          <---|
*      Garbage            <---| same in pattern
* */
sealed trait Validatable

case class Valid(hand: List[Int], suits: List[Char]) extends Validatable

case class Invalid(reason: String) extends Validatable


object PokerApp {
	val valueDictionary = Map("A" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6,
							  "7" -> 7, "8" -> 8, "9" -> 9, "10" -> 10, "J" -> 11, "Q" -> 12, "K" -> 13)

	val languageDictionary = Map(1 ->("Ace", "Aces"), 2 ->("Two", "Twos"), 3 ->("Three", "Threes"),
		4 ->("Four", "Fours"), 5 ->("Five", "Fives"), 6 ->("Six", "Sixes"),
		7 ->("Seven", "Sevens"), 8 ->("Eight", "Eights"), 9 ->("Nine", "Nines"),
		10 ->("Ten", "Tens"), 11 ->("Jack", "Jacks"), 12 ->("Queen", "Queens"),
		13 ->("King", "Kings"))

	val suitDictionary = Map('H' -> "Hearts", 'S' -> "Spades", 'C' -> "Clubs", 'D' -> "Diamonds")


	def fourOfAKind(value: Int) = s"Four of a Kind: ${languageDictionary.get(value).get._2}"

	def threeOfAKind(value: Int) = s"Three of a Kind: ${languageDictionary.get(value).get._2}"

	def twoPair(hand: List[Int]) = {
		val least = hand.groupBy(identity).minBy(_._2.size)._1
		val bigger = hand.distinct.map(_ != least).max
		val smaller = hand.distinct.map(_ != least).min
		s"Two Pair: ${bigger} over ${smaller}"
	}

	def onePair(value: Int) = s"One Pair: ${languageDictionary.get(value).get._2}"

	def fullHouse(hand: List[Int]) = s"Full House: ${languageDictionary.get(hand.distinct.head).get._2} over ${languageDictionary.get(hand.distinct.last).get._2}"

	def flush(suit: Char) = s"Flush: ${suitDictionary.get(suit).get}"

	def straight(value: Int) = s"Straight: ${languageDictionary.get(value - 4).get._1} to ${languageDictionary.get(value).get._1}"

	def straightFlush(value: Int) = s"Straight Flush: ${languageDictionary.get(value - 4).get._1} to ${languageDictionary.get(value).get._1}"

	def highCard(value: Int) = s"High card: ${languageDictionary.get(value).get._1}"

	def classifyHand(hand: List[String]): String = {
		validateCards(hand) match {
			case Invalid(reason) => reason
			case Valid(sortedHand, suits) => showHand(sortedHand, suits)
		}
	}

	def validateCards(hand: List[String]):Validatable = {
		(hand.length,hand.distinct.length) match {
			case (a,_) if a>5 => Invalid("Invalid hand: Too many cards")
			case (a,_) if a<5 => Invalid("Invalid hand: Too few cards")
			case (_,b) if b<5 => Invalid(s"Invalid hand: "+
				s"${valueDictionary.get(hand.groupBy(identity).maxBy(_._2.size)._1.dropRight(1)).get} of" +
				s"${suitDictionary.get(hand.groupBy(identity).maxBy(_._2.size)._1.last).get} appears "+
				s"${hand.groupBy(identity).maxBy(_._2.size)._2.size} times")
			case _ =>
				val sortedHand = hand.map(makeItSortable).sortWith(_>_)
				val suits = hand.map(card => card.last)
				Valid(sortedHand, suits)
		}
	}

	def makeItSortable(value: String): Int = {
		valueDictionary.get(value.dropRight(1)).get
	}

	def showHand(hand: List[Int], suits: List[Char]): String = {

		val numberOfDistinctValue = hand.distinct.length
		val mostFrequency = hand.groupBy(identity).maxBy(_._2.size)._2.size
		val mostFrequentValue = hand.groupBy(identity).maxBy(_._2.size)._1
		def isStraightFlush = isFlush(suits) && isConsecutive(hand)

		 numberOfDistinctValue match {
			case 2  if mostFrequency ==4        => fourOfAKind(mostFrequentValue)
			case 2                              => fullHouse(hand)
			case 3  if mostFrequency ==3        => threeOfAKind(mostFrequentValue)
			case 3                              => twoPair(hand)
			case 4                              => onePair(mostFrequentValue)
			case 5 if isStraightFlush           => straightFlush(hand.max)
			case 5 if isFlush(suits)            => flush(suits.head)
			case 5 if isConsecutive(hand)       => straight(hand.max)
			case 5                              => highCard(hand.max)
			case _                              => "this case should never happen"
		}
	}

	def isFlush(suits: List[Char]): Boolean = suits.distinct.length == 1

	def isConsecutive(hand: List[Int]): Boolean = (hand.head to hand.last by -1).toList.equals(hand)


}


PokerApp.classifyHand(List("AC", "4D", "QC", "3H"))
PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D"))
PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S"))
PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S"))
PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "10S"))
PokerApp.classifyHand(List("AC", "8D", "8C", "8H", "10S"))
PokerApp.classifyHand(List("6C", "6D", "QC", "6H", "6S"))
PokerApp.classifyHand(List("AC", "4D", "QC", "4H", "QS"))
PokerApp.classifyHand(List("5C", "9D", "5H", "9H", "9S"))
PokerApp.classifyHand(List("AH", "7H", "QH", "4H", "10H"))
PokerApp.classifyHand(List("9C", "10D", "JC", "QH", "KS"))
PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D"))
