package blackjack 

import scala.io.StdIn.readLine
// add imports here...
import cards.suits._
import cards.values._
import cards.PlayingCard

object Blackjack{

    /**
     * Compute the value of a PlayingCard given a current sum of cards. 
     */
    def cardValue(card: PlayingCard, current_sum: Int): Int = {
        card match {
            // if it's a number value, we simply return the value
            case PlayingCard(suit, NumberValue(value:Int)) => value
            // if it's Jack, Queen, King, then return 10
            case PlayingCard(suit, Jack) => 10
            case PlayingCard(suit, Queen) => 10
            case PlayingCard(suit, King) => 10
            // if it's Ace, then return 11 or 1 based on the current_sum
            case PlayingCard(suit, Ace) => if(current_sum + 11 <= 21) 11 else 1
        }
    }

    def main(args: Array[String]) {

        val card1 = PlayingCard(Hearts, Queen)
        val card2 = PlayingCard(Diamonds, NumberValue(9))
        val card3 = PlayingCard(Spades, Ace)

        println(cardValue(card1,0))
        // Should be 10 
        println(cardValue(card2,10))
        // Should be 9
        println(cardValue(card3,19))
        // Should be 1
        println(cardValue(card3,2))
        // Should be 11

        // comment the cardValue examples and add game code for Part 3 here
    }

}
