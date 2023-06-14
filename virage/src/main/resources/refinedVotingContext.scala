import scala.util.chaining.scalaUtilChainingOps
import scala.language.postfixOps

import scala.io.Source
import scala.collection.mutable.HashMap


object VotingContext {
  
  def readBallotsFromFile(file: String): 
    (List[Nat.nat], List[List[Nat.nat]], Map[Nat.nat,String]) = {
    var ballots: List[List[Nat.nat]] = Nil

    var candidatesMap = scala.collection.immutable.Map[String,Int]()
    var numtoNamesMap = scala.collection.immutable.Map[Nat.nat,String]()

    var candidates: List[String] = Nil


    for(line <- Source.fromFile(file).getLines()) {
      val strings: Array[String] = line split ';'
      
      
      if(candidates == Nil) {
        candidates = strings.toList
        
        candidatesMap = (candidates zip candidates.indices).toMap

        numtoNamesMap = (candidates.indices.map(n =>  Nat.Nata(BigInt(n))) zip candidates).toMap
      }
      val ballotnum = strings.toList.reverse.map(x => Nat.Nata(BigInt(candidatesMap(x))))
      
      val stringArray = List(ballotnum)

      ballots = ballots ::: stringArray
    }

    var numcandidates = candidates.map(x => Nat.Nata(BigInt(candidatesMap(x))))
    
    (numcandidates, ballots, numtoNamesMap)
  }

  def prettyPrint(result: (Hash_Table.hashtable[Nat.nat, Unit],
              (Hash_Table.hashtable[Nat.nat, Unit],
                Hash_Table.hashtable[Nat.nat, Unit])),
                numtoNamesMap: Map[Nat.nat,String]): Unit = {
    val elected = result._1
    val rejected = result._2._1
    val deferred = result._2._2
    
    print("elected: ");
    Hash_Table.the_array(elected).toList.map(x => x.map(y => print(numtoNamesMap(y._1) + ", ")))
    println();
    print("deferred: ");
    Hash_Table.the_array(deferred).toList.map(x => x.map(y => print(numtoNamesMap(y._1) + ", ")))
    println();
    print("rejected: ");
    Hash_Table.the_array(rejected).toList.map(x => x.map(y => print(numtoNamesMap(y._1)+ ", ")))
    println();
  }

   def time[A,B,T](reps: Int, f: (A, B) => T, p1: A, p2: B): T = {
    val start = System.nanoTime()
    val ret = f(p1,p2)
    val end = System.nanoTime()
    println(s"Time taken: ${(end - start) / 1000 / 1000} ms")
    ret
  }
  
}

object Main {

  def computeresult(candidateskv:Hash_Table.hashtable[Nat.nat, Unit],
                  converted_ballots: List[(Array.T[Nat.nat], Nat.nat)]):((Hash_Table.hashtable[Nat.nat, Unit],
              (Hash_Table.hashtable[Nat.nat, Unit],
                Hash_Table.hashtable[Nat.nat, Unit]))) = {
    val module = Blacks_Rule_Ref.
         blacks_sep(candidateskv)

     val result = module(converted_ballots)()
     (result)
    } 

  def main(args: Array[String]): Unit = {
    val (candidates, ballots, numtoNamesMap) = VotingContext.readBallotsFromFile(args(0))
    
       
    val converted_ballots = ballots.map(x => 
        Profile_List_Monadic.clist(x)())

    val candidateskv = Profile_List_Monadic.convert_list_to_hash_set(candidates)()
    
    

    println("Reading finished.")

    
    val reti = VotingContext.time(1, computeresult, candidateskv, converted_ballots)
    

    VotingContext.prettyPrint(reti, numtoNamesMap)

  }
}

