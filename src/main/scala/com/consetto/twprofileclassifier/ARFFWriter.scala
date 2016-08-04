package com.consetto.twitterprofileclassifier

import scala.io.Source
import java.io.{FileWriter, BufferedWriter, File}
import twitter4j._
import scala.collection.JavaConversions._

object ARFFWriter extends App {

  val outputFolder: String = "output/crawler/"
  val resourceFolder: String = "./src/main/resources/"

  val firstnames =
    Source.
      fromFile(new File(resourceFolder, "firstnames.txt")).
      getLines.
      map(x=>x.toLowerCase).
      toList

  val manualClassified =
    Source.
      fromFile(new File(resourceFolder, "dataset.csv")).
      getLines.
      take(440).
      map(_.split(","))
  
  val dataset = manualClassified.drop(2).map { x =>
    val dirName = x(0)
    val classTag = x(2).trim

    val twittDir = new File(outputFolder , dirName)
    val userInfoFile = new File(twittDir, "userInfo.json")
    val statusesFile = new File(twittDir, "lastTweets.json")

    val userInfo = TwitterObjectFactory.createUser(
      Source.
        fromFile(userInfoFile).
        getLines.
        mkString
    )

    val statusArrayJson = new org.json.JSONArray(
      Source.fromFile(statusesFile).getLines.mkString
    )
    
    var statuses: Seq[Status] = Seq()
    val q = statusArrayJson.toList
    for( i <- 0 to statusArrayJson.length - 1) {
      statuses = statuses :+ TwitterObjectFactory.createStatus(statusArrayJson.getJSONObject(i).toString)
    }
    
    FeatureList.getComputedFeatures(userInfo, statuses,firstnames, classTag)
  }.toList

  val writer = new BufferedWriter(new FileWriter("output/twitter.arff"))
  ARFFHelper.writeFile("twitter", writer, dataset)
  writer.flush
  writer.close
}
