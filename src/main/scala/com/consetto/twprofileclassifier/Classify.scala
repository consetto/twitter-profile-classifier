package com.consetto.twitterprofileclassifier

import java.nio.file.Files
import weka.classifiers.trees.RandomForest
import java.io.{FileInputStream, File}
import java.util.zip.GZIPInputStream
import weka.core.Instances
import scala.collection.JavaConversions._
import twitter4j._
import scala.io.Source

object Classify extends App {

  val screenname = args(0)
  val tmpFolder = Files.createTempDirectory("tw-classifier-")

  // Get data of profile
  val c = new Crawler(outputFolder = tmpFolder.toString)
  c.crawlProfile(screenname)

  // load classifier
  val is = new GZIPInputStream(new FileInputStream("output/model.ser.gz"))
  val classifier = weka.core.SerializationHelper.read(is).asInstanceOf[RandomForest]

  // load data
  val firstnames = DataLoaders.loadFirstnames

  // create features for profile
  val userFolder = new File(tmpFolder.toString, screenname)
  val features = FeatureList.getComputedFeaturesFromDirectory(userFolder, firstnames, "C")
  val instances = new Instances(
    "twitter-profile-classifier",
    new java.util.ArrayList(ARFFHelper.toAttributes(features).toList),
    1
  )
  instances.setClassIndex(instances.numAttributes - 1)

  // Classify user
  val clsLabel = classifier.classifyInstance(ARFFHelper.toInstance(instances, features))
  val clsName = instances.classAttribute.value(clsLabel.toInt)

  // output result
  println(screenname + " is a " + (if (clsName == "P") "Person" else "Company"))
}
