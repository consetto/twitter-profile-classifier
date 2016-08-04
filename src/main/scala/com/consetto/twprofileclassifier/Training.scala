package com.consetto.twitterprofileclassifier

import scala.io.Source
import java.io.{File, FileOutputStream}
import twitter4j._
import scala.collection.JavaConversions._
import weka.core.{Instances, Instance, Attribute, DenseInstance}
import weka.classifiers.Evaluation
import weka.classifiers.trees.RandomForest
import java.util.zip.{GZIPOutputStream}

object Training extends App {

  val outputFolder: String = "output/crawler/"

  // Load required data
  val firstnames = DataLoaders.loadFirstnames
  val manualClassified = DataLoaders.loadManualClassified

  // Build a set of features for each profile
  val dataset = manualClassified.map { case (dirName, classTag) =>
    val twittDir = new File(outputFolder , dirName)
    FeatureList.getComputedFeaturesFromDirectory(twittDir, firstnames, classTag)
  }.toList

  // Convert Features to Weka instances
  val instances = new Instances(
    "twitter-profile-classifier",
    new java.util.ArrayList(ARFFHelper.toAttributes(dataset.head).toList),
    dataset.size
  )
  instances.setClassIndex(instances.numAttributes - 1)

  dataset.foreach { item =>
    instances.add(ARFFHelper.toInstance(instances, item))
  }

  // Setup the classifier
  val classifier = new RandomForest()

  // Evaluate the classifier
  println("Running 10-fold cross evaluation")
  val evaluation = new Evaluation(instances)
  evaluation.crossValidateModel(classifier, instances, 10, new java.util.Random)
  println(evaluation.toSummaryString)

  // Train the classifier
  classifier.buildClassifier(instances)

  // Save model
  println("Saving classifier to file")
  val os = new GZIPOutputStream(new FileOutputStream("output/model.ser.gz"))
  weka.core.SerializationHelper.write(os, classifier)
}
