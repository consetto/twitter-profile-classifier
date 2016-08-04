package com.consetto.twitterprofileclassifier

import java.io.{File, Writer, BufferedWriter, FileWriter}
import weka.core.{Attribute, DenseInstance, Instances}
import scala.collection.JavaConversions._

object ARFFHelper {

  def toAttributes(features: Seq[ComputedFeature[_]]): Seq[Attribute] = {
    features.map {
      case f if f.nominalValues.isDefined =>
        new Attribute(f.name, f.nominalValues.get)
      case f: ComputedFeature[_] =>
        new Attribute(f.name)
    }
  }

  def toInstance(dataset: Instances, features: Seq[ComputedFeature[_]]) = {
    val inst = new DenseInstance(features.size)
    inst.setDataset(dataset)
    features.zipWithIndex.foreach { case (f, idx) =>
      try {
        f match {
          case f if f.nominalValues.isDefined =>
            inst.setValue(idx, f.value.toString)
          case f: ComputedFeature[_] if (f.value.isInstanceOf[Int]) =>
            inst.setValue(idx, f.value.asInstanceOf[Int].toDouble)
          case f: ComputedFeature[_] if (f.value.isInstanceOf[Double]) =>
            inst.setValue(idx, f.value.asInstanceOf[Double])
          case _ =>
            throw new Exception("not supported: " + f.name)
        }
      } catch {
        case e: IllegalArgumentException =>
          throw new IllegalArgumentException("can not set value for feature " + f.name + ": '" + f.value + "'", e)
      }
    }
    inst
  }

  def writeHeader(writer: Writer, features: Seq[ComputedFeature[_]]) = {
    // write features
    features.foreach { feature =>
      writer.write("@ATTRIBUTE ")
      writer.write("\"" + feature.name + "\"")
      writer.write(" ")

      feature match {
        case f if f.nominalValues.isDefined =>
          writer.write(f.nominalValues.get.mkString("{", ",", "}"))
        case f: ComputedFeature[_] if (f.value.isInstanceOf[Int]) =>
          writer.write("numeric")
        case f: ComputedFeature[_] if (f.value.isInstanceOf[Int]) =>
          writer.write("numeric")
        case _ =>
          writer.write("string")
      }
      writer.write("\n")
    }
  }

  def writeLine(writer: Writer, features: Seq[ComputedFeature[_]]) = {
    val featureLine = features.map { x => x match {
      case f: ComputedFeature[Boolean] =>
        f.value.toString
      case f: ComputedFeature[Int] =>
        f.value.toString
      case f: ComputedFeature[Double] =>
        f.value.toString
      case f: ComputedFeature[String] =>
        f.value
      case f =>
        throw new Exception("can not handle feature: " + f)
    }}.mkString(",")

    writer.write(featureLine)
    writer.write("\n")
  }

  def writeFile(name: String, writer: Writer, xs: Seq[Seq[ComputedFeature[_]]]) = {
    writer.write("@RELATION ")
    writer.write(name)
    writer.write("\n")

    writeHeader(writer, xs.head)
    writer.write("\n")

    writer.write("@DATA\n")
    xs.foreach(m => writeLine(writer, m))
  }
}
