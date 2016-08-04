package com.consetto.twitterprofileclassifier

import twitter4j.{User, Status, TwitterObjectFactory, URLEntity}
import scala.collection.JavaConversions._
import scala.io.Source
import java.io.{BufferedWriter, FileWriter, File}
import org.bytedeco.javacpp.{opencv_highgui, opencv_imgproc, opencv_imgcodecs}
import org.bytedeco.javacpp.opencv_core._
import org.bytedeco.javacpp.opencv_objdetect.CascadeClassifier
import org.bytedeco.javacpp.opencv_imgcodecs.cvLoadImage

case class ComputedFeature[T](name: String, value: T, nominalValues: Option[Seq[String]])

trait Feature[In, Out] { self =>
  def value(line: In): Out
  def valueSafe(line: In): Option[Out] = {
    try {
      Some(value(line))
    } catch {
      case e: Throwable => None
    }
  }
  def name: String = self.getClass.getSimpleName
  def getType: String
  def getNominalValues: Option[List[String]]
}

trait BooleanFeature[In] extends Feature[In, Boolean] {
  def getType: String = "boolean"
  def getNominalValues: Option[List[String]] = Some(List("true", "false", "undefined"))
}

trait IntFeature[In] extends Feature[In, Int] {
  def getType: String = "int"
  def getNominalValues = None
}

trait DoubleFeature[In] extends Feature[In, Double] {
  def getType: String = "double"
  def getNominalValues = None
}

trait StringFeature[In] extends Feature[In, Option[String]] {
  def getType: String = "string"
  def getNominalValues: Option[List[String]] = None
}

object Features {

  class TargetClass extends Feature[String, String] {
    def value(s: String) = s
    def getNominalValues = Some(List("C", "P"))
    def getType = "string"
  }

  class ContainsEmoji extends BooleanFeature[String] {
    val regexEmoji = "([\\u20a0-\\u32ff\\ud83c\\udc00-\\ud83d\\udeff\\udbb9\\udce5-\\udbb9\\udcee])".r.pattern

    def value(s: String) = (
      regexEmoji.matcher(s.toString).find() ||
			  s.contains(":-)") ||
			  s.contains(":)") ||
			  s.contains(";-)") ||
			  s.contains(";)") ||
			  s.contains(":-D") ||
			  s.contains(":D")
    )
  }

  class StringDistance extends IntFeature[(String, String)] {
      def stringDistance(s1: String, s2: String): Int = {
        val memo = scala.collection.mutable.Map[(List[Char],List[Char]),Int]()
        def min(a:Int, b:Int, c:Int) = Math.min( Math.min( a, b ), c)
        def sd(s1: List[Char], s2: List[Char]): Int = {
        if (memo.contains((s1,s2)) == false)
          memo((s1,s2)) = (s1, s2) match {
            case (_, Nil) => s1.length
            case (Nil, _) => s2.length
            case (c1::t1, c2::t2)  => min( sd(t1,s2) + 1, sd(s1,t2) + 1,
                                           sd(t1,t2) + (if (c1==c2) 0 else 1) )
          }
        memo((s1,s2))
        }

        sd( s1.toList, s2.toList )
      }

      def value(s: (String, String)) = stringDistance(s._1, s._2)
  }

  class DomainExtractor extends StringFeature[String] {
    def value(s: String) = {
      val url = s.replace("://",".")
      val slashIndex = url.indexOf("/")
      val firstPart = if (slashIndex>=0) url.substring(0, slashIndex) else url    
      val subdomainDomain = url.substring(0, firstPart.lastIndexOf("."))    
      val domain  = 
        if (subdomainDomain.lastIndexOf(".")>=0) 
          subdomainDomain.substring(subdomainDomain.lastIndexOf(".")+1)
        else 
          subdomainDomain
      Some(domain)
    } 
  }

  class UrlExtractor extends StringFeature[(Int, Array[URLEntity])] {
    def value(p: (Int, Array[URLEntity])) = {
      val index = p._1
      val urls = p._2
      if (urls != null && urls.size > index) {
        val firstUrlEntity = urls(index)
        val url = Option(firstUrlEntity.getDisplayURL()) getOrElse firstUrlEntity.getURL()
        Some(url)
      } else
        None
    }
  }

  class ContainsUrl extends BooleanFeature[String] {
    def value(s: String) =
      s.contains("http://") || s.contains("https://")
  }

  class StatusesCount extends DoubleFeature[User] {
    def value(u: User) = if (u.getStatusesCount>0) math.log10(u.getStatusesCount) else 0
  }

  class FriendsCount extends DoubleFeature[User]  {
    def value(u: User) = if (u.getFriendsCount>0) math.log10(u.getFriendsCount) else 0
  }

  class ListedCount extends DoubleFeature[User]  {
    def value(u: User) = if (u.getListedCount>0) math.log10(u.getListedCount) else 0
  }


  class FollowersCount extends DoubleFeature[User]  {
    def value(u: User) = if (u.getFollowersCount>0) math.log10(u.getFollowersCount) else 0
  }

  class FriendsFollowersRatio extends DoubleFeature[User]  {
    def value(u: User) = 
      if (u.getFollowersCount.toDouble!=0)
        u.getFriendsCount.toDouble / u.getFollowersCount.toDouble
      else
        0
  }

  class FavouritesCount extends DoubleFeature[User] {
    def value(u: User) = if (u.getFavouritesCount>0) math.log10(u.getFavouritesCount) else 0
  }

  class FavouritesFriendsRatio extends DoubleFeature[User] {
    def value(u: User) = 
      math.min(if (u.getFriendsCount.toDouble!=0)
        u.getFavouritesCount.toDouble / u.getFriendsCount.toDouble
      else
        0, 5)
  }

  class DescriptionContainsUrl extends BooleanFeature[User] {
    def value(u: User) = (new ContainsUrl).value(u.getDescription)
  }

  class DescriptionContainsNumber extends BooleanFeature[User] {
    def value(u: User) = u.getDescription.matches(".*\\d+.*")
  }

  class DescriptionLength extends IntFeature[User] {
    def value(u: User) = u.getDescription.size
  }

  class HasProfileLocation extends BooleanFeature[User] {
    def value(u: User) = u.getLocation != null && u.getLocation.trim.size > 0
  }

  class HasBackgroundImage extends BooleanFeature[User] {
    def value(u: User) = u.isProfileUseBackgroundImage
  }

  class HasDefaultProfileImage extends BooleanFeature[User] {
    def value(u: User) = u.isDefaultProfileImage
  }

  class TweetsPerDay extends DoubleFeature[User] {
    def value(u: User) = {
      val r = (u.getStatusesCount.toDouble / (
        (((new java.util.Date).getTime - u.getCreatedAt.getTime) / (1000*60*60*24)).toDouble
      )).toInt
      if (r>0) math.log10(r) else 0
    }
  }

  class HasUrl extends BooleanFeature[User] {
    def value(u: User) = u.getDescriptionURLEntities != null && u.getDescriptionURLEntities.size > 0
  }

  class UrlDomainAccountDistance extends IntFeature[User] {
    def value(u: User) =
      (new UrlExtractor()).value((0,u.getDescriptionURLEntities)) match {
        case Some(url: String) => (new DomainExtractor).value(url) match {
          case Some(domain: String) => (new StringDistance).value((domain, u.getScreenName))
          case _ => 0
        }
        case _ => 0
      }
  }

  class FirstNameMatches extends BooleanFeature[(User, Seq[String])] {
    def value(un: (User, Seq[String])) = un._2.contains(un._1.getName.split(" ")(0).toLowerCase)
  }

  class ScreennameUppercaseRatio extends DoubleFeature[User] {
    def value(u: User) = 
      u.getScreenName.filter(c => Character.isUpperCase(c)).size.toDouble / u.getScreenName.size.toDouble    
  }

  class ScreennameContainsNumber extends BooleanFeature[User] {
    def value(u: User) = u.getScreenName.matches(".*\\d+.*")
  }

  class ScreennameIsPlain extends BooleanFeature[User] {
    def value(u: User) = u.getScreenName.matches("[a-zA-Z]+")
  }

  class NumberOfFaceInProfilePicture extends IntFeature[User] {
    def value(u: User) = {
      try {
        val mat = opencv_imgcodecs.imread("./output/crawler/"+u.getScreenName+"/profile.jpg")
        val greyMat = new Mat()
        opencv_imgproc.cvtColor(mat, greyMat, opencv_imgproc.CV_BGR2GRAY, 1)
        val equalizedMat = new Mat()
        opencv_imgproc.equalizeHist(greyMat, equalizedMat)
        val faceXml = "./src/main/resources/haarcascade_frontalface_alt.xml"
        val faceCascade = new CascadeClassifier(faceXml)
        val faceRects = new RectVector()
        faceCascade.detectMultiScale(equalizedMat, faceRects)
        faceRects.size().toInt
      }
      catch {
        case e: Exception => 0
      }
    }
  }

  class StatusFavouritesCount extends DoubleFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = {
      val r = s.map(_.getFavoriteCount).foldLeft(0)(_ + _)
      if (r>0) math.log10(r) else 0
    }
  }

  class StatusRetweetCount extends DoubleFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = {
      val r = s.map(_.getRetweetCount).foldLeft(0)(_ + _)
      if (r>0) math.log10(r) else 0
    }
  }

  class StatusContainsEmoji extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map { status => (new ContainsEmoji).value(status.getText) }.filter(x=>x).size
  }

  class StatusIsDuplicate extends BooleanFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getText).toSet.size != s.size
  }

  class StatusIsQuote extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getQuotedStatusId != -1).filter(x=>x).size
  }

  class StatusIsPlain extends IntFeature[ Seq[Status]] {  
    def value(s:  Seq[Status]) = s.map { status => 
        status.getQuotedStatusId == -1 &&
        status.getHashtagEntities.size == 0 &&
        status.getUserMentionEntities.size == 0 &&
        status.getMediaEntities.size == 0 &&
        status.getURLEntities.size == 0
      }.filter(x=>x).size
  }

  class StatusContainsMedia extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getMediaEntities.size > 0).filter(x=>x).size
  }

  class StatusContainsHashtag extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getHashtagEntities.size > 0).filter(x=>x).size
  }

  class StatusContainsUserMentions extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getUserMentionEntities.size > 0).filter(x=>x).size
  }

  class StatusContainsUrl extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getURLEntities.size > 0).filter(x=>x).size
  }

  class StatusUrlDomainDistance extends IntFeature[(User, Seq[Status])] {  
    def value(p: (User, Seq[Status])) = p._2.map { s =>
        (new UrlExtractor()).value((0,s.getURLEntities)) match {
          case Some(url: String) => (new DomainExtractor).value(url) match {
            case Some(domain: String) => (new StringDistance).value((domain, p._1.getScreenName))
            case _ => 0
          }
          case _ => 0
        }
      }.foldLeft(0)(_ + _)
  }

  class StatusHashtagDomainDistance extends IntFeature[(User, Seq[Status])] {  
    def value(p: (User, Seq[Status])) = p._2.map { s =>
        if (s.getHashtagEntities != null && s.getHashtagEntities.size > 0) {
          val hashTag = s.getHashtagEntities()(0).getText()
          (new StringDistance).value((hashTag, p._1.getScreenName))            
        } else
          0
      }.foldLeft(0)(_ + _)
  }

  class StatusContainsSymbol extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getSymbolEntities().size > 0).filter(x=>x).size
  }

  class StatusDistinctUrl extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) =
      s.map { s =>
        (new UrlExtractor()).value((0,s.getURLEntities)) match {
          case Some(url: String) => (new DomainExtractor).value(url) match {
            case Some(domain: String) => domain
            case _ => ""
          }
          case _ => ""
        }
      }.toSet.size
  }

  class StatusIsInReplyTo extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.map(_.getInReplyToUserId != -1).filter(x=>x).size
  }

  class StatusRetweetedCount extends IntFeature[Seq[Status]] {  
    def value(s: Seq[Status]) = s.filter(_.isRetweet()).size
  }


}
