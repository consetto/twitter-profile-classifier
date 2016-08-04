package com.consetto.twitterprofileclassifier

import twitter4j.TwitterFactory
import twitter4j.Twitter
import twitter4j.User
import twitter4j.conf.ConfigurationBuilder
import twitter4j.TwitterObjectFactory
import scala.collection.JavaConversions._
import java.io.{BufferedWriter, FileWriter, File}
import scala.util.parsing.json._
import sys.process._
import com.typesafe.config.ConfigFactory

class Crawler(outputFolder: String = "output/crawler") {

  val path = new File(outputFolder)

  val LANG = "en";
  val TWEETLIMIT = 10;

  lazy val client = configTwitter

  def getFollowersList(client: Twitter, id: Long): Seq[User] =
    client.getFollowersList(id, -1, 200)

  def configTwitter() : Twitter = {
    val config = ConfigFactory.load()

    val cb = new ConfigurationBuilder()
    cb.setDebugEnabled(true)
      .setOAuthConsumerKey(config.getString("twitter.consumer-key"))
      .setOAuthConsumerSecret(config.getString("twitter.consumer-secret"))
      .setOAuthAccessToken(config.getString("twitter.token"))
      .setOAuthAccessTokenSecret(config.getString("twitter.token-secret"))
      .setJSONStoreEnabled(true)

    (new TwitterFactory(cb.build)).getInstance
  }

  def writeToFile(f: File, content: String) = {
    val writer = new BufferedWriter(new FileWriter(f))
    writer.write(content)
    writer.flush
    writer.close
  }

  def downloadFile(url: String, f: File) = {
	  new java.net.URL(url) #> f !!
	}

  def checkLimit: Option[(String, Int)] = {
    val runOut = client.getRateLimitStatus.
      collect {
        case (endpoint, status) if (status.getRemaining <= 1) => (endpoint, status.getSecondsUntilReset)
      }.toVector

    if (runOut.size > 0) {
      Some(runOut.maxBy(_._2))
    } else {
      None
    }
  }

  def waitBeforeNextCall = {
    checkLimit.foreach { case (endpoint, timeToWait) =>
      println(s"""Limit reached for ${endpoint}. Waiting for ${timeToWait} seconds""")
      Thread.sleep(timeToWait * 1000)
    }
  }

  def crawlProfile(screenName: String) = {
    // Create folder for username
    val userIdStr = screenName
    val userPath = new File(path, userIdStr)
    userPath.mkdir

    waitBeforeNextCall
    val user = client.showUser(screenName)

    // Download info
    writeToFile(
      new File(userPath, "userInfo.json"),
      TwitterObjectFactory.getRawJSON(user)
    )

    waitBeforeNextCall
    writeToFile(
      new File(userPath, "lastTweets.json"),
      TwitterObjectFactory.getRawJSON(client.getUserTimeline(user.getId()))
    )

    // Download profile picture
    downloadFile(
      user.getProfileImageURL.replace("_normal.", "_200x200."),
      new File(userPath, "profile.jpg")
    )
  }

  def crawl = {
    var userToCrawl: Set[Long] = Set(5768872L) // Let's start with GaryVee
    var alreadyCrawled: Set[Long] = Set()

    if (!path.exists) {
      path.mkdir
    }

    while (!userToCrawl.isEmpty) {
      val nextId = userToCrawl.head

      waitBeforeNextCall
      try {
        val followers = getFollowersList(client, nextId).
          map { user =>
            (user, TwitterObjectFactory.getRawJSON(user))
          }.
          filter { case (user, json) =>
            user.getLang == LANG && user.getStatusesCount() >= TWEETLIMIT
          }

        followers.map { case (user, json) =>
          if (user.getStatus != null) {
            println("Crawling: " + user.getScreenName() + "(" + user.getId() + ") ...")

            // Create folder for username
            val userIdStr = user.getScreenName()
            val userPath = new File(path, userIdStr)
            userPath.mkdir

            // Download info
            writeToFile(
              new File(userPath, "userInfo.json"),
              json
            )

            // Download last tweets
            waitBeforeNextCall
            writeToFile(
              new File(userPath, "lastTweets.json"),
              TwitterObjectFactory.getRawJSON(client.getUserTimeline(user.getId()))
            )

            // Download profile picture
            downloadFile(
              user.getProfileImageURL.replace("_normal.", "_200x200."),
              new File(userPath, "profile.jpg")
            )
          }
        }

        alreadyCrawled = alreadyCrawled + nextId
        userToCrawl = (userToCrawl ++ followers.map(_._1.getId)) -- alreadyCrawled
      } catch {
        case e: Exception =>
          alreadyCrawled = alreadyCrawled + nextId
          userToCrawl = userToCrawl - nextId
      }
    }
  }
}

object Crawler extends App {
  val c = new Crawler()
  c.crawl
}
