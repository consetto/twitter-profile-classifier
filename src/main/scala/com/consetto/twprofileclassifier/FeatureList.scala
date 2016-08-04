package com.consetto.twitterprofileclassifier

import twitter4j._
import scala.io.Source
import java.io.File
import scala.collection.JavaConversions._
import com.consetto.twitterprofileclassifier.Features._

object FeatureList {

  def makeComputedFeature[A, B](f: Feature[A, B], value: A): ComputedFeature[B] = {
    ComputedFeature(f.name, f.value(value), f.getNominalValues)
  }

  def getComputedFeatures(u: User, s: Seq[Status], firstnames: Seq[String], c: String) : Seq[ComputedFeature[_]] =
    Seq(
      makeComputedFeature(new StatusesCount, (u)) ,
      makeComputedFeature(new FriendsCount, (u)) ,
      makeComputedFeature(new ListedCount, (u)) ,
      makeComputedFeature(new FollowersCount, (u)) ,
      makeComputedFeature(new FriendsFollowersRatio, (u)) ,
      makeComputedFeature(new FavouritesCount, (u)) ,
      makeComputedFeature(new FavouritesFriendsRatio, (u)) ,
      makeComputedFeature(new DescriptionContainsUrl, (u)) ,
      makeComputedFeature(new DescriptionContainsNumber, (u)) ,
      makeComputedFeature(new DescriptionLength, (u)) ,
      makeComputedFeature(new HasProfileLocation, (u)) ,
      makeComputedFeature(new HasBackgroundImage, (u)) ,
      makeComputedFeature(new HasDefaultProfileImage, (u)) ,
      makeComputedFeature(new TweetsPerDay, (u)) ,
      makeComputedFeature(new HasUrl, (u)) ,
      makeComputedFeature(new UrlDomainAccountDistance, (u)) ,
      makeComputedFeature(new FirstNameMatches, (u, firstnames)) ,
      makeComputedFeature(new ScreennameUppercaseRatio, (u)) ,
      makeComputedFeature(new ScreennameContainsNumber, (u)) ,
      makeComputedFeature(new ScreennameIsPlain, (u)) ,
      makeComputedFeature(new NumberOfFaceInProfilePicture, (u)) ,
      makeComputedFeature(new StatusFavouritesCount, (s)) ,
      makeComputedFeature(new StatusRetweetCount, (s)) ,
      makeComputedFeature(new StatusContainsEmoji, (s)) ,
      makeComputedFeature(new StatusIsDuplicate, (s)) ,
      makeComputedFeature(new StatusIsQuote, (s)) ,
      makeComputedFeature(new StatusIsPlain, (s)) ,
      makeComputedFeature(new StatusContainsMedia, (s)) ,
      makeComputedFeature(new StatusContainsHashtag, (s)) ,
      makeComputedFeature(new StatusContainsUserMentions, (s)) ,
      makeComputedFeature(new StatusContainsUrl, (s)) ,
      makeComputedFeature(new StatusUrlDomainDistance, (u, s)) ,
      makeComputedFeature(new StatusHashtagDomainDistance, (u, s)) ,
      makeComputedFeature(new StatusContainsSymbol, s) ,
      makeComputedFeature(new StatusDistinctUrl, s) ,
      makeComputedFeature(new StatusIsInReplyTo, (s)) ,
      makeComputedFeature(new StatusRetweetedCount, (s)) ,
      makeComputedFeature(new TargetClass(),(c))
    )

  def getComputedFeaturesFromDirectory(userFolder: File, firstnames: Seq[String], c: String) = {
    val userInfoFile = new File(userFolder, "userInfo.json")
    val statusesFile = new File(userFolder, "lastTweets.json")

    val user = TwitterObjectFactory.createUser(
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

    getComputedFeatures(user, statuses, firstnames, c)
  }
}
