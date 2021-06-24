#' @name
#' visvow
#'
#' @title
#' Visible Vowels
#'
#' @aliases
#' visvow
#'
#' @description
#' Visible Vowels is an app that visualizes vowel variation in f0, F1, F2, F3 and duration.
#'
#' A vowel is a speech sound produced without audible impediment to the airflow in the mouth and/or throat.
#' Each vowel has a particular pitch (except when whispered), quality (timbre) and duration.
#' f0 is the fundamental frequency of the periodic waveform and determines the perceived pitch.
#' The quality is determined by the formants.
#' Formants are resonance frequencies that define the spectral shape of vowels (and vowel-like sounds).
#' The formant with the lowest frequency is called F1, the second-lowest F2, and the third F3.
#' F1 is correlated with tongue height.
#' The closer the tongue approaches the palate, the lower F1.
#' F2 correlates with tongue retraction and lip protrusion.
#' The more the tongue is positioned towards the front of the mouth, and the wider the lips are spread, the higher F2.
#' F3 correlates with the tongue-blade position.
#' The closer the blade is to the lips, the higher is F3.
#' The acoustic vowel duration primarily corresponds with the perceived duration of a vowel sound.
#' See Johnson (2012).
#'
#' @usage
#' visvow()
#'
#' @format
#' NULL
#'
#' @details
#' \code{visvow()} opens Visible Vowels in your default web browser.
#'
#' @seealso
#' The Help tab in the app provides more information about the format of the input file.
#' Details about scale conversion and speaker normalization procedures and some specific metrics are found in the vignette.
#'
#' @references
#' \insertRef{johnson:2012}{visvow}
#'
#' @examples
#' if (interactive()){
#'   visvow()
#' }
#'
#' @import
#' shiny shinyBS stats tidyr PBSmapping ggplot2 plot3D MASS ggdendro ggrepel readxl WriteXLS pracma Rtsne plyr grid svglite Cairo tikzDevice shinybusy
#'
#' @importFrom
#' formattable renderFormattable formattable formatter style color_tile formattableOutput
#' 
#' @importFrom
#' Rdpack reprompt
#'
#' @export
#' visvow
NULL

################################################################################

Scale <- function(h,replyScale,Ref)
{
  if (replyScale==" Hz")
  {
    s <- h
  }

  if (replyScale==" bark I")
  {
    s = 7*log(h/650+sqrt(1+(h/650)^2))
  }

  if (replyScale==" bark II")
  {
    s = 13 * atan(0.00076*h) + 3.5 * atan((h/7500)^2)
  }

  if (replyScale==" bark III")
  {
    s <- (26.81 * (h/(1960+h))) - 0.53

    s[which(s< 2  )] <- s[which(s< 2  )] + (0.15 * (2-s[which(s< 2  )]     ))
    s[which(s>20.1)] <- s[which(s>20.1)] + (0.22 * (  s[which(s>20.1)]-20.1))
  }

  if (replyScale==" ERB I")
  {
    s <- 16.7 * log10(1 + (h/165.4))
  }

  if (replyScale==" ERB II")
  {
    s <- 11.17 * log((h+312) / (h+14675)) + 43
  }

  if (replyScale==" ERB III")
  {
    s <- 21.4 * log10((0.00437*h)+1)
  }

  if (replyScale==" ln")
  {
    s <- log(h)
  }

  if (replyScale==" mel I")
  {
    s <- (1000/log10(2)) * log10((h/1000) + 1)
  }

  if (replyScale==" mel II")
  {
    s <-1127 * log(1 + (h/700))
  }

  if (replyScale==" ST")
  {
    s <- 12 * log2(h/Ref)
  }

  return(s)
}

vowelScale <- function(vowelTab, replyScale, Ref)
{
  if (is.null(vowelTab))
    return(NULL)

  indexVowel <- grep("^vowel$", colnames(vowelTab))
  nColumns   <- ncol(vowelTab)
  nPoints    <- (nColumns - (indexVowel + 1))/5

  vT <- vowelTab

  for (i in (1:nPoints))
  {
    indexTime <- indexVowel + 2 + ((i-1)*5)

    for (j in ((indexTime+1):(indexTime+4)))
    {
      vT[,j] <- Scale(vT[,j],replyScale, Ref)
    }
  }

  return(vT)
}

################################################################################

vowelLong1 <- function(vowelScale,replyTimesN)
{
  indexVowel <- grep("^vowel$", colnames(vowelScale))
  nColumns   <- ncol(vowelScale)
  nPoints    <- (nColumns - (indexVowel + 1))/5

  vT <- data.frame()

  for (i in (1:nPoints))
  {
    if (is.element(as.character(i),replyTimesN))
    {
      indexTime <- indexVowel + 2 + ((i-1)*5)

      vTsub <- data.frame(speaker = vowelScale[,1],
                          vowel   = vowelScale[,indexVowel],
                          point   = i,
                          f0      = vowelScale[,indexTime+1],
                          f1      = vowelScale[,indexTime+2],
                          f2      = vowelScale[,indexTime+3],
                          f3      = vowelScale[,indexTime+4])

      vT <- rbind(vT,vTsub)
    }
  }

  return(vT)
}

vowelLong2 <- function(vowelLong1)
{
  vT <- data.frame()

  for (j in (1:2))
  {
    vTsub <- data.frame(speaker = vowelLong1$speaker,
                        vowel   = vowelLong1$vowel,
                        point   = vowelLong1$point,
                        formant = j,
                        f       = log(vowelLong1[,j+4]))

    vT <- rbind(vT,vTsub)
  }

  return(vT)
}

vowelLong3 <- function(vowelLong1)
{
  vT <- data.frame()

  for (j in (1:3))
  {
    vTsub <- data.frame(speaker = vowelLong1$speaker,
                        vowel   = vowelLong1$vowel,
                        point   = vowelLong1$point,
                        formant = j,
                        f       = log(vowelLong1[,j+4]))

    vT <- rbind(vT,vTsub)
  }

  return(vT)
}

vowelLong4 <- function(vowelLong1)
{
  vT <- data.frame()

  for (j in (0:3))
  {
    vTsub <- data.frame(speaker = vowelLong1$speaker,
                        vowel   = vowelLong1$vowel,
                        point   = vowelLong1$point,
                        formant = j,
                        f       = log(vowelLong1[,j+4]))

    vT <- rbind(vT,vTsub)
  }

  return(vT)
}

vowelNormF <- function(vowelScale,vowelLong1,vowelLong2,vowelLong3,vowelLong4,replyNormal)
{
  if (is.null(vowelScale))
    return(NULL)

  indexVowel <- grep("^vowel$", colnames(vowelScale))
  nColumns   <- ncol(vowelScale)
  nPoints    <- (nColumns - (indexVowel + 1))/5

   SpeaKer   <- unique(vowelScale[,1])
  nSpeaKer   <- length(SpeaKer)

   VoWel     <- unique(vowelScale[,indexVowel])
  nVoWel     <- length(VoWel)

  if (replyNormal=="")
  {
    vT <- vowelScale
  }

  if (replyNormal==" Peterson")
  {
    vT <- vowelScale
    
    for (i in (1:nPoints))
    {
      indexTime <- indexVowel + 2 + ((i-1)*5)
      
      vT[,indexTime+1] <- vT[,indexTime+1]/vT[,indexTime+4]
      vT[,indexTime+2] <- vT[,indexTime+2]/vT[,indexTime+4]
      vT[,indexTime+3] <- vT[,indexTime+3]/vT[,indexTime+4]
    }
  }
  
  if (replyNormal==" Sussman")
  {
    vT <- vowelScale

    for (i in (1:nPoints))
    {
      indexTime <- indexVowel + 2 + ((i-1)*5)

      F123 <- psych::geometric.mean(c(vT[,indexTime+2],vT[,indexTime+3],vT[,indexTime+4]))

      vT[,indexTime+2] <- log(vT[,indexTime+2]/F123)
      vT[,indexTime+3] <- log(vT[,indexTime+3]/F123)
      vT[,indexTime+4] <- log(vT[,indexTime+4]/F123)
    }
  }

  if (replyNormal==" Syrdal & Gopal")
  {
    vT <- vowelScale

    for (i in (1:nPoints))
    {
      indexTime <- indexVowel + 2 + ((i-1)*5)

      vT[,indexTime+2] <-  1 * (vT[,indexTime+2] - vT[,indexTime+1])
      vT[,indexTime+3] <- -1 * (vT[,indexTime+4] - vT[,indexTime+3])
    }
  }

  if (replyNormal==" Miller")
  {
    vT <- data.frame()
    vTLong1Ag <- aggregate(f0~speaker+vowel+point, data=vowelLong1, FUN=psych::geometric.mean)

    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

      GMf0 <- psych::geometric.mean(vTLong1AgSub$f0)
      SR   <- 168*((GMf0/168)^(1/3))

      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])

      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)

        vTsub[,indexTime+4] <- log(vTsub[,indexTime+4] / vTsub[,indexTime+3])
        vTsub[,indexTime+3] <- log(vTsub[,indexTime+3] / vTsub[,indexTime+2])
        vTsub[,indexTime+2] <- log(vTsub[,indexTime+2] / SR)
      }

      vT <- rbind(vT,vTsub)
    }
  }

  if (replyNormal==" Thomas & Kendall")
  {
    vT <- vowelScale
    
    for (i in (1:nPoints))
    {
      indexTime <- indexVowel + 2 + ((i-1)*5)
      
      vT[,indexTime+1] <- -1 * (vT[,indexTime+4] - vT[,indexTime+1])
      vT[,indexTime+2] <- -1 * (vT[,indexTime+4] - vT[,indexTime+2])
      vT[,indexTime+3] <- -1 * (vT[,indexTime+4] - vT[,indexTime+3])
    }
  }

  if (replyNormal==" Gerstman")
  {
    vT <- data.frame()
    
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1, FUN=mean)
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel      , data=vTLong1Ag , FUN=mean)

    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

      minF <- rep(0,4)
      maxF <- rep(0,4)
      
      for (j in 1:4)
      {
        minF[j] <- min(vTLong1AgSub[,j+2])
        maxF[j] <- max(vTLong1AgSub[,j+2])
      }
      
      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])
      
      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)
        
        for (j in (1:4))
        {
          vTsub[,j+indexTime] <- 999 * ((vTsub[,j+indexTime]-minF[j]) / (maxF[j]-minF[j]))
        }
      }
      
      vT <- rbind(vT,vTsub)
    }
  }

  if ((replyNormal==" Lobanov") & (nVoWel==1))
  {
    vT <- data.frame()
    vTLong1Ag <- vowelLong1
    
    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])
      
      meanF <- rep(0,4)
        sdF <- rep(0,4)
      
      for (j in 1:4)
      {
        meanF[j] <- mean(vTLong1AgSub[,j+3])
          sdF[j] <-   sd(vTLong1AgSub[,j+3])
      }
      
      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])
      
      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)
        
        for (j in (1:4))
        {
          vTsub[,j+indexTime] <- (vTsub[,j+indexTime]-meanF[j])/sdF[j]
        }
      }
      
      vT <- rbind(vT,vTsub)
    }
  }
  
  if ((replyNormal==" Lobanov") & (nVoWel> 1))
  {
    vT <- data.frame()
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1, FUN=mean)
    
    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])
      
      meanF <- rep(0,4)
        sdF <- rep(0,4)
      
      for (j in 1:4)
      {
        meanF[j] <- mean(vTLong1AgSub[,j+3])
          sdF[j] <-   sd(vTLong1AgSub[,j+3])
      }
      
      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])
      
      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)
        
        for (j in (1:4))
        {
          vTsub[,j+indexTime] <- (vTsub[,j+indexTime]-meanF[j])/sdF[j]
        }
      }
      
      vT <- rbind(vT,vTsub)
    }
  }

  if (replyNormal==" Watt & Fabricius")
  {
    vT <- data.frame()
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1, FUN=mean)

    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

      vowelF1 <- aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
      vowelF2 <- aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)

      iF1 <- min(vowelF1$f1)
      iF2 <- max(vowelF2$f2)

      uF1 <- min(vowelF1$f1)
      uF2 <- min(vowelF1$f1)

      aF1 <- max(vowelF1$f1)
      aF2 <- vowelF2$f2[which(vowelF1$f1 == aF1)]

      centroidF1 <- (iF1 + uF1 + aF1)/3
      centroidF2 <- (iF2 + uF2 + aF2)/3

      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])

      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)

        vTsub[,indexTime+2] <- vTsub[,indexTime+2] / centroidF1
        vTsub[,indexTime+3] <- vTsub[,indexTime+3] / centroidF2
      }

      vT <- rbind(vT,vTsub)
    }
  }

  if (replyNormal==" Fabricius et al.")
  {
    vT <- data.frame()
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1, FUN=mean)

    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

      vowelF1 <- aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
      vowelF2 <- aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)

      iF1 <- min(vowelF1$f1)
      iF2 <- max(vowelF2$f2)

      uF1 <- min(vowelF1$f1)
      uF2 <- min(vowelF1$f1)

      aF1 <- max(vowelF1$f1)

      centroidF1 <- (iF1 + uF1 + aF1)/3
      centroidF2 <- (iF2 + uF2      )/2

      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])

      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)

        vTsub[,indexTime+2] <- vTsub[,indexTime+2] / centroidF1
        vTsub[,indexTime+3] <- vTsub[,indexTime+3] / centroidF2
      }

      vT <- rbind(vT,vTsub)
    }
  }

  if (replyNormal==" Bigham")
  {
    vT <- data.frame()
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1, FUN=mean)
    
    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])
      
      vowelF1 <- aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
      vowelF2 <- aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)
      
      iF1 <- min(vowelF1$f1)
      iF2 <- max(vowelF2$f2)
      
      uF1 <- min(vowelF1$f1)
      uF2 <- min(vowelF2$f2)
      
      oF1 <- max(vowelF1$f1)
      oF2 <- min(vowelF2$f2)
      
      aF1 <- max(vowelF1$f1)

                          index <- which(vowelF2$vowel ==        intToUtf8("0x00E6"))
      if (!length(index)) index <- which(vowelF2$vowel == paste0(intToUtf8("0x00E6"), intToUtf8("0x02D1")))
      if (!length(index)) index <- which(vowelF2$vowel == paste0(intToUtf8("0x00E6"), intToUtf8("0x02D0")))
      if (!length(index)) index <- which(vowelF2$vowel ==        intToUtf8("0x0061"))
      if (!length(index)) index <- which(vowelF2$vowel == paste0(intToUtf8("0x0061"), intToUtf8("0x02D1")))
      if (!length(index)) index <- which(vowelF2$vowel == paste0(intToUtf8("0x0061"), intToUtf8("0x02D0")))
      if (!length(index)) index <- which(vowelF2$vowel ==        intToUtf8("0x025B"))
      if (!length(index)) index <- which(vowelF2$vowel == paste0(intToUtf8("0x025B"), intToUtf8("0x02D1")))
      if (!length(index)) index <- which(vowelF2$vowel == paste0(intToUtf8("0x025B"), intToUtf8("0x02D0")))
                          
      aF2 <- vowelF2$f2[index]

      centroidF1 <- (iF1 + uF1 + oF1 + aF1)/4
      centroidF2 <- (iF2 + uF2 + oF2 + aF2)/4
      
      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])
      
      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)
        
        vTsub[,indexTime+2] <- vTsub[,indexTime+2] / centroidF1
        vTsub[,indexTime+3] <- vTsub[,indexTime+3] / centroidF2
      }
      
      vT <- rbind(vT,vTsub)
    }
  }
  
  if (replyNormal==" Heeringa & Van de Velde I")
  {
    vT <- data.frame()
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1, FUN=mean)
    
    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])
      
      vowelF1 <- aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
      vowelF2 <- aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)
      
      k <- grDevices::chull(vowelF1$f1,vowelF2$f2)
      
      xx <- vowelF1$f1[k]
      xx[length(xx)+1] <- xx[1]
      yy <- vowelF2$f2[k]
      yy[length(yy)+1] <- yy[1]
      
      if ((length(xx)>=3) & (length(yy)>=3))
        pc <- poly_center(xx,yy)
      else
        pc <- c(mean(xx),mean(yy))
      
      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])
      
      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)
        
        vTsub[,indexTime+2] <- vTsub[,indexTime+2]/pc[1]
        vTsub[,indexTime+3] <- vTsub[,indexTime+3]/pc[2]
      }
      
      vT <- rbind(vT,vTsub)
    }
  }
  
  if (replyNormal==" Heeringa & Van de Velde II")
  {
    vT <- data.frame()
    vTLong1Ag <- aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1, FUN=mean)

    for (q in (1:nSpeaKer))
    {
      vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

      vowelF1 <- aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
      vowelF2 <- aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)

      k <- grDevices::chull(vowelF1$f1,vowelF2$f2)

      xx <- vowelF1$f1[k]
      xx[length(xx)+1] <- xx[1]
      yy <- vowelF2$f2[k]
      yy[length(yy)+1] <- yy[1]

      if ((length(xx)>=3) & (length(yy)>=3))
        pc <- poly_center(xx,yy)
      else
        pc <- c(mean(vowelF1$f1[k]),mean(vowelF2$f2[k]))
      
      xxi <- approx(1:length(xx), xx, n = 1000)$y
      yyi <- approx(1:length(yy), yy, n = 1000)$y
      
      xxi <- xxi[1:(length(xxi)-1)]
      yyi <- yyi[1:(length(yyi)-1)]

      xxg <- cut(xxi, breaks=10)
      yyg <- cut(yyi, breaks=10)

      ag <- aggregate(cbind(xxi,yyi)~xxg+yyg, FUN=mean)

      mean1 <- mean(ag$xxi)
      mean2 <- mean(ag$yyi)
      
      sd1 <- sd(ag$xxi)
      sd2 <- sd(ag$yyi)

      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])

      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)

        vTsub[,indexTime+2] <- (vTsub[,indexTime+2]-pc[1])/sd1
        vTsub[,indexTime+3] <- (vTsub[,indexTime+3]-pc[2])/sd2
      }

      vT <- rbind(vT,vTsub)
    }
  }

  if (replyNormal==" Nearey I")
  {
    vT <- data.frame()
    vTLong3Ag <- aggregate(f~speaker+vowel+point+formant, data=vowelLong4, FUN=mean)
    
    for (q in (1:nSpeaKer))
    {
      vTLong3AgSub  <- subset(vTLong3Ag, speaker==SpeaKer[q])
      speakerMean   <- aggregate(f~formant, data=vTLong3AgSub, FUN=mean)
      
      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])
      
      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)
        
        for (j in (1:4))
        {
          vTsub[,j+indexTime] <- log(vTsub[,j+indexTime]) - speakerMean$f[j]
        }
      }
      
      vT <- rbind(vT,vTsub)
    }
  }
  
  if (replyNormal==" Nearey II")
  {
    vT <- data.frame()
    vTLong4Ag <- aggregate(f~speaker+vowel+point+formant, data=vowelLong4, FUN=mean)
    
    for (q in (1:nSpeaKer))
    {
      vTLong4AgSub  <- subset(vTLong4Ag, speaker==SpeaKer[q])
      speakerMean   <- mean(vTLong4AgSub$f)
      
      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])
      
      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)
        
        for (j in (1:4))
        {
          vTsub[,j+indexTime] <- log(vTsub[,j+indexTime]) - speakerMean
        }
      }
      
      vT <- rbind(vT,vTsub)
    }
  }

  if (replyNormal==" Labov I")
  {
    vT <- data.frame()

    vTLong2Ag <- aggregate(f~speaker+vowel+point+formant, data=vowelLong2, FUN=psych::geometric.mean)
    grandMean <- psych::geometric.mean(vTLong2Ag$f)

    for (q in (1:nSpeaKer))
    {
      vTLong2AgSub  <- subset(vTLong2Ag, speaker==SpeaKer[q])
      speakerMean   <- psych::geometric.mean(vTLong2AgSub$f)
      speakerFactor <- exp(grandMean - speakerMean)

      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])

      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)

        for (j in (2:3))
        {
          vTsub[,j+indexTime] <- speakerFactor * vTsub[,j+indexTime]
        }
      }

      vT <- rbind(vT,vTsub)
    }
  }

  if (replyNormal==" Labov II")
  {
    vT <- data.frame()

    vTLong3Ag <- aggregate(f~speaker+vowel+point+formant, data=vowelLong3, FUN=psych::geometric.mean)
    grandMean <- psych::geometric.mean(vTLong3Ag$f)

    for (q in (1:nSpeaKer))
    {
      vTLong3AgSub  <- subset(vTLong3Ag, speaker==SpeaKer[q])
      speakerMean   <- psych::geometric.mean(vTLong3AgSub$f)
      speakerFactor <- exp(grandMean - speakerMean)

      vTsub <- subset(vowelScale, vowelScale[,1]==SpeaKer[q])

      for (i in (1:nPoints))
      {
        indexTime <- indexVowel + 2 + ((i-1)*5)

        for (j in (2:4))
        {
          vTsub[,j+indexTime] <- speakerFactor * vTsub[,j+indexTime]
        }
      }

      vT <- rbind(vT,vTsub)
    }
  }

  return(vT)
}

################################################################################

optionsScale <- function()
{
  options <- c("Hz" = " Hz",
               "bark: Schroeder et al. (1979)" = " bark I",
               "bark: Zwicker & Terhardt (1980)" = " bark II",
               "bark: Traunmueller (1990)" = " bark III",
               "ERB: Greenwood (1961)" = " ERB I",
               "ERB: Moore & Glasberg (1983)" = " ERB II",
               "ERB: Glasberg & Moore (1990)" = " ERB III",
               "ln" = " ln",
               "mel: Fant (1968)" = " mel I",
               "mel: O'Shaughnessy (1987)" = " mel II",
               "ST" = " ST")

  return(options)
}

optionsNormal <- function(vowelTab, replyScale, noSelF0, noSelF3)
{
  indexVowel <- grep("^vowel$", colnames(vowelTab))

  nonEmptyF0 <- (sum(vowelTab[,indexVowel+3]==0)!=nrow(vowelTab))
  nonEmptyF3 <- (sum(vowelTab[,indexVowel+6]==0)!=nrow(vowelTab))
  
  ###
  
  options1 <- c()
  
  if (nonEmptyF3 & noSelF3)
    options1 <- c(options1, "Peterson (1951)" = " Peterson")
  
  if (nonEmptyF3 & noSelF0 & (replyScale==" Hz"))
    options1 <- c(options1, "Sussman (1986)" = " Sussman")
  
  if (nonEmptyF0 & nonEmptyF3 & noSelF0 & noSelF3)
    options1 <- c(options1, "Syrdal & Gopal (1986)" = " Syrdal & Gopal")
  
  if (nonEmptyF0 & noSelF0 & (replyScale==" Hz"))
    options1 <- c(options1, "Miller (1989)" = " Miller")
  
  if (nonEmptyF3 & noSelF3)
    options1 <- c(options1, "Thomas & Kendall (2007)" = " Thomas & Kendall")
  
  ###
  
  options2 <- c()
  
  options2 <- c(options2, "Gerstman (1968)" = " Gerstman")
  
  ###
  
  options3 <- c()
  
  options3 <- c(options3, "Lobanov (1971)" = " Lobanov")
  
  if (noSelF0 & noSelF3)
    options3 <- c(options3, "Watt & Fabricius (2002)" = " Watt & Fabricius")
  
  if (noSelF0 & noSelF3)
    options3 <- c(options3, "Fabricius et al. (2009)" = " Fabricius et al.")
  
  if (noSelF0 & noSelF3)
    options3 <- c(options3, "Bigham (2008)" = " Bigham")
  
  if (noSelF0 & noSelF3)
    options3 <- c(options3, "Heeringa & Van de Velde (2021) I"  = " Heeringa & Van de Velde I" )
  
  if (noSelF0 & noSelF3)
    options3 <- c(options3, "Heeringa & Van de Velde (2021) II" = " Heeringa & Van de Velde II")
  
  ###
  
  options4 <- c()
  
  if (replyScale==" Hz")
    options4 <- c(options4, "Nearey (1978) I"  = " Nearey I" )
  
  if (nonEmptyF0 & nonEmptyF3 & (replyScale==" Hz"))
    options4 <- c(options4, "Nearey (1978) II" = " Nearey II")
  
  if (noSelF0 & noSelF3 & (replyScale==" Hz"))
    options4 <- c(options4, "Labov (2006) I"  = " Labov I" )
  
  if (nonEmptyF3 & noSelF0 & (replyScale==" Hz"))
    options4 <- c(options4, "Labov (2006) II" = " Labov II")

  ###

  return(c("None" = "", list(" Formant-ratio normalization"=options1,
                             " Range normalization"        =options2,
                             " Centroid normalization"     =options3,
                             " Log-mean normalization"     =options4)))
}

################################################################################

vowelNormD <- function(vowelTab,replyNormal)
{
  if ((is.null(vowelTab)) || (length(replyNormal)==0))
    return(NULL)

  indexVowel <- grep("^vowel$", colnames(vowelTab))

   SpeaKer <- unique(vowelTab[,1])
  nSpeaKer <- length(SpeaKer)

   VoWel   <- unique(vowelTab[,indexVowel])
  nVoWel   <- length(VoWel)

  if (replyNormal=="")
  {
    vT <- vowelTab
  }

  if ((replyNormal==" Lobanov") & (nVoWel==1))
  {
    vT <- data.frame()
    vTAg <- data.frame(vowelTab[,1],vowelTab[,indexVowel+1])

    for (q in (1:nSpeaKer))
    {
      vTAgSub <- subset(vTAg, vTAg[,1]==SpeaKer[q])

      meanD <- mean(vTAgSub[,2])
        sdD <-   sd(vTAgSub[,2])

      vTsub <- subset(vowelTab, vowelTab[,1]==SpeaKer[q])
      vTsub[,indexVowel+1] <- (vTsub[,indexVowel+1]-meanD)/sdD

      vT <- rbind(vT,vTsub)
    }
  }

  if ((replyNormal==" Lobanov") & (nVoWel> 1))
  {
    vT <- data.frame()
    vTAg <- aggregate(vowelTab[,indexVowel+1]~vowelTab[,1]+vowelTab[,indexVowel], FUN=mean)

    for (q in (1:nSpeaKer))
    {
      vTAgSub <- subset(vTAg, vTAg[,1]==SpeaKer[q])

      meanD <- mean(vTAgSub[,3])
        sdD <-   sd(vTAgSub[,3])

      vTsub <- subset(vowelTab, vowelTab[,1]==SpeaKer[q])
      vTsub[,indexVowel+1] <- (vTsub[,indexVowel+1]-meanD)/sdD

      vT <- rbind(vT,vTsub)
    }
  }

  return(vT)
}

################################################################################

speaker=NULL
indexColor=indexShape=indexPlot=NULL
X=Y=NULL
color=shape=vowel=plot=time=NULL
ll=ul=NULL
voweltime=NULL
xend=yend=NULL
V1=V2=NULL

################################################################################

visvow <- function()
{
  options(shiny.sanitize.errors = TRUE)
  options(shiny.usecairo=FALSE)
  options(shiny.maxRequestSize=20*1024^2)

  addResourcePath('www', system.file(package='visvow'))

  shinyApp(
    ui <- fluidPage(
      tags$style(type = 'text/css', '.title { margin-left: 20px; font-weight: bold; }'),
      tags$style(type = 'text/css', '.navbar-default > .container-fluid { margin-left: -11px; }'),
      tags$style(type = 'text/css', 'nav.navbar-default { margin-left: 15px; margin-right: 15px; }'),

      tags$style(type = 'text/css', 'p { margin-top: 0; margin-bottom: 0; }'),
      tags$style(type = 'text/css', 'h5 { margin-top: 0.3em; margin-bottom: 0.1em; }'),
      tags$style(type = 'text/css', 'h6 { margin-top: 1.0em; margin-bottom: 0.1em; }'),
      tags$style(type = 'text/css', 'li { margin-top: 0; margin-bottom: 0.4em; }'),

      tags$style(type = 'text/css', '.shiny-progress-container { position: absolute; left: 49% !important; width: 200px; margin-left: 100px; top: 40% !important; height: 100px; margin-top: 50px; z-index: 2000; }'),
      tags$style(type = 'text/css', '.shiny-progress .progress {position: absolute; left: 50% !important; width: 487px; margin-left: -419px; top: 50% !important; height: 16px; margin-top: 8px; }'),
      tags$style(type = 'text/css', '.shiny-progress .bar { background-color: #2081d4; .opacity = 0.8; }'),
      tags$style(type = 'text/css', '.shiny-progress .progress-text { position: absolute; right: 30px; height: 30px; width: 490px; background-color: #00AA00; padding-top: 2px; padding-right: 3px; padding-bottom: 2px; padding-left: 3px; opacity: 0.95; border-radius: 10px; -webkit-border-radius: 10px; -moz-border-radius: 10px; }'),
      tags$style(type = 'text/css', '.progress-text { top: 15px !important; color: #FFFFFF !important; text-align: center; }'),
      tags$style(type = 'text/css', '.shiny-progress .progress-text .progress-message { padding-top: 0px; padding-right: 3px; padding-bottom: 3px; padding-left: 10px; font-weight: bold; font-size: 18px; }'),
      tags$style(type = 'text/css', '.shiny-progress .progress-text .progress-detail { padding-top: 0px; padding-right: 3px; padding-bottom: 3px; padding-left: 3px; font-size: 17px; }'),

      img(src = "www/FA1.jpg", height = 60, align = "right", style = 'margin-right: 15px;'),
      titlePanel(title = HTML("<div class='title'>Visible Vowels<div>"), windowTitle = "Visible Vowels"),

      tags$head(
        tags$link(rel="icon", href="www/FA2.png"),

        tags$meta(charset="UTF-8"),
        tags$meta(name   ="description", content="Visible Vowels is a web app for the analysis of acoustic vowel measurements: f0, formants and duration. The app is an useful instrument for research in phonetics, sociolinguistics, dialectology, forensic linguistics, and speech-language pathology."),
      ),

      navbarPage
      (
        title=NULL, id = "navBar", collapsible = TRUE,

        tabPanel
        (
          title = "Load file",
          value = "load_file",

          fluidPage
          (
            style = "border: 1px solid silver; padding: 6px; min-height: 690px;",

            fluidPage
            (
              fileInput('vowelFile', 'Choose xlsx file',accept = c(".xlsx"), width="40%"),
              uiOutput('checkFormat')
            ),

            fluidPage
            (
              style = "font-size: 90%; white-space: nowrap;",
              align = "center",
              DT::dataTableOutput('vowelRound')
            )
          )
        ),

        tabPanel
        (
          title = "Contours",
          value = "contours",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("32%", "68%"),
            cellArgs = list(style = "padding: 6px"),

            column
            (
              width=12,

              textInput("title0", "Plot title", "", width="100%"),
              uiOutput('selScale0'),
              uiOutput('selRef0'),

              splitLayout
              (
                cellWidths = c("70%", "30%"),

                radioButtons("selError0", "Size of confidence intervals:", c("0%","90%","95%","99%"), selected = "0%", inline = TRUE),
                radioButtons("selMeasure0", "Use:", c("SD","SE"), selected = "SE", inline = TRUE)
              ),

              splitLayout
              (
                uiOutput('selVar0'),
                uiOutput('selLine0'),
                uiOutput('selPlot0')
              ),

              splitLayout
              (
                uiOutput('catXaxis0'),
                uiOutput('catLine0'),
                uiOutput('catPlot0')
              ),

              checkboxGroupInput("selGeon0", "Options:", c("average", "points", "smooth"), selected="points", inline=TRUE)
            ),

            column
            (
              width = 12,

              uiOutput("Graph0"),

              column
              (
                width = 11,

                splitLayout
                (
                  uiOutput('selFormat0a'),
                  downloadButton('download0a', 'Table'),

                  uiOutput('selSize0b'),
                  uiOutput('selFont0b'),
                  uiOutput('selPoint0b'),
                  uiOutput('selFormat0b'),
                  downloadButton('download0b', 'Graph')
                )
              )
            )
          )
        ),

        tabPanel
        (
          title = "Formants",
          value = "formants",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("32%", "68%"),
            cellArgs = list(style = "padding: 6px"),

            column
            (
              width=12,

              textInput("title1", "Plot title", "", width="100%"),
              uiOutput('selTimes1'),

              splitLayout
              (
                cellWidths = c("50%", "50%"),
                uiOutput('selScale1'),
                uiOutput('selNormal1')
              ),

              uiOutput('selTimesN'),

              splitLayout
              (
                uiOutput('selColor1'),
                uiOutput('selShape1'),
                uiOutput('selPlot1')
              ),

              splitLayout
              (
                uiOutput('catColor1'),
                uiOutput('catShape1'),
                uiOutput('catPlot1')
              ),

              uiOutput('selGeon1'),
              uiOutput('selPars' ),

              splitLayout
              (
                cellWidths = c("28%", "26%", "46%"),
                checkboxInput("grayscale1", "grayscale"         , FALSE),
                checkboxInput("average1"  , "average"           , FALSE),
                checkboxInput("ltf1"      , "long-term formants", FALSE)
              )
            ),

            column
            (
              width=12,

              splitLayout
              (
                cellWidths = c("85%", "15%"),

                uiOutput("Graph1"),

                column
                (
                  width=12,

                  br(),br(),

                  selectInput('axisX', "x-axis", choices=c("F1","F2","F3","--"), selected="F2", selectize=FALSE, width = "100%"),
                  selectInput('axisY', "y-axis", choices=c("F1","F2","F3","--"), selected="F1", selectize=FALSE, width = "100%"),
                  selectInput('axisZ', "z-axis", choices=c("F1","F2","F3","--"), selected="--", selectize=FALSE, width = "100%"),

                  uiOutput('manScale'),
                  uiOutput('selF1min'),
                  uiOutput('selF1max'),
                  uiOutput('selF2min'),
                  uiOutput('selF2max')
                )
              ),

              column
              (
                width = 11,

                splitLayout
                (
                  uiOutput('selFormat1a'),
                  downloadButton('download1a', 'Table'),

                  uiOutput('selSize1b'),
                  uiOutput('selFont1b'),
                  uiOutput('selPoint1b'),
                  uiOutput('selFormat1b'),
                  downloadButton('download1b', 'Graph')
                )
              )
            )
          )
        ),

        tabPanel
        (
          title = "Dynamics",
          value = "dynamics",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("32%", "68%"),
            cellArgs = list(style = "padding: 6px"),

            column
            (
              width=12,

              textInput("title4", "Plot title", "", width="100%"),

              splitLayout
              (
                cellWidths = c("50%", "49%"),
                uiOutput('selScale4'),
                uiOutput('selMethod4')
              ),

              uiOutput('selGraph4'),

              splitLayout
              (
                cellWidths = c("70%", "30%"),

                radioButtons("selError4", "Size of confidence intervals:", c("0%","90%","95%","99%"), selected = "95%", inline = TRUE),
                radioButtons("selMeasure4", "Use:", c("SD","SE"), selected = "SE", inline = TRUE)
              ),

              splitLayout
              (
                cellWidths = c("21%", "26%", "26%", "26%"),
                uiOutput('selVar4'),
                uiOutput('selXaxis4'),
                uiOutput('selLine4'),
                uiOutput('selPlot4')
              ),

              splitLayout
              (
                cellWidths = c("21%", "26%", "26%", "26%"),
                uiOutput('selTimes4'),
                uiOutput('catXaxis4'),
                uiOutput('catLine4'),
                uiOutput('catPlot4')
              ),

              checkboxGroupInput("selGeon4", "Options:", c("average", "rotate x-axis labels"), inline=TRUE)
            ),

            column
            (
              width = 12,

              uiOutput("Graph4"),

              column
              (
                width = 11,

                splitLayout
                (
                  uiOutput('selFormat4a'),
                  downloadButton('download4a', 'Table'),

                  uiOutput('selSize4b'),
                  uiOutput('selFont4b'),
                  uiOutput('selPoint4b'),
                  uiOutput('selFormat4b'),
                  downloadButton('download4b', 'Graph')
                )
              )
            )
          )
        ),

        tabPanel
        (
          title = "Duration",
          value = "duration",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("32%", "68%"),
            cellArgs = list(style = "padding: 6px"),

            column
            (
              width=12,

              textInput("title2", "Plot title", "", width="100%"),
              uiOutput('selNormal2'),
              uiOutput('selGraph2'),

              splitLayout
              (
                cellWidths = c("70%", "30%"),

                radioButtons("selError2", "Size of confidence intervals:", c("0%","90%","95%","99%"), selected = "95%", inline = TRUE),
                radioButtons("selMeasure2", "Use:", c("SD","SE"), selected = "SE", inline = TRUE)
              ),

              splitLayout
              (
                uiOutput('selXaxis2'),
                uiOutput('selLine2'),
                uiOutput('selPlot2')
              ),

              splitLayout
              (
                uiOutput('catXaxis2'),
                uiOutput('catLine2'),
                uiOutput('catPlot2')
              ),

              checkboxGroupInput("selGeon2", "Options:", c("average", "rotate x-axis labels"), inline=TRUE)
            ),

            column
            (
              width = 12,

              uiOutput("Graph2"),

              column
              (
                width = 11,

                splitLayout
                (
                  uiOutput('selFormat2a'),
                  downloadButton('download2a', 'Table'),

                  uiOutput('selSize2b'),
                  uiOutput('selFont2b'),
                  uiOutput('selPoint2b'),
                  uiOutput('selFormat2b'),
                  downloadButton('download2b', 'Graph')
                )
              )
            )
          )
        ),

        tabPanel
        (
          title = "Explore",
          value = "explore",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("32%", "68%"),
            cellArgs = list(style = "padding: 6px"),

            column
            (
              width=12,

              textInput("title3", "Plot title", "", width="100%"),
              uiOutput('selTimes3'),

              splitLayout
              (
                cellWidths = c("50%", "50%"),
                checkboxGroupInput("selFormant3", "Include formants:", c("F1","F2","F3"), selected=c("F1","F2"), TRUE),
                radioButtons('selMetric3', 'Metric:', c("Euclidean","Accdist"), selected = "Euclidean", TRUE)
              ),

              splitLayout
              (
                cellWidths = c("50%", "50%"),
                uiOutput('selScale3'),
                uiOutput('selNormal3')
              ),

              uiOutput('selTimesN3'),

              splitLayout
              (
                uiOutput('selVowel3'),
                uiOutput('selGrouping3'),
                uiOutput('catGrouping3')
              ),

              uiOutput('exclVow3'),

              radioButtons('selClass3', 'Explorative method:', c("Cluster analysis","Multidimensional scaling"), selected = "Cluster analysis", TRUE),
              uiOutput('selMethod3'),
              uiOutput('explVar3'),

              uiOutput("selGeon3"),

              splitLayout
              (
                checkboxInput("grayscale3", "grayscale", FALSE),
                checkboxInput("summarize3" ,"summarize", FALSE)
              )
            ),

            column
            (
              width = 12,

              uiOutput("Graph3"),

              column
              (
                width = 11,

                splitLayout
                (
                  uiOutput('selFormat3a'),
                  downloadButton('download3a', 'Table'),

                  uiOutput('selSize3b'),
                  uiOutput('selFont3b'),
                  uiOutput('selPoint3b'),
                  uiOutput('selFormat3b'),
                  downloadButton('download3b', 'Graph')
                )
              )
            )
          )
        ),

        tabPanel
        (
          title = "Evaluate",
          value = "evaluate",

          splitLayout
          (
            style = "border: 1px solid silver; min-height: 690px;",
            cellWidths = c("32%", "68%"),
            cellArgs = list(style = "padding: 6px"),

            column
            (
              width=12,

              fluidPage(
                style = 'border: 1px solid silver; margin-top: 7px; padding-top: 4px; padding-bottom: 4px;',
                align = "center",
                actionLink("buttonHelp5", label="", icon=icon("info-circle", lib = "font-awesome"), style='color: #2c84d7; font-size: 180%; margin-left: 4px;')
              ),

              br(),

              uiOutput('selTimes5'),
              uiOutput('selTimesN5'),

              splitLayout(
                uiOutput('selVars51'),
                uiOutput('selVars52')
              ),

              uiOutput("selF035"),
              uiOutput('exclVow5'),

              br(),

              uiOutput('goButton')
            ),

            column
            (
              width = 12,
              uiOutput("Graph5"),

              hr(style='border-top: 1px solid #cccccc;'),

              splitLayout
              (
                radioButtons(inputId  = 'selMeth5',
                             label    = 'Choose:',
                             choices  = c("Evaluate",
                                          "Compare"),
                             selected =   "Evaluate",
                             inline   = FALSE),

                uiOutput("getOpts5"),
                uiOutput("getEval5")
              )
            )
          )
        ),

        navbarMenu("More",

        tabPanel
        (
          title = "Help",
          value = "help",

          fluidPage
          (
            style = "border: 1px solid silver;",

            br(),
            h5(strong("About")),
            p("Visible Vowels is a web app for the analysis of acoustic vowel measurements: f0, formants and duration. The app is an useful instrument for research in phonetics, sociolinguistics, dialectology, forensic linguistics, and speech-language pathology. The program has been developed at the Fryske Akademy (Leeuwarden, The Netherlands) by Wilbert Heeringa under supervision of Hans Van de Velde. Visible Vowels is still under development. Comments are welcome and can be sent to", img(src = 'www/email.png', height = 20, align = "center"),"."),
            br(),
            h5(strong("System requirements")),
            p("Visible Vowels runs best on a computer with a monitor with a minimum resolution of 1370 x 870 (width x height). The use of Mozilla Firefox as a web browser is to be preferred."),
            br(),
            h5(strong("Format")),
            p("The input file should be a spreadsheet that is created in Excel or LibreOffice. It should be saved as an Excel 2007/2010/2013 XML file, i.e. with extension '.xlsx'. An example is schematically shown below."),
            br(),

            tags$div(tags$ul
            (
              tags$li(tags$span(HTML("<span style='color:blue'>Speakers</span>"),p("The first column should contain the speaker labels. Choose 'speaker' as column name. In our example there are three speakers labeled as 'A', 'B' and 'C'. This column is obligatory."))),
              tags$li(tags$span(HTML("<span style='color:blue'>Vowels</span>"),p("A column that contains the vowel labels should follow. For this column choose 'vowel' as column name. In our example each of the speakers pronounced four different vowels: i\u02D0, \u025B, a\u02D0 and \u0254. Although in this table each vowel occurs just one time per speaker, multiple pronunciations are possible. In case you want to use IPA characters (as in the example), enter them as Unicode characters. In order to find Unicode IPA characters, use the online IPA Chart Keyboard of Weston Ruter at http://westonruter.github.io/ipa-chart/keyboard/. This column is obligatory."))),
              tags$li(tags$span(HTML("<span style='color:blue'>Categorical variables</span>"),p("An arbitrary number of columns representing categorical variables such as location, language, gender, age group, etc. may follow, but is not obligatory. See to it that each categorical variable has an unique set of different values. Prevent the use of numbers, rather use meaningful codes. For example, rather then using codes '1' and '2' for a variable 'age group' use 'old' and 'young' or 'o' and 'y'."))),
              tags$li(tags$span(HTML("<span style='color:blue'>Duration</span>"),p("A column which contains the durations of the vowels should follow, with 'duration' as column name. The measurements may be either in seconds or milliseconds. This column is obligatory, but may be empty."))),
              tags$li(tags$span(HTML("<span style='color:blue'>Spectral variabels</span>"),p("Finally, a set of five columns should follow: 'time', f0', 'F1', 'F2' and 'F3'. The variable 'time' gives the time point within the vowel interval in seconds or milliseconds, i.e. it is assumed that the vowel interval starts at 0 (milli)seconds. The f0, F1, F2 and F3 should be measured at the time given in the column 'time'. The program assumes that they are measured in Hertz and not normalized. The set of five columns may be repeated as ",em("many times"), " as the user wishes, but should occur at least one time. For each repetition the same column names may be used. In the example table below f0, F1, F2 and F3 are given for two different time points, hence the set of five columns comprising 'time', 'f0', 'F1', 'F2' and 'F3' occurs twice. A set should always include all five columns, but the columns 'time', 'f0' and 'F3' may be empty.")))
            )),

            br(),

            div(img(src = 'www/format.png', height=330), style="margin-left: 26px;"),
            br(), br(),
            h5(strong("Example input file")),
            p("In order to try Visible Vowels an example spreadsheet can be downloaded ", a("here", href = "www/example.xlsx", target = "_blank"), "and be loaded by this program."),
            br(),
            h5(strong("Graphs")),
            p("Graphs can be saved in six formats: JPG, PNG, SVG, EPS, PDF and TEX. TEX files are created with TikZ. When using this format, it is assumed that XeLaTeX is installed. Generating a TikZ may take a long time. When including a TikZ file in a LaTeX document, you need to use a font that supports the IPA Unicode characters, for example: 'Doulos SIL', 'Charis SIL' or 'Linux Libertine O'. You also need to adjust the left margin and the scaling of the graph. The LaTeX document should be compiled with", code("xelatex"), ". Example of a LaTeX file in which a TikZ file is included:"),
            br(),
        
            code(style="margin-left: 36px;", "\\documentclass{minimal}"),
            br(), br(),
            code(style="margin-left: 36px;", "\\usepackage{tikz}"),
            br(),
            code(style="margin-left: 36px;", "\\usepackage{fontspec}"),
            br(),
            code(style="margin-left: 36px;", "\\setmainfont{Linux Libertine O}"),
            br(), br(),
            code(style="margin-left: 36px;", "\\begin{document}"),
            br(),
            code(style="margin-left: 36px;", "{\\hspace*{-3cm}\\scalebox{0.8}{\\input{formantPlot.TEX}}}"),
            br(),
            code(style="margin-left: 36px;", "\\end{document}"),
            br(), br(), br(),
            
            h5(strong("Implementation")),
            p("This program is implemented as a Shiny app. Shiny was developed by RStudio. This app uses the following R packages:"),
            br(),

            tags$div(tags$ul
            (
              tags$li(tags$span(HTML("<span style='color:blue'>base</span>"),p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/"))),
              tags$li(tags$span(HTML("<span style='color:blue'>shiny</span>"),p("Winston Chang, Joe Cheng, J.J. Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version 1.0.0. https://CRAN.R-project.org/package=shiny"))),
              tags$li(tags$span(HTML("<span style='color:blue'>shinyBS</span>"),p("Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61. https://CRAN.R-project.org/package=shinyBS"))),
              tags$li(tags$span(HTML("<span style='color:blue'>splitstackshape</span>"),p("Ananda Mahto (2019). splitstackshape: Stack and Reshape Datasets After Splitting Concatenated Values. R package version 1.4.8. https://CRAN.R-project.org/package=splitstackshape"))),
              tags$li(tags$span(HTML("<span style='color:blue'>stats</span>"),p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/"))),
              tags$li(tags$span(HTML("<span style='color:blue'>tydr</span>"),p("Hadley Wickham and Lionel Henry (2019). tidyr: Tidy Messy Data. R package version 1.0.0. https://CRAN.R-project.org/package=tidyr"))),
              tags$li(tags$span(HTML("<span style='color:blue'>PBSmapping</span>"),p("Jon T. Schnute, Nicholas Boers and Rowan Haigh (2019). PBSmapping: Mapping Fisheries Data and Spatial Analysis Tools. R package version 2.72.1. https://CRAN.R-project.org/package=PBSmapping"))),
              tags$li(tags$span(HTML("<span style='color:blue'>formattable</span>"),p("Kun Ren and Kenton Russell (2016). formattable: Create 'Formattable' Data Structures. R package version 0.2.0.1. https://CRAN.R-project.org/package=formattable"))),
              tags$li(tags$span(HTML("<span style='color:blue'>ggplot2</span>"),p("H. Wickham (2009). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. http://ggplot2.org"))),
              tags$li(tags$span(HTML("<span style='color:blue'>plot3D</span>"),p("Karline Soetaert (2017). plot3D: Plotting Multi-Dimensional Data. R package version 1.1.1. https://CRAN.R-project.org/package=plot3D"))),
              tags$li(tags$span(HTML("<span style='color:blue'>MASS</span>"),p("W.N. Venables & B.D. Ripley (2002). Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0"))),
              tags$li(tags$span(HTML("<span style='color:blue'>ggdendro</span>"),p("Andrie de Vries and Brian D. Ripley (2016). ggdendro: Create Dendrograms and Tree Diagrams Using 'ggplot2'. R package version 0.1-20. https://CRAN.R-project.org/package=ggdendro"))),
              tags$li(tags$span(HTML("<span style='color:blue'>ggrepel</span>"),p("Kamil Slowikowski (2017). ggrepel: Repulsive Text and Label Geoms for 'ggplot2'. R package version 0.7.0. https://CRAN.R-project.org/package=ggrepel"))),
              tags$li(tags$span(HTML("<span style='color:blue'>readxl</span>"),p("Hadley Wickham and Jennifer Bryan (2017). readxl: Read Excel Files. R package version 1.0.0. https://CRAN.R-project.org/package=readxl"))),
              tags$li(tags$span(HTML("<span style='color:blue'>WriteXLS</span>"),p("Marc Schwartz and various authors. (2015). WriteXLS: Cross-Platform Perl Based R Function to Create Excel 2003 (XLS) and Excel 2007 (XLSX) Files. R package version 4.0.0. https://CRAN.R-project.org/package=WriteXLS"))),
              tags$li(tags$span(HTML("<span style='color:blue'>DT</span>"),p("Yihui Xie (2016). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.2. https://CRAN.R-project.org/package=DT"))),
              tags$li(tags$span(HTML("<span style='color:blue'>psych</span>"),p("W. Revelle (2016). psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, Version = 1.6.12, https://CRAN.R-project.org/package=psych"))),
              tags$li(tags$span(HTML("<span style='color:blue'>pracma</span>"),p("Hans Werner Borchers (2017). pracma: Practical Numerical Math Functions. R package version 1.9.9. https://CRAN.R-project.org/package=pracma"))),
              tags$li(tags$span(HTML("<span style='color:blue'>Rtsne</span>"),p("Jesse H. Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor Embedding using a Barnes-Hut Implementation, URL: https://github.com/jkrijthe/Rtsne"),p("L.J.P. van der Maaten and G.E. Hinton (2008). Visualizing High-Dimensional Data Using t-SNE. Journal of Machine Learning Research 9(Nov):2579-2605"),p("L.J.P. van der Maaten (2014). Accelerating t-SNE using Tree-Based Algorithms. Journal of Machine Learning Research 15(Oct):3221-3245"))),
              tags$li(tags$span(HTML("<span style='color:blue'>plyr</span>"),p("Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29. URL http://www.jstatsoft.org/v40/i01/"))),
              tags$li(tags$span(HTML("<span style='color:blue'>grid</span>"),p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/"))),
              tags$li(tags$span(HTML("<span style='color:blue'>ggsave_autosize</span>"),p("Z. Lin (GovTech, Singapore) developed this function for autocropping faceted plots made by using ggplot2"))),
              tags$li(tags$span(HTML("<span style='color:blue'>svglite</span>"),p("Hadley Wickham, Lionel Henry, T Jake Luciani, Matthieu Decorde and Vaudor Lise (2016). svglite: An 'SVG' Graphics Device. R package version 1.2.0. https://CRAN.R-project.org/package=svglite"))),
              tags$li(tags$span(HTML("<span style='color:blue'>Cairo</span>"),p("Simon Urbanek and Jeffrey Horner (2015). Cairo: R graphics device using cairo graphics library for creating high-quality bitmap (PNG, JPEG, TIFF),  vector (PDF, SVG, PostScript) and display (X11 and Win32) output. R package version 1.5-9. https://CRAN.R-project.org/package=Cairo"))),
              tags$li(tags$span(HTML("<span style='color:blue'>tikzDevice</span>"),p("Charlie Sharpsteen and Cameron Bracken (2020). tikzDevice: R Graphics Output in LaTeX Format. R package version 0.12.3.1. https://CRAN.R-project.org/package=tikzDevice"))),
              tags$li(tags$span(HTML("<span style='color:blue'>shinybusy</span>"),p("Fanny Meyer and Victor Perrier (2020). shinybusy: Busy Indicator for 'Shiny' Applications. R package version 0.2.2. https://CRAN.R-project.org/package=shinybusy")))
            )),

            br(),
            p("Visible Vowels allows to convert and normalize vowel data and calculate some specific metrics. To get all the details on how these values are calculated, type ", span(style="font-family: monospace; font-size: 100%;", 'vignette("visvow")'), " in the R console."),
            br(),
            h5(strong("How to cite this app")),
            p("Heeringa, W. & Van de Velde, H. (2018). \u201CVisible Vowels: a Tool for the Visualization of Vowel Variation.\u201D In ",tags$i("Proceedings CLARIN Annual Conference 2018, 8 - 10 October, Pisa, Italy."),"CLARIN ERIC."),
            br()
          ),

          br()
        ),

        tabPanel
        (
          title = "Disclaimer",
          value = "disclaimer",

          fluidPage
          (
            style = "border: 1px solid silver; min-height: 690px;",

            br(),
            h5(strong("Liability")),
            p("This app is provided 'as is' without warranty of any kind, either express or implied, including, but not limited to, the implied warranties of fitness for a purpose, or the warranty of non-infringement. Without limiting the foregoing, the Fryske Akademy makes no warranty that: 1) the app will meet your requirements, 2) the app will be uninterrupted, timely, secure or error-free, 3) the results that may be obtained from the use of the app will be effective, accurate or reliable, 4) the quality of the app will meet your expectations, 5) any errors in the app will be corrected."),
            br(),
            p("The app and its documentation could include technical or other mistakes, inaccuracies or typographical errors. The Fryske Akademy may make changes to the app or documentation made available on its web site. The app and its documentation may be out of date, and the Fryske Akademy makes no commitment to update such materials."),
            br(),
            p("The Fryske Akademy assumes no responsibility for errors or ommissions in the app or documentation available from its web site."),
            br(),
            p("In no event shall the Fryske Akademy be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the Fryske Akademy has been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this software."),
            br(),
            p("The use of the app is done at your own discretion and risk and with agreement that you will be solely responsible for any damage to your computer system or loss of data that results from such activities. No advice or information, whether oral or written, obtained by you from the Fryske Akademy shall create any warranty for the software."),
            br(),
            h5(strong("Other")),
            p("The disclaimer may be changed from time to time."),
            br()
          )
        ))
      )
    ),

    ############################################################################

    server <- function(input, output, session)
    {
      observeEvent(input$navBar,
      {
        if (getUrlHash() == paste0("#", input$navBar)) return()
        updateQueryString(paste0("#", input$navBar), mode = "push")
      })

      observeEvent(getUrlHash(),
      {
        Hash <- getUrlHash()
        if (Hash == paste0("#", input$navBar)) return()
        Hash <- gsub("#", "", Hash)
        updateNavbarPage(session, "navBar", selected=Hash)
      })

      ##########################################################################

      vowelFile <- reactive(
      {
        inFile <- input$vowelFile

        if (is.null(inFile))
          return(NULL)

        file.rename(inFile$datapath, paste0(inFile$datapath,".xlsx"))
        return(as.data.frame(read_excel(paste0(inFile$datapath,".xlsx"), 1, .name_repair = "minimal")))
      })

      vowelTab <- reactive(
      {
        if (is.null(vowelFile()) || (Check()!="OK"))
          return(NULL)

        vT <- vowelFile()

        indexDuration <- grep("^duration$", tolower(colnames(vT)))

        if (indexDuration > 3)
        {
          cnames <- colnames(vT)
          vT <- data.frame(vT[,1], vT[,3:(indexDuration-1)], vT[,2], vT[,indexDuration:ncol(vT)])
          cnames <- c(cnames[1],cnames[3:(indexDuration-1)],cnames[2],cnames[indexDuration:ncol(vT)])
          colnames(vT) <- cnames
        }
        else {}

        indexVowel <- grep("^vowel$", tolower(trimws(colnames(vT), "r")))

        for (k in (1:(indexVowel+1)))
        {
          colnames(vT)[k] <- tolower(trimws(colnames(vT)[k], "r"))
        }

        k0 <- 0
        for (k in ((indexVowel+2):(ncol(vT))))
        {
          k0 <- k0 +1

          if (((k0 %% 5)==1) | ((k0 %% 5)==2))
          {
            colnames(vT)[k] <- tolower(colnames(vT)[k])
          }
          else
          {
            colnames(vT)[k] <- toupper(colnames(vT)[k])
          }
        }

        if (length(indexVowel)>0)
        {
          for (i in ((indexVowel+1):(ncol(vT))))
          {
            if (is.character(vT[,i]))
            {
              vT[,i] <- as.numeric(vT[,i])
            }

            if (sum(is.na(vT[,i]))==nrow(vT))
            {
              vT[,i] <- 0
            }
          }

          vT <- vT[rowSums(is.na(vT)) == 0,]
        }

        return(vT)
      })

      Check <- reactive(
      {
        if (is.null(vowelFile()))
          return(NULL)

        indexVowel <- grep("^duration$", tolower(colnames(vowelFile()))) - 1

        if (length(grep("^speaker$", tolower(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'speaker' not found."
        else

        if (tolower(colnames(vowelFile())[1])!="speaker")
          Message <- "Column 'speaker' should be first column."
        else

        if (sum(is.na(vowelFile()[,1]))==nrow(vowelFile()))
          Message <- "Column 'speaker' is empty."
        else

        if (length(grep("^vowel$", tolower(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'vowel' not found."
        else

        if (tolower(colnames(vowelFile())[2])!="vowel")
          Message <- "Column 'vowel' should be second column."
        else

        if (sum(is.na(vowelFile()[,indexVowel]))==nrow(vowelFile()))
          Message <- "Column 'vowel' is empty."
        else

        if (length(grep("^duration", tolower(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'duration' not found."
        else

        if (length(grep("^time", tolower(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'time' not found."
        else

        if (length(grep("^f0", tolower(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'f0' not found."
        else

        if (length(grep("^F1", toupper(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'F1' not found."
        else

        if (length(grep("^F2", toupper(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'F2' not found."
        else

        if (length(grep("^F3", toupper(colnames(vowelFile())), value = TRUE)) == 0)
          Message <- "Column 'F3' not found."
        else
          Message <- "OK"

        return(Message)
      })

      output$checkFormat <- renderUI(
      {
        if (is.null(vowelFile()))
          return(NULL)

        if (Check()!="OK")
          return(tags$div(HTML(paste0("<font color='red'>",Check(),"</font>"))))
        else {}
      })

      Round <- function(x)
      {
        return(trunc(x+0.5))
      }

      vowelRound <- reactive(
      {
        if (is.null(vowelFile()))
          return(NULL)

        vT <- vowelFile()

        nColumns <- ncol(vT)

        for (i in (1:nColumns))
        {
          if (grepl("^duration",tolower(colnames(vT)[i])) |
              grepl("^time"    ,tolower(colnames(vT)[i])) |
              grepl("^f0"      ,tolower(colnames(vT)[i])) |
              grepl("^F1"      ,toupper(colnames(vT)[i])) |
              grepl("^F2"      ,toupper(colnames(vT)[i])) |
              grepl("^F3"      ,toupper(colnames(vT)[i])))
          {
            if (is.character(vT[,i]))
            {
              vT[,i] <- as.numeric(vT[,i])
            }

            if (sum(is.na(vT[,i]))<nrow(vT))
            {
              if (max(vT[,i], na.rm = TRUE)<=1)
              {
                vT[,i] <- round(((Round(vT[,i]*1000))/1000),3)
              }
              else
              {
                vT[,i] <- Round(vT[,i])
              }
            }
          }
        }

        return(vT)
      })

      output$vowelRound <- DT::renderDataTable(expr = vowelRound(), options = list(scrollX = TRUE))

      vowelExcl <- reactive(
      {
        if (is.null(vowelTab()) || (nrow(vowelTab())==0))
          return(NULL)

        vowels   <- unique(vowelTab()$vowel)
        vowels0  <- unique(vowelTab()$vowel)
        speakers <- unique(vowelTab()$speaker)

        for (i in 1:length(speakers))
        {
          vTsub  <- subset(vowelTab(), speaker==speakers[i])
          vowels <- intersect(vowels,unique(vTsub$vowel))
        }

        return(setdiff(vowels0,vowels))
      })

      vowelSame <- reactive(
      {
        if (is.null(vowelTab()) || (nrow(vowelTab())==0))
          return(NULL)

        if (length(vowelExcl())==0)
          return(vowelTab())
        else
          return(subset(vowelTab(), !is.element(vowelTab()$vowel,vowelExcl())))
      })

      ##########################################################################

      vowelScale0 <- reactive(
      {
        if ((length(input$replyRef0)==0) || is.na(input$replyRef0))
          Ref <- 0
        else
          Ref <- input$replyRef0

        return(vowelScale(vowelTab(),input$replyScale0,Ref))
      })

      fuseCols <- function(vT,replyValue)
      {
        columns <- ""

        if (length(replyValue)>0)
        {
          for (i in (1:length(replyValue)))
          {
            indexValue <- grep(paste0("^",as.character(replyValue)[i],"$"), colnames(vT))

            if (i==1)
              columns <- paste0(columns,vT[,indexValue])
            else
              columns <- paste (columns,vT[,indexValue])
          }
        }

        return(columns)
      }

      getTimeCode <- reactive(
      {
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        percentages <-FALSE

        if (sum(is.na(vowelTab()[,indexVowel + 1]))!=nrow(vowelTab()))
        {
          meanDuration <- mean(vowelTab()[,indexVowel+1])

          if (mean(vowelTab()[,indexVowel+2+((nPoints-1)*5)]) <= meanDuration)
          {
            if (meanDuration==0)
            {
              meanDuration <- 0.000001
            }

            timeLabel <- c()
            timeCode  <- c()

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              timeLabel[i] <- (mean(vowelTab()[,indexTime])/meanDuration) * 100
              timeCode [i] <- i

              names(timeCode) <- as.character(round(timeLabel))
            }

            percentages <-TRUE
          }
        }

        if (percentages==FALSE)
        {
          timeCode <- seq(from=1, to=nPoints, by=1)
          names(timeCode) <- as.character(timeCode)
        }

        return(timeCode)
      })

      vowelSub0 <- reactive(
      {
        if (is.null(vowelScale0()) || (nrow(vowelScale0())==0) || (length(input$catXaxis0)==0))
          return(NULL)

        vT <- vowelScale0()

        vT$indexPlot <- fuseCols(vowelScale0(),input$replyPlot0)
        vT$indexLine <- fuseCols(vowelScale0(),input$replyLine0)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))

        nColumns   <- ncol(vowelTab())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        xi <- as.numeric(input$catXaxis0)
        xn <- names(getTimeCode())[xi]

        if (input$replyVar0=="f0")
          varIndex <- 1
        else
        if (input$replyVar0=="F1")
          varIndex <- 2
        else
        if (input$replyVar0=="F2")
          varIndex <- 3
        else
        if (input$replyVar0=="F3")
          varIndex <- 4
        else
          return(NULL)

        if (input$selError0=="0%")
          z <- 0
        if (input$selError0=="90%")
          z <- 1.645
        if (input$selError0=="95%")
          z <- 1.96
        if (input$selError0=="99%")
          z <- 2.575

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          x <- c()
          y <- c()
          s <- c()
          v <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+varIndex])
              s <- c(s,as.character(vT$speaker))
              v <- c(v,as.character(vT$vowel))
            }
          }

          vT0 <- data.frame(x,s,v,y)

          if (is.element("average",input$selGeon0))
          {
            vT0 <- aggregate(y~x+s+v, data=vT0, FUN=mean)
            vT0 <- aggregate(y~x+  v, data=vT0, FUN=mean)
          }

          ag    <- aggregate(y~x, data=vT0, FUN=mean)
          ag$sd <- aggregate(y~x, data=vT0, FUN=sd)[,2]
          ag$n  <- aggregate(y~x, data=vT0, FUN=length)[,2]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure0=="SD")
          {
            ag$ll <- ag[,2] - z * ag$sd
            ag$ul <- ag[,2] + z * ag$sd
          }
          if (input$selMeasure0=="SE")
          {
            ag$ll <- ag[,2] - z * ag$se
            ag$ul <- ag[,2] + z * ag$se
          }

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- input$replyVar0

          return(ag)
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          vT <- subset(vT, is.element(vT$indexPlot,input$catPlot0))
          vT$indexPlot <- as.character(vT$indexPlot)

          if (nrow(vT)==0)
            return(data.frame())

          x <- c()
          y <- c()
          p <- c()
          s <- c()
          v <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+varIndex])
              p <- c(p,vT$indexPlot)
              s <- c(s,as.character(vT$speaker))
              v <- c(v,as.character(vT$vowel))
            }
          }

          vT0 <- data.frame(x,p,s,v,y)

          if (is.element("average",input$selGeon0))
          {
            vT0 <- aggregate(y~x+p+s+v, data=vT0, FUN=mean)
            vT0 <- aggregate(y~x+p+  v, data=vT0, FUN=mean)
          }

          ag    <- aggregate(y~x+p, data=vT0, FUN=mean)
          ag$sd <- aggregate(y~x+p, data=vT0, FUN=sd)[,3]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(y~x+p, data=vT0, FUN=length)[,3]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure0=="SD")
          {
            ag$ll <- ag[,3] - z * ag$sd
            ag$ul <- ag[,3] + z * ag$sd
          }
          if (input$selMeasure0=="SE")
          {
            ag$ll <- ag[,3] - z * ag$se
            ag$ul <- ag[,3] + z * ag$se
          }

          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- paste(input$replyPlot0, collapse = " ")
          colnames(ag)[3] <- input$replyVar0

          return(ag)
        }
        else

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          vT <- subset(vT, is.element(vT$indexLine,input$catLine0))
          vT$indexLine <- as.character(vT$indexLine)

          if (nrow(vT)==0)
            return(data.frame())

          x <- c()
          y <- c()
          l <- c()
          s <- c()
          v <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+varIndex])
              l <- c(l,vT$indexLine)
              s <- c(s,as.character(vT$speaker))
              v <- c(v,as.character(vT$vowel))
            }
          }

          vT0 <- data.frame(x,l,s,v,y)

          if (is.element("average",input$selGeon0))
          {
            vT0 <- aggregate(y~x+l+s+v, data=vT0, FUN=mean)
            vT0 <- aggregate(y~x+l+  v, data=vT0, FUN=mean)
          }

          ag    <- aggregate(y~x+l, data=vT0, FUN=mean)
          ag$sd <- aggregate(y~x+l, data=vT0, FUN=sd)[,3]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(y~x+l, data=vT0, FUN=length)[,3]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure0=="SD")
          {
            ag$ll <- ag[,3] - z * ag$sd
            ag$ul <- ag[,3] + z * ag$sd
          }
          if (input$selMeasure0=="SE")
          {
            ag$ll <- ag[,3] - z * ag$se
            ag$ul <- ag[,3] + z * ag$se
          }

          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- paste(input$replyLine0, collapse = " ")
          colnames(ag)[3] <- input$replyVar0

          return(ag)
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          vT <- subset(vT, is.element(vT$indexPlot,input$catPlot0) & is.element(vT$indexLine,input$catLine0))
          vT$indexPlot <- as.character(vT$indexPlot)
          vT$indexLine <- as.character(vT$indexLine)

          if (nrow(vT)==0)
            return(data.frame())

          x <- c()
          y <- c()
          p <- c()
          l <- c()
          s <- c()
          v <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+varIndex])
              p <- c(p,vT$indexPlot)
              l <- c(l,vT$indexLine)
              s <- c(s,as.character(vT$speaker))
              v <- c(v,as.character(vT$vowel))
            }
          }

          vT0 <- data.frame(x,p,l,s,v,y)

          if (is.element("average",input$selGeon0))
          {
            vT0 <- aggregate(y~x+p+l+s+v, data=vT0, FUN=mean)
            vT0 <- aggregate(y~x+p+l+  v, data=vT0, FUN=mean)
          }

          ag    <- aggregate(y~x+p+l, data=vT0, FUN=mean)
          ag$sd <- aggregate(y~x+p+l, data=vT0, FUN=sd)[,4]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(y~x+p+l, data=vT0, FUN=length)[,4]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure0=="SD")
          {
            ag$ll <- ag[,4] - z * ag$sd
            ag$ul <- ag[,4] + z * ag$sd
          }
          if (input$selMeasure0=="SE")
          {
            ag$ll <- ag[,4] - z * ag$se
            ag$ul <- ag[,4] + z * ag$se
          }

          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- paste(input$replyPlot0, collapse = " ")
          colnames(ag)[3] <- paste(input$replyLine0, collapse = " ")
          colnames(ag)[4] <- input$replyVar0

          return(ag)
        }
        else
          return(data.frame())
      })

      output$selScale0 <- renderUI(
      {
        selectInput('replyScale0', 'Scale:', optionsScale(), selected = optionsScale()[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      output$selRef0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyScale0)>0) && (input$replyScale0=="ST"))
          numericInput('replyRef0', 'Reference frequency:', value=50, min=1, step=1, width = "100%")
        else
          return(NULL)
      })

      output$selVar0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        options <- c("f0","F1","F2","F3")
        selectInput('replyVar0', 'Variable:', options, selected = options[1], multiple=FALSE, selectize=FALSE, width="100%", size=4)
      })

      output$catXaxis0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        timeCode   <- getTimeCode()
        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        nColumns   <- ncol(vowelTab())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        selectInput('catXaxis0', 'Select points:', timeCode, multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$selLine0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyLine0', 'Color variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catLine0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyLine0)>0)
          options <- unique(fuseCols(vowelTab(),input$replyLine0))
        else
          options <- NULL

        selectInput('catLine0', 'Select colors:', options, multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$selPlot0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyPlot0', 'Panel variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catPlot0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyPlot0)>0)
          options <- unique(fuseCols(vowelTab(),input$replyPlot0))
        else
          options <- NULL

        selectInput('catPlot0', 'Select panels:', options, multiple=TRUE, selectize=FALSE, width="100%")
      })

      scaleLab <- function(replyScale)
      {
        if (replyScale==" Hz")
          return("Hz")

        if (replyScale==" bark I")
          return("bark")

        if (replyScale==" bark II")
          return("bark")

        if (replyScale==" bark III")
          return("bark")

        if (replyScale==" ERB I")
          return("ERB")

        if (replyScale==" ERB II")
          return("ERB")

        if (replyScale==" ERB III")
          return("ERB")

        if (replyScale==" ln")
          return("ln")

        if (replyScale==" mel I")
          return("mel")

        if (replyScale==" mel II")
          return("mel")

        if (replyScale==" ST")
          return("ST")
      }

      scaleLab0 <- function()
      {
        return(scaleLab(input$replyScale0))
      }

      plotGraph0 <- function()
      {
        if (is.null(vowelSub0()) || (nrow(vowelSub0())==0))
          return(NULL)

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,2], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

          if (!is.element("smooth", input$selGeon0))
            vS <- vT
          else
          {
            vS <- data.frame(spline(vT$x, vT$y, n=nrow(vT)*10))
            vS$ll <- spline(vT$x, vT$ll, n=nrow(vT)*10)$y
            vS$ul <- spline(vT$x, vT$ul, n=nrow(vT)*10)$y
          }

          if (is.element("points", input$selGeon0))
            Geom_Point <- geom_point(colour="indianred2", size=3)
          else
            Geom_Point <- geom_point(colour="indianred2", size=0)

          graphics::plot(ggplot(data=vT, aes(x, y, group=1)) +
                         geom_line(data=vS, colour="indianred2", size=1) +
                         Geom_Point +
                         geom_ribbon(data=vS, aes(ymin=ll, ymax=ul), alpha=0.2) +
                         ggtitle(input$title0) +
                         scale_x_continuous(breaks = unique(vT$x)) +
                         xlab("relative duration") + ylab(paste0(input$replyVar0," (",scaleLab0(),")")) +
                         theme_bw() +
                         theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                               plot.title     =element_text(face="bold", hjust = 0.5),
                               aspect.ratio   =0.67))
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,3], p=vowelSub0()[,2], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

          if (!is.element("smooth", input$selGeon0))
            vS <- vT
          else
          {
            panels <- unique(vT$p)

            vS <- data.frame()
            for (i in 1:length(panels))
            {
              vSsub <- subset(vT, p==panels[i])
              vSspl <- data.frame(spline(vSsub$x, vSsub$y, n=nrow(vSsub)*10), p=panels[i])
              vSspl$ll <- spline(vSsub$x, vSsub$ll, n=nrow(vSsub)*10)$y
              vSspl$ul <- spline(vSsub$x, vSsub$ul, n=nrow(vSsub)*10)$y
              vS <- rbind(vS,vSspl)
            }

            vT <- vT[with(vT, order(x, p)), ]
            vS <- vS[with(vS, order(x, p)), ]
          }

          if (is.element("points", input$selGeon0))
            Geom_Point <- geom_point(colour="indianred2", size=3)
          else
            Geom_Point <- geom_point(colour="indianred2", size=0)

          graphics::plot(ggplot(data=vT, aes(x, y, group=1)) +
                         geom_line(data=vS, colour="indianred2", size=1) +
                         Geom_Point +
                         geom_ribbon(data=vS, aes(ymin=ll, ymax=ul), alpha=0.2) +
                         ggtitle(input$title0) +
                         scale_x_continuous(breaks = unique(vT$x)) +
                         xlab("relative duration") + ylab(paste0(input$replyVar0," (",scaleLab0(),")")) +
                         facet_wrap(~vT$p) +
                         theme_bw() +
                         theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                               plot.title     =element_text(face="bold", hjust = 0.5),
                               aspect.ratio   =0.67))
        }
        else

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,3], l=vowelSub0()[,2], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

          if (!is.element("smooth", input$selGeon0))
            vS <- vT
          else
          {
            lines <- unique(vT$l)

            vS <- data.frame()
            for (i in 1:length(lines))
            {
              vSsub <- subset(vT, l==lines[i])
              vSspl <- data.frame(spline(vSsub$x, vSsub$y, n=nrow(vSsub)*10), l=lines[i])
              vSspl$ll <- spline(vSsub$x, vSsub$ll, n=nrow(vSsub)*10)$y
              vSspl$ul <- spline(vSsub$x, vSsub$ul, n=nrow(vSsub)*10)$y
              vS <- rbind(vS,vSspl)
            }

            vT <- vT[with(vT, order(x, l)), ]
            vS <- vS[with(vS, order(x, l)), ]
          }

          if (is.element("points", input$selGeon0))
            Geom_Point <- geom_point(size=3)
          else
            Geom_Point <- geom_point(size=0)

          graphics::plot(ggplot(data=vT, aes(x, y, group=l, color=l)) +
                         geom_line(data=vS, size=1) +
                         Geom_Point +
                         geom_ribbon(data=vS, aes(x=x, ymin=ll, ymax=ul, fill = l), alpha=0.2, colour=NA) +
                         ggtitle(input$title0) +
                         scale_x_continuous(breaks = unique(vT$x)) +
                         xlab("relative duration") + ylab(paste0(input$replyVar0," (",scaleLab0(),")")) +
                         scale_colour_discrete(name=paste0(paste(input$replyLine0,collapse = " "),"\n")) +
                         theme_bw() +
                         theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                               plot.title     =element_text(face="bold", hjust = 0.5),
                               legend.key.size=unit(1.5, 'lines'),
                               aspect.ratio   =0.67) +
                         guides(fill=FALSE))
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          vT <- data.frame(x=vowelSub0()$time, y=vowelSub0()[,4], p=vowelSub0()[,2], l=vowelSub0()[,3], ll=vowelSub0()$ll, ul=vowelSub0()$ul)

          if (!is.element("smooth", input$selGeon0))
            vS <- vT
          else
          {
            panels <- unique(vT$p)
             lines <- unique(vT$l)

            vS <- data.frame()
            for (i in 1:length(panels))
            {
              for (j in 1:length(lines))
              {
                vSsub <- subset(vT, (p==panels[i]) & (l==lines[j]))

                if (nrow(vSsub)>0)
                {
                  vSspl <- data.frame(spline(vSsub$x, vSsub$y, n=nrow(vSsub)*10), p=panels[i], l=lines[j])
                  vSspl$ll <- spline(vSsub$x, vSsub$ll, n=nrow(vSsub)*10)$y
                  vSspl$ul <- spline(vSsub$x, vSsub$ul, n=nrow(vSsub)*10)$y
                  vS <- rbind(vS,vSspl)
                }
              }
            }

            vT <- vT[with(vT, order(x, p, l)), ]
            vS <- vS[with(vS, order(x, p, l)), ]
          }

          if (is.element("points", input$selGeon0))
            Geom_Point <- geom_point(size=3)
          else
            Geom_Point <- geom_point(size=0)

          graphics::plot(ggplot(data=vT, aes(x, y, group=l, color=l)) +
                         geom_line(data=vS, size=1) +
                         Geom_Point +
                         geom_ribbon(data=vS, aes(x=x, ymin=ll, ymax=ul, fill = l), alpha=0.2, colour=NA) +
                         ggtitle(input$title0) +
                         scale_x_continuous(breaks = unique(vT$x)) +
                         xlab("relative duration") + ylab(paste0(input$replyVar0," (",scaleLab0(),")")) +
                         scale_colour_discrete(name=paste0(paste(input$replyLine0, collapse = " "),"\n")) +
                         facet_wrap(~vT$p) +
                         theme_bw() +
                         theme(text           =element_text(size=as.numeric(input$replyPoint0b), family=input$replyFont0b),
                               plot.title     =element_text(face="bold", hjust = 0.5),
                               legend.key.size=unit(1.5, 'lines'),
                               aspect.ratio   =0.67)+
                         guides(fill=FALSE))
        }
        else {}
      }

      res0 <- function()
      {
        if (length(input$replySize0b)==0)
          return(72)

        if (input$replySize0b=="small")
          return(36)
        if (input$replySize0b=="medium")
          return(72)
        if (input$replySize0b=="large")
          return(90)
      }

      observeEvent(input$replySize0b,
      {
        output$graph0 <- renderPlot(height = 550, width = 700, res = res0(),
        {
          if (length(input$catXaxis0)>0)
          {
            plotGraph0()
          }
        })
      })

      output$Graph0 <- renderUI(
      {
        plotOutput("graph0", height="627px")
      })

      output$selFormat0a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat0a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName0a <- function()
      {
        return(paste0("contoursTable.",input$replyFormat0a))
      }

      output$download0a <- downloadHandler(filename = fileName0a, content = function(file)
      {
        if (length(input$catXaxis0)>0)
        {
          vT <- vowelSub0()

          colnames(vT)[which(colnames(vT)=="sd")] <- "standard deviation"
          colnames(vT)[which(colnames(vT)=="se")] <- "standard error"
          colnames(vT)[which(colnames(vT)=="n" )] <- "number of observations"
          colnames(vT)[which(colnames(vT)=="ll")] <- "lower limit"
          colnames(vT)[which(colnames(vT)=="ul")] <- "upper limit"
        }
        else
          vT <- data.frame()

        if (input$replyFormat0a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

        if (input$replyFormat0a=="xlsx")
        {
          WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
        }
        else {}
      })

      output$selSize0b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize0b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selFont0b <- renderUI(
      {
        options <- c("Courier" = "Courier", "Helvetica" = "Helvetica", "Times" = "Times")
        selectInput('replyFont0b', label=NULL, options, selected = "Helvetica", selectize=FALSE, multiple=FALSE)
      })

      output$selPoint0b <- renderUI(
      {
        options <- c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
        selectInput('replyPoint0b', label=NULL, options, selected = 22, selectize=FALSE, multiple=FALSE)
      })

      output$selFormat0b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
        selectInput('replyFormat0b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
      })

      fileName0b <- function()
      {
        return(paste0("contoursPlot.",input$replyFormat0b))
      }

      output$download0b <- downloadHandler(filename = fileName0b, content = function(file)
      {
        grDevices::pdf(NULL)

        scale  <- 72/res0()
        width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
        height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
        
        if ((length(input$catXaxis0)>0) && (nrow(vowelSub0())>0))
          plot <- plotGraph0()
        else
          plot <- ggplot()+theme_bw()
        
        show_modal_spinner()
        
        if (input$replyFormat0b=="JPG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
        else
        if (input$replyFormat0b=="PNG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
        else
        if (input$replyFormat0b=="SVG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
        else
        if (input$replyFormat0b=="EPS")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
        else
        if (input$replyFormat0b=="PDF")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
        else
        if (input$replyFormat0b=="TEX")
        {
          tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
          print(plot)
        }
        else {}

        grDevices::graphics.off()
        
        remove_modal_spinner()
      })

      ##########################################################################

      replyTimes10  <- reactive(input$replyTimes1)
      replyTimes1   <- debounce(replyTimes10 , 2000)

      replyTimesN10 <- reactive(input$replyTimesN1)
      replyTimesN1  <- debounce(replyTimesN10, 2000)

      vowelScale1 <- reactive(
      {
        return(vowelScale(vowelTab(),input$replyScale1,0))
      })

      vowelNorm1 <- reactive(
      {
        if (length(input$replyNormal1)==0)
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelScale1()))
        nColumns   <- ncol(vowelScale1())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        if (!is.null(replyTimesN1()))
          replyTimesN <- replyTimesN1()
        else
          return(NULL)

        if (!is.null(replyTimesN1()))
          replyTimesN <- replyTimesN1()
        else
          return(NULL)
        
        vL1 <- vowelLong1(vowelScale1(),replyTimesN)
        vL2 <- vowelLong2(vL1)
        vL3 <- vowelLong3(vL1)
        vL4 <- vowelLong4(vL1)

        return(vowelNormF(vowelScale1(),vL1,vL2,vL3,vL4,input$replyNormal1))
      })

      vowelSub1 <- reactive(
      {
        if ((is.null(vowelNorm1())) || (nrow(vowelNorm1())==0))
          return(NULL)

        vT <- vowelNorm1()

        vT$indexColor <- fuseCols(vowelNorm1(),input$replyColor1)
        vT$indexShape <- fuseCols(vowelNorm1(),input$replyShape1)
        vT$indexPlot  <- fuseCols(vowelNorm1(),input$replyPlot1)

        indexVowel <- grep("^vowel$", colnames(vowelNorm1()))

        ### check begin

        nPoints <- (ncol(vowelTab()) - (indexVowel + 1))/5

        if (max(as.numeric(replyTimes1()))>nPoints)
          return(NULL)
        else

        if (length(vT$indexColor)==0)
          return(NULL)
        else

        if (length(replyTimes1())>1)
        {}
        else

        if (input$axisZ!="--")
        {}
        else

        if (length(vT$indexShape)==0)
          return(NULL)
        else

        if (length(vT$indexPlot)==0)
          return(NULL)
        else {}

        ### check end

        if (length(input$catColor1)>0)
        {
          vT1 <- data.frame()

          for (q in (1:length(input$catColor1)))
          {
            vT1 <- rbind(vT1, subset(vT, indexColor==input$catColor1[q]))
          }
        }
        else
        {
          vT1 <- vT
        }

        if (length(input$catShape1)>0)
        {
          vT2 <- data.frame()

          for (q in (1:length(input$catShape1)))
          {
            vT2 <- rbind(vT2, subset(vT1, indexShape==input$catShape1[q]))
          }
        }
        else
        {
          vT2 <- vT1
        }

        if (length(input$catPlot1)>0)
        {
          vT3 <- data.frame()

          for (q in (1:length(input$catPlot1)))
          {
            vT3 <- rbind(vT3, subset(vT2, indexPlot==input$catPlot1[q]))
          }
        }
        else
        {
          vT3 <- vT2
        }

        vT <- vT3

        ###

        if (nrow(vT)>0)
        {
          vT0 <- data.frame()

          for (i in (1:length(replyTimes1())))
          {
            Code <- strtoi(replyTimes1()[i])

            indexF1 <- indexVowel + 4 + ((Code-1) * 5)
            indexF2 <- indexVowel + 5 + ((Code-1) * 5)
            indexF3 <- indexVowel + 6 + ((Code-1) * 5)

            if (length(input$catColor1)>0)
              Color <- vT$indexColor
            else
              Color <- rep("none",nrow(vT))

            if (length(input$catShape1)>0)
              Shape <- vT$indexShape
            else
              Shape <- rep("none",nrow(vT))

            if (length(input$catPlot1)>0)
              Plot  <- vT$indexPlot
            else
              Plot  <- rep("none",nrow(vT))

            if (input$axisX=="F1")
              Xaxis <- vT[,indexF1]
            if (input$axisX=="F2")
              Xaxis <- vT[,indexF2]
            if (input$axisX=="F3")
              Xaxis <- vT[,indexF3]

            if (input$axisY=="F1")
              Yaxis <- vT[,indexF1]
            if (input$axisY=="F2")
              Yaxis <- vT[,indexF2]
            if (input$axisY=="F3")
              Yaxis <- vT[,indexF3]

            if (input$axisZ=="--")
              Zaxis <- 0
            if (input$axisZ=="F1")
              Zaxis <- vT[,indexF1]
            if (input$axisZ=="F2")
              Zaxis <- vT[,indexF2]
            if (input$axisZ=="F3")
              Zaxis <- vT[,indexF3]

            if (input$axisX=="--")
              Xaxis <- 0
            if (input$axisY=="--")
              Yaxis <- 0
            if (input$axisZ=="--")
              Zaxis <- 0

            if (any(is.na(Xaxis)))
              Xaxis <- 0
            if (any(is.na(Yaxis)))
              Yaxis <- 0
            if (any(is.na(Zaxis)))
              Zaxis <- 0

            vT0 <- rbind(vT0, data.frame(speaker = vT$speaker  ,
                                         vowel   = vT$vowel    ,
                                         color   = Color       ,
                                         shape   = Shape       ,
                                         plot    = Plot        ,
                                         index   = rownames(vT),
                                         time    = i           ,
                                         X       = Xaxis,
                                         Y       = Yaxis,
                                         Z       = Zaxis))
          }

          if (input$average1 | input$ltf1)
            vT0 <- aggregate(cbind(X,Y,Z)~speaker+vowel+color+shape+plot+time, data=vT0, FUN=mean)

          if (input$ltf1)
          {
            colnames(vT0)[2] <- "v0wel"
            colnames(vT0)[1] <- "vowel"

            vT0$v0wel   <- NULL
          }
          else
            vT0$speaker <- NULL

          vT <- vT0
        }
        else {}

        ###

        if ((nrow(vT)>0) & (input$average1 | input$ltf1))
        {
          vT <- aggregate(cbind(X,Y,Z)~vowel+color+shape+plot+time, data=vT, FUN=mean)

          no <- nrow(aggregate(cbind(X,Y,Z)~vowel+color+shape+plot, data=vT, FUN=mean))
          index <- seq(1:no)
          vT$index <- rep(index,length(replyTimes1()))
        }

        ###

        if (nrow(vT)>0)
        {
          vT$vowel <- factor(vT$vowel)
          vT$color <- factor(vT$color)
          vT$shape <- factor(vT$shape)
          vT$plot  <- factor(vT$plot)
          vT$time  <- factor(vT$time)
          vT$index <- factor(vT$index)

        # utils::write.table(vT, "vT.csv", sep = "\t", row.names = FALSE)
          return(vT)
        }
        else
        {
          return(data.frame())
        }
      })

      output$selTimes1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimes1', 'Time points to be shown:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$selScale1 <- renderUI(
      {
        selectInput('replyScale1', 'Scale:', optionsScale()[1:(length(optionsScale())-1)], selected = optionsScale()[1], selectize=FALSE, multiple=FALSE)
      })

      output$selNormal1 <- renderUI(
      {
        if (is.null(vowelTab()) || length(input$replyScale1)==0)
          return(NULL)

        onlyF1F2 <- ((input$axisX!="F3") & (input$axisY!="F3") & (input$axisZ!="F3"))
        selectInput('replyNormal1', 'Normalization:', optionsNormal(vowelTab(), input$replyScale1, TRUE, onlyF1F2), selected = optionsNormal(vowelTab(), input$replyScale1, TRUE, onlyF1F2)[1], selectize=FALSE, multiple=FALSE)
      })

      output$selTimesN <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyNormal1)>0) && ((input$replyNormal1=="") |
                                               (input$replyNormal1==" Peterson") |
                                               (input$replyNormal1==" Sussman") |
                                               (input$replyNormal1==" Syrdal & Gopal") |
                                               (input$replyNormal1==" Thomas & Kendall")))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimesN1', 'Normalization based on:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$manScale <- renderUI(
      {
        if (input$axisZ=="--")
        {
          checkboxInput("selManual", "min/max", FALSE)
        }
      })

      output$selF1min <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyXmin', 'min. x', value=NULL, step=10, width = "100%")
        }
      })

      output$selF1max <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyXmax', 'max. x', value=NULL, step=10, width = "100%")
        }
      })

      output$selF2min <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyYmin', 'min. y', value=NULL, step=10, width = "100%")
        }
      })

      output$selF2max <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyYmax', 'max. y', value=NULL, step=10, width = "100%")
        }
      })

      output$selColor1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[1:(indexVowel-1)]))

        if (!input$ltf1)
          options <- c(colnames(vowelTab()[indexVowel]),options)

        selectInput('replyColor1', 'Color variable:', options, selected=options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
      })

      output$catColor1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyColor1)>0)
          options <- unique(fuseCols(vowelTab(),input$replyColor1))
        else
          options <- NULL

        selectInput('catColor1', 'Select colors:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
      })

      output$selShape1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(replyTimes1())==1) && (input$axisZ=="--"))
        {
          indexVowel <- grep("^vowel$", colnames(vowelTab()))

          if (input$geon2 | input$geon3 | input$geon4 | input$geon5)
            options <- c()
          else
            options <- c(colnames(vowelTab()[1:(indexVowel-1)]))

          if (!input$ltf1)
            options <- c(colnames(vowelTab()[indexVowel]),options)
        }
        else
        {
          options <- "none"
        }

        selectInput('replyShape1', 'Shape variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
      })

      output$catShape1 <- renderUI(
      {
        if  (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyShape1)>0) && (length(replyTimes1())==1) && (input$axisZ=="--"))
        {
          if (input$geon2 | input$geon3 | input$geon4 | input$geon5)
            options <- NULL
          else
            options <- unique(fuseCols(vowelTab(),input$replyShape1))
        }
        else
          options <- NULL

        selectInput('catShape1', 'Select shapes:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
      })

      output$selPlot1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (input$axisZ=="--")
        {
          indexVowel <- grep("^vowel$", colnames(vowelTab()))
          options <- c(colnames(vowelTab()[1:(indexVowel-1)]))

          if (!input$ltf1)
            options <- c(colnames(vowelTab()[indexVowel]),options)
        }
        else
        {
          options <- "none"
        }

        selectInput('replyPlot1', 'Panel variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
      })

      output$catPlot1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyPlot1)>0) && (input$axisZ=="--"))
          options <- unique(fuseCols(vowelTab(),input$replyPlot1))
        else
          options <- NULL

        selectInput('catPlot1', 'Select panels:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
      })

      output$selGeon1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((input$axisZ=="--") && (length(replyTimes1())<=1))
          tagList(splitLayout
          (
            cellWidths = c("19%", "17%", "15%", "21%", "19%"),

            checkboxInput("geon1", "labels" , value = FALSE),
            checkboxInput("geon2", "cent."  , value = FALSE),
            checkboxInput("geon3", "hull"   , value = FALSE),
            checkboxInput("geon4", "spokes" , value = FALSE),
            checkboxInput("geon5", "ellipse", value = FALSE)
          ))
        else

        if ((input$axisZ!="--") && (length(replyTimes1())<=1))
          tagList(splitLayout
          (
            cellWidths = c("19%", "19%", "19%"),

            checkboxInput("geon1", "labels", value = FALSE),
            checkboxInput("geon2", "lines" , value = TRUE )
          ))
        else

        if (length(replyTimes1())> 2)
          checkboxInput("geon1", "smooth trajectories", value = FALSE)
        else {}
      })

      output$selPars <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((input$axisZ=="--") && (length(replyTimes1())==1) && (length(input$geon5)>0) && input$geon5)
          numericInput('replyLevel', 'Confidence level:', value=0.95, step=0.01, width = "100%")
        else

        if  (input$axisZ!="--")
        {
          tagList(splitLayout
          (
            cellWidths = c("50%", "50%"),
            numericInput('replyPhi'  , 'Angle x-axis:', value=40, step=1, width = "100%"),
            numericInput('replyTheta', 'Angle z-axis:', value=30, step=1, width = "100%")
          ))
        }
        else
          return(NULL)
      })

      numColor <- function()
      {
        if ((length(input$replyColor1)>0) && (length(input$catColor1)>0))
          return(length(input$catColor1))
        else
          return(0)
      }

      numShape <- function()
      {
        if ((length(input$replyShape1)>0) && (length(input$catShape1)>0))
          return(length(input$catShape1))
        else
          return(0)
      }

      numAll <- function()
      {
        return(numColor()+numShape())
      }

      colPalette <- function(n,grayscale)
      {
        if (!grayscale)
        {
          labColors  <- c("#c87e66","#b58437","#988a00","#709000","#27942e","#00965c","#009482","#008ea3","#0081bd","#386acc","#8d46c8","#b315b1","#bd0088","#b61a51")
          labPalette <- grDevices::colorRampPalette(labColors, space = "Lab")

          if (n==1)
            return(labColors[c(10)])

          if (n==2)
            return(labColors[c(8,14)])

          if (n==3)
            return(labColors[c(5,10,13)])

          if (n==4)
            return(labColors[c(3,7,11,14)])

          if (n==5)
            return(labColors[c(3,5,9,11,14)])

          if (n==6)
            return(labColors[c(2,5,7,10,12,14)])

          if (n==7)
            return(labColors[c(2,4,5,8,10,12,14)])

          if (n==8)
            return(labColors[c(1,3,5,7,9,11,12,14)])

          if (n==9)
            return(labColors[c(1,3,4,6,8,10,11,12,14)])

          if (n==10)
            return(labColors[c(1,3,4,5,7,9,10,11,12,14)])

          if (n==11)
            return(labColors[c(1,2,4,5,6,7,9,10,11,12,14)])

          if (n==12)
            return(labColors[c(1,2,3,4,5,7,8,9,11,12,13,14)])

          if (n==13)
            return(labColors[c(1,2,3,4,5,6,7,8,10,11,12,13,14)])

          if (n==14)
            return(labColors[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)])

          if (n>=15)
            return(labPalette(n))
        }
        else
        {
          return(grDevices::gray(0:(n-1)/n))
        }
      }

      colPalette1 <- function(n)
      {
        return(colPalette(n,input$grayscale1))
      }

      shpPalette <- function()
      {
        return(c(19,1,17,2,15,0,18,5,3,4,8))
      }

      scaleLab1 <- function()
      {
        return(scaleLab(input$replyScale1))
      }

      plotGraph1 <- function()
      {
        if (is.null(vowelSub1()) || (nrow(vowelSub1())==0) | (length(replyTimes1())==0))
          return(NULL)

        if ((length(replyTimes1())==1) && (!(input$geon2 | input$geon3 | input$geon4 | input$geon5)) && (input$axisZ=="--"))
        {
          vT <- vowelSub1()

          if ((numColor()>0) & (numShape()>0) & (numShape()<=11))
          {
            Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color, shape=shape)) +
              scale_shape_manual(values=shpPalette())
            
            if (input$geon1)
              Basis <- Basis + geom_point(size=2.5) + geom_text_repel(position="identity", aes(label=vowel), hjust=0.5, vjust=0.5, family=input$replyFont1b, size=5, alpha=1.0)
            else        
              Basis <- Basis + geom_point(size=2.5)
          }
          else
            
          if  (numColor()>0)
          {
            Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color))
            
            if (input$geon1)
              Basis <- Basis + geom_text(position="identity", aes(label=vowel), hjust=0.5, vjust=0.5, family=input$replyFont1b, size=5, alpha=1.0)
            else        
              Basis <- Basis + geom_point(size=2.5)
          }
          else
            
          if ((numShape()>0) & (numShape()<=11))
          {
            Basis <- ggplot(data=vT, aes(x=X, y=Y, shape=shape)) +
              scale_shape_manual(values=shpPalette())
  
          if (input$geon1)
              Basis <- Basis + geom_point(size=2.5, colour=colPalette1(1)) + geom_text_repel(position="identity", aes(label=vowel), hjust=0.5, vjust=0.5, family=input$replyFont1b, size=5, alpha=1.0)
            else        
              Basis <- Basis + geom_point(size=2.5, colour=colPalette1(1))
          }
          else
          {
            Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color))
            
            if (input$geon1)
              Basis <- Basis + geom_text(position="identity", aes(label=vowel), hjust=0.5, vjust=0.5, family=input$replyFont1b, size=5, alpha=1.0)
            else        
              Basis <- Basis + geom_point(size=2.5)
          }
  
          if (!input$geon1)
              Basis <- Basis + labs(colour=paste(input$replyColor1, collapse = " "), shape=paste(input$replyShape1, collapse = " "))
            else
              Basis <- Basis + guides(colour=FALSE) + labs(shape=paste(input$replyShape1, collapse = " "))    

          if ((length(input$selManual)>0) && (input$selManual==TRUE))
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"), position="top"  , limits = c(input$replyXmax, input$replyXmin))
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"), position="right", limits = c(input$replyYmax, input$replyYmin))
          }
          else
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"), position="top"  )
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"), position="right")
          }

          if (length(input$catPlot1)>0)
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_wrap(~plot)
          }
          else
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_null()
          }

          if ((numAll()>0) & (numAll()<=18))
            Legend <- theme(legend.position="right")
          else
          if ((numColor()>0) & (numColor()<=18))
            Legend <- guides(shape=FALSE)
          else
          if ((numShape()>0) & (numShape()<=11))
            Legend <- guides(color=FALSE)
          else
            Legend <- theme(legend.position="none")

          graphics::plot(Basis + scaleX + scaleY + Title + Facet +
                         scale_color_manual(values=colPalette1(length(unique(vT$color)))) +
                         theme_bw() +
                         theme(text           =element_text(size=as.numeric(input$replyPoint1b), family=input$replyFont1b),
                               plot.title     =element_text(face="bold", hjust = 0.5),
                               legend.key.size=unit(1.5,'lines'),
                               aspect.ratio   =1) +
                         Legend)
        }
        else

        if ((length(replyTimes1())==1) && (input$geon2 | input$geon3 | input$geon4 | input$geon5) && (input$axisZ=="--"))
        {
          vT <- vowelSub1()

          centers <- aggregate(cbind(X,Y)~color+plot, data=vT, FUN=mean)

          vT <- vT[order(vT$plot, vT$index, vT$time),]
          vT$index <- paste0(vT$time,vT$index)

          Basis <- ggplot(data = vT, aes(x=X, y=Y, fill=color, color=color))
          Fill  <- geom_blank()

          if (input$geon1)
            Points <- geom_text(position="identity", aes(label=vowel), hjust=0.5, vjust=0.5, family=input$replyFont1b, size=5, alpha=0.3)
          else
            Points <- geom_blank()

          if (input$geon2)
          {
            if (input$geon4)
              Centers <- geom_text(data=centers, position="identity", aes(label=color), hjust=0.5, vjust=0.5, family=input$replyFont1b, size= 7, alpha=1.0, color="black")
            else
              Centers <- geom_text(data=centers, position="identity", aes(label=color), hjust=0.5, vjust=0.5, family=input$replyFont1b, size=10, alpha=1.0)

            Legend <- theme(legend.position="none")
          }
          else
          {
            Centers <- geom_blank()
            Legend <- theme(legend.position="right")
          }

          if (input$geon3)
          {
            chulls <- ddply(vT, .(color,plot), function(df) df[grDevices::chull(df$X, df$Y), ])

            if ((length(unique(vT$color))==1) | (as.character(input$replyColor1)[1]=="vowel"))
            {
              Hull <- geom_polygon(data=chulls, aes(x=X, y=Y, group=color, fill=color), alpha=0.1)
              Fill <- scale_fill_manual(values=colPalette1(length(unique(vT$color))))
            }
            else

            if (length(unique(vT$color))> 1)
            {
              Hull <- geom_polygon(data=chulls, aes(x=X, y=Y, group=color, fill=color), alpha=0  )
              Fill <- scale_fill_manual(values=rep("white", length(unique(vT$color))))
            }
            else {}
          }
          else
          {
            Hull <- geom_blank()
          }

          if (input$geon4)
          {
            vT0 <- vT
            for (i in (1:nrow(vT0)))
            {
              centersSub <- subset(centers, (centers$color==vT0$color[i]) & (centers$plot==vT0$plot[i]))

              vT0$X[i] <- centersSub$X
              vT0$Y[i] <- centersSub$Y
            }

            vT0 <- rbind(vT,vT0)
            vT0 <- vT0[order(vT0$plot, vT0$index, vT0$time),]
            vT0$index <- paste0(vT0$time,vT0$index)

            Spokes <- geom_path(data=vT0, aes(group = index), arrow = arrow(ends = "last", length = unit(0, "inches")), size=1.0, alpha=0.3)
          }
          else
          {
            Spokes <- geom_blank()
          }

          if ((input$geon5) & length(input$replyLevel)>0)
          {
            if ((input$geon1) | (input$geon3))
              Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel)
            else
            {
              if ((length(unique(vT$color))==1) | (as.character(input$replyColor1)[1]=="vowel"))
              {
                Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel, geom="polygon", alpha=0.3)
                Fill <- scale_fill_manual(values=colPalette1(length(unique(vT$color))))
              }
              else

              if (length(unique(vT$color))> 1)
              {
                Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel, geom="polygon", alpha=0  )
                Fill <- scale_fill_manual(values=rep("white", length(unique(vT$color))))
              }
              else {}
            }
          }
          else
          {
            Ellipse <- geom_blank()
          }

          if ((length(input$selManual)>0) && (input$selManual==TRUE))
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"), position="top"  , limits = c(input$replyXmax, input$replyXmin))
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"), position="right", limits = c(input$replyYmax, input$replyYmin))
          }
          else
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"), position="top"  )
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"), position="right")
          }

          if (length(input$catPlot1)>0)
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_wrap(~plot)
          }
          else
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_null()
          }

          if ((numColor()==0) | (numColor()>18))
          {
            Legend <- theme(legend.position="none")
          }

          graphics::plot(Basis + Points + Hull +Spokes + Ellipse + Centers + scaleX + scaleY + Title + Facet +
                         scale_color_manual(values=colPalette1(length(unique(vT$color)))) + Fill +
                         labs(colour=paste(input$replyColor1, collapse = " "), fill=paste(input$replyColor1, collapse = " ")) +
                         theme_bw() +
                         theme(text           =element_text(size=as.numeric(input$replyPoint1b), family=input$replyFont1b),
                               plot.title     =element_text(face="bold", hjust = 0.5),
                               legend.key.size=unit(1.5,'lines'),
                               aspect.ratio   =1) +
                         Legend)
        }
        else

        if ((length(replyTimes1())>1) && (input$axisZ=="--"))
        {
          vT <- vowelSub1()[order(vowelSub1()$index, vowelSub1()$time),]

          if (input$geon1)
          {
            xx <- c()
            yy <- c()

            for (i in unique(vT$index))
            {
              vTsub <- subset(vT, index==i)

              xx <- c(xx, spline(vTsub$time, vTsub$X, n=length(replyTimes1())*10)$y)
              yy <- c(yy, spline(vTsub$time, vTsub$Y, n=length(replyTimes1())*10)$y)
            }

            vT <- splitstackshape::expandRows(vT, 10, count.is.col = F, drop = F)
            vT$X <- xx
            vT$Y <- yy
          }

          Basis <- ggplot(data=vT, aes(x=X, y=Y, colour=color, label=""))

          if ((length(input$selManual)>0) && (input$selManual==TRUE))
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"), position="top"  , limits = c(input$replyXmax, input$replyXmin))
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"), position="right", limits = c(input$replyYmax, input$replyYmin))
          }
          else
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"), position="top"  )
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"), position="right")
          }

          if (length(input$catPlot1)>0)
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_wrap(~plot)
          }
          else
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_null()
          }

          if ((numColor()>0) & (numColor()<=18))
            Legend <- theme(legend.position="right")
          else
            Legend <- theme(legend.position="none")

          graphics::plot(Basis + scaleX + scaleY + Title + Facet +
                         geom_path(aes(group = index), arrow = arrow(ends = "last", length = unit(0.1, "inches")), size=0.7) +
                         scale_color_manual(values=colPalette1(length(unique(vT$color)))) +
                         labs(colour=paste(input$replyColor1, collapse = " ")) +
                         theme_bw() +
                         theme(text           =element_text(size=as.numeric(input$replyPoint1b), family=input$replyFont1b),
                               plot.title     =element_text(face="bold", hjust = 0.5),
                               legend.key.size=unit(1.5,'lines'),
                               aspect.ratio   =1) +
                         Legend
          )
        }
        else

        if ((length(replyTimes1())==1) && (input$axisZ!="--"))
        {
          vT <- vowelSub1()

          if (nrow(vT)==1)
            return(NULL)

          if ((input$axisX=="F1") | (input$axisX=="F2"))
            vT$X <- -1 * vT$X

          if ((input$axisY=="F1") | (input$axisY=="F2"))
            vT$Y <- -1 * vT$Y

          if ((input$axisZ=="F1") | (input$axisZ=="F2"))
            vT$Z <- -1 * vT$Z

          if (length(unique(vT$Z))==1)
          {
            zmin <- mean(vT$Z)-1
            zmax <- mean(vT$Z)+1
          }
          else
          {
            zmin <- min(vT$Z)
            zmax <- max(vT$Z)
          }

          if (input$geon1)
            cex <- 0.0
          else
            cex <- 1.0

          if (input$geon1)
            Cex <- 1.0
          else
            Cex <- 0.00001

          graphics::par(family = input$replyFont1b)
          Point <- as.numeric(input$replyPoint1b)/22

          if (input$geon2)
            alpha <- 0.2
          else
            alpha <- 0.0

          if ((length(input$replyPhi)==0) || (is.na(input$replyPhi)))
            Phi <- 40
          else
            Phi <- input$replyPhi

          if ((length(input$replyPhi)==0) || (is.na(input$replyTheta)))
            Theta <- 30
          else
            Theta <- input$replyTheta

          mar   <- ((length(unique(vT$color))-1)/length(unique(vT$color)))/2
          first <- 1+mar
          last  <- length(unique(vT$color))-mar
          step  <- (last-first)/(length(unique(vT$color))-1)

          at <- c(first)

          if (length(unique(vT$color))>2)
          {
            for (i in 1:(length(unique(vT$color))-2))
            {
              at <- c(at,first + (i*step))
            }
          }

          at <- c(at,last)

          if ((numColor()>1) & (numColor()<=18))
          {
            colvar <- as.integer(as.factor(vT$color))
            colkey <- list(at       = at,
                           side     = 4,
                           addlines = TRUE,
                           length   = 0.04*length(unique(vT$color)),
                           width    = 0.5,
                           labels   = unique(vT$color))
          }
          else
          {
            colvar <- F
            colkey <- FALSE
          }

          graphics::par(mar=c(1,2,2.0,4))

          scatter3D(x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    zlim     = c(zmin, zmax),
                    phi      = Phi,
                    theta    = Theta,
                    bty      = "g",
                    type     = "h",
                    cex      = 0,
                    alpha    = alpha,
                    ticktype = "detailed",
                    colvar   = colvar,
                    col      = colPalette1(length(unique(vT$color))),
                    colkey   = FALSE,
                    cex.main = Point * 1.75,
                    cex.lab  = Point,
                    cex.axis = Point,
                    main     = input$title1,
                    xlab     = paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"),
                    ylab     = paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"),
                    zlab     = paste0(input$axisZ," (",scaleLab1(),input$replyNormal1,")"),
                    add      = FALSE)

          scatter3D(x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    zlim     = c(zmin, zmax),
                    type     = "p",
                    pch      = 19,
                    cex      = cex,
                    alpha    = 1,
                    colvar   = colvar,
                    col      = colPalette1(length(unique(vT$color))),
                    colkey   = colkey,
                    add      = TRUE)

          text3D   (x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    zlim     = c(zmin, zmax),
                    cex      = Cex,
                    alpha    = 1,
                    labels   = as.character(vT$vowel),
                    colvar   = colvar,
                    col      = colPalette1(length(unique(vT$color))),
                    colkey   = FALSE,
                    add      = TRUE)

          graphics::par(mar=c(5.1,4.1,4.1,2.1))
        }
        else

        if ((length(replyTimes1())>1) && (input$axisZ!="--"))
        {
          vT <- vowelSub1()[order(vowelSub1()$index, vowelSub1()$time),]

          if (input$geon1)
          {
            xx <- c()
            yy <- c()
            zz <- c()

            for (i in unique(vT$index))
            {
              vTsub <- subset(vT, index==i)

              xx <- c(xx, spline(vTsub$time, vTsub$X, n=length(replyTimes1())*10)$y)
              yy <- c(yy, spline(vTsub$time, vTsub$Y, n=length(replyTimes1())*10)$y)
              zz <- c(zz, spline(vTsub$time, vTsub$Z, n=length(replyTimes1())*10)$y)
            }

            vT <- splitstackshape::expandRows(vT, 10, count.is.col = F, drop = F)
            vT$time <- rep(seq(1, length(replyTimes1())*10), length(unique(vT$index)))
            vT$X <- xx
            vT$Y <- yy
            vT$Z <- zz
          }

          if ((input$axisX=="F1") | (input$axisX=="F2"))
            vT$X <- -1 * vT$X

          if ((input$axisY=="F1") | (input$axisY=="F2"))
            vT$Y <- -1 * vT$Y

          if ((input$axisZ=="F1") | (input$axisZ=="F2"))
            vT$Z <- -1 * vT$Z

          if (length(unique(vT$Z))==1)
          {
            zmin <- mean(vT$Z)-1
            zmax <- mean(vT$Z)+1
          }
          else
          {
            zmin <- min(vT$Z)
            zmax <- max(vT$Z)
          }

          graphics::par(family = input$replyFont1b)
          Point <- as.numeric(input$replyPoint1b)/22

          if ((length(input$replyPhi)==0) || (is.na(input$replyPhi)))
            Phi <- 40
          else
            Phi <- input$replyPhi

          if ((length(input$replyPhi)==0) || (is.na(input$replyTheta)))
            Theta <- 30
          else
            Theta <- input$replyTheta

          mar   <- ((length(unique(vT$color))-1)/length(unique(vT$color)))/2
          first <- 1+mar
          last  <- length(unique(vT$color))-mar
          step  <- (last-first)/(length(unique(vT$color))-1)

          at <- c(first)

          if (length(unique(vT$color))>2)
          {
            for (i in 1:(length(unique(vT$color))-2))
            {
              at <- c(at,first + (i*step))
            }
          }

          at <- c(at,last)

          if ((numColor()>1) & (numColor()<=18))
          {
            VT     <- subset(vT, time==1)

            colvar <- as.integer(as.factor(vT$color))
            ColVar <- as.integer(as.factor(VT$color))
            ColKey <- list(at       = at,
                           side     = 4,
                           addlines = TRUE,
                           length   = 0.04*length(unique(VT$color)),
                           width    = 0.5,
                           labels   = unique(VT$color))
          }
          else
          {
            colvar <- F
            ColVar <- F
            ColKey <- FALSE
          }

          graphics::par(mar=c(1,2,2.0,4))

          scatter3D(x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    zlim     = c(zmin, zmax),
                    phi      = Phi,
                    theta    = Theta,
                    bty      = "g",
                    type     = "h",
                    pch      = 19,
                    cex      = 0,
                    ticktype = "detailed",
                    colvar   = colvar,
                    col      = colPalette1(length(unique(vT$color))),
                    colkey   = FALSE,
                    main     = input$title1,
                    cex.main = Point * 1.75,
                    cex.lab  = Point,
                    cex.axis = Point,
                    alpha    = 0.2,
                    xlab     = paste0(input$axisX," (",scaleLab1(),input$replyNormal1,")"),
                    ylab     = paste0(input$axisY," (",scaleLab1(),input$replyNormal1,")"),
                    zlab     = paste0(input$axisZ," (",scaleLab1(),input$replyNormal1,")"),
                    add      = FALSE)

          nTimes <- length(unique(vT$time))

          if (nTimes > 2)
          {
            for (i in (2:(nTimes-1)))
            {
              vT0 <- subset(vT, time==i-1)
              vT1 <- subset(vT, time==i)

              arrows3D(x0     = vT0$X,
                       y0     = vT0$Y,
                       z0     = vT0$Z,
                       x1     = vT1$X,
                       y1     = vT1$Y,
                       z1     = vT1$Z,
                       zlim   = c(zmin, zmax),
                       colvar = ColVar,
                       col    = colPalette1(length(unique(vT0$color))),
                       colkey = FALSE,
                       code   = 0,
                       length = 0.2,
                       type   = "triangle",
                       lwd    = 2,
                       alpha  = 1,
                       add    = TRUE)
            }
          }

          vT0 <- subset(vT, time==nTimes-1)
          vT1 <- subset(vT, time==nTimes  )

          arrows3D(x0     = vT0$X,
                   y0     = vT0$Y,
                   z0     = vT0$Z,
                   x1     = vT1$X,
                   y1     = vT1$Y,
                   z1     = vT1$Z,
                   zlim   = c(zmin, zmax),
                   colvar = ColVar,
                   col    = colPalette1(length(unique(vT0$color))),
                   colkey = ColKey,
                   code   = 2,
                   length = 0.3,
                   type   = "simple",
                   lwd    = 2,
                   alpha  = 1,
                   add    = TRUE)

          graphics::par(mar=c(5.1,4.1,4.1,2.1))
        }
        else {}
      }

      res1 <- function()
      {
        if (length(input$replySize1b)==0)
          return(72)

        if (input$replySize1b=="small")
          return(36)
        if (input$replySize1b=="medium")
          return(72)
        if (input$replySize1b=="large")
          return(90)
      }

      observeEvent(input$replySize1b,
      {
        output$graph1 <- renderPlot(height = 550, width = 700, res = res1(),
        {
          if (length(replyTimes1())>0)
          {
            plotGraph1()
          }
        })
      })

      output$Graph1 <- renderUI(
      {
        plotOutput("graph1", height="627px")
      })

      output$selFormat1a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat1a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName1a <- function()
      {
        return(paste0("formantsTable.",input$replyFormat1a))
      }

      output$download1a <- downloadHandler(filename = fileName1a, content = function(file)
      {
        if (length(replyTimes1())>0)
        {
          vT <- vowelSub1()

          colnames(vT)[which(colnames(vT)=="X")] <- input$axisX
          colnames(vT)[which(colnames(vT)=="Y")] <- input$axisY
          colnames(vT)[which(colnames(vT)=="Z")] <- input$axisZ
        }
        else
          vT <- data.frame()

        if (input$replyFormat1a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

        if (input$replyFormat1a=="xlsx")
        {
          WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
        }
        else {}
      })

      output$selSize1b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize1b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selFont1b <- renderUI(
      {
        options <- c("Courier" = "Courier", "Helvetica" = "Helvetica", "Times" = "Times")
        selectInput('replyFont1b', label=NULL, options, selected = "Helvetica", selectize=FALSE, multiple=FALSE)
      })

      output$selPoint1b <- renderUI(
      {
        options <- c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
        selectInput('replyPoint1b', label=NULL, options, selected = 22, selectize=FALSE, multiple=FALSE)
      })

      output$selFormat1b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
        selectInput('replyFormat1b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
      })

      fileName1b <- function()
      {
        return(paste0("formantPlot.",input$replyFormat1b))
      }

      save2D <- function(file)
      {
        grDevices::pdf(NULL)

        scale  <- 72/res1()
        width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
        height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
        
        if ((length(replyTimes1())>0) && (nrow(vowelSub1())>0))
          plot <- plotGraph1()
        else
          plot <- ggplot()+theme_bw()
        
        show_modal_spinner()
        
        if (input$replyFormat1b=="JPG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
        else
        if (input$replyFormat1b=="PNG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
        else
        if (input$replyFormat1b=="SVG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
        else
        if (input$replyFormat1b=="EPS")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
        else
        if (input$replyFormat1b=="PDF")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
        else    
        if (input$replyFormat1b=="TEX")
        {
          tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
          print(plot)
        }
        else {}

        grDevices::graphics.off()
        
        remove_modal_spinner()
      }

      save3D <- function(file)
      {
        grDevices::pdf(NULL)

        scale0  <- 300/res1()
        width0  <- 700 * scale0
        height0 <- 550 * scale0
        
        scale   <-  72/res1()
        width   <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE) * scale
        height  <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE) * scale

        show_modal_spinner()

        if (input$replyFormat1b=="JPG")
          grDevices::jpeg      (file, width = width0, height = height0, pointsize = 12, res = 300)
        else
        if (input$replyFormat1b=="PNG")
          grDevices::png       (file, width = width0, height = height0, pointsize = 12, res = 300)
        else
        if (input$replyFormat1b=="SVG")
          grDevices::svg       (file, width = width , height = height , pointsize = 12)
        else
        if (input$replyFormat1b=="EPS")
          grDevices ::cairo_ps (file, width = width , height = height , pointsize = 12)
        else
        if (input$replyFormat1b=="PDF")
          grDevices ::cairo_pdf(file, width = width , height = height , pointsize = 12)
        else    
        if (input$replyFormat1b=="TEX")
          tikzDevice::tikz     (file, width = width , height = height , pointsize = 12, engine='xetex')
        else {}

        if ((length(replyTimes1())>0) && (nrow(vowelSub1())>0))
          print(plotGraph1())
        else
          graphics::plot.new()

        grDevices::graphics.off()
        
        remove_modal_spinner()
      }

      output$download1b <- downloadHandler(filename = fileName1b, content = function(file)
      {
        if (input$axisZ=="--")
          save2D(file)
        else
          save3D(file)
      })

      ##########################################################################

      vowelScale4 <- reactive(
      {
        return(vowelScale(vowelTab(),input$replyScale4,0))
      })

      vowelDyn4 <- reactive(
      {
        if (is.null(vowelScale4()) || (nrow(vowelScale4())==0) || (length(input$replyVar4)<1) || (length(input$replyTimes4)<2))
          return(NULL)

        vT <- vowelScale4()

        indexVowel <- grep("^vowel$", colnames(vowelTab()))

        nColumns   <- ncol(vowelTab())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        vT["dynamics"] <- 0

        for (i in 1:(length(input$replyTimes4)-1))
        {
          i1 <- as.numeric(input$replyTimes4[i  ])
          i2 <- as.numeric(input$replyTimes4[i+1])

          indexTime1 <- indexVowel + 2 + ((i1-1)*5)
          indexTime2 <- indexVowel + 2 + ((i2-1)*5)

          Var <- c("f0","F1","F2","F3")
          sum <- rep(0,nrow(vT))

          for (j in 1:4)
          {
            if (is.element(Var[j],input$replyVar4))
            {
              indexVar1 <- indexTime1 + j
              indexVar2 <- indexTime2 + j

              sum <- sum + (vT[,indexVar1] - vT[,indexVar2])^2
            }
          }

          VL     <- sqrt(sum)
          VL_roc <- VL / (vT[,indexTime2] - vT[,indexTime1])

          if (input$replyMethod4=="TL")
            vT$dynamics <- vT$dynamics + VL

          if (input$replyMethod4=="TL_roc")
            vT$dynamics <- vT$dynamics + VL_roc
        }

        return(vT)
      })

      vowelSub4 <- reactive(
      {
        if (is.null(vowelDyn4()) || (nrow(vowelDyn4())==0) || (length(input$catXaxis4)==0))
          return(NULL)

        vT <- vowelDyn4()

        indexVowel <- grep("^vowel$", colnames(vT))

        if (any(is.na(vT[,indexVowel+1])))
          vT[,indexVowel+1] <- 0

        vT$indexXaxis <- fuseCols(vowelDyn4(),input$replyXaxis4)
        vT$indexLine  <- fuseCols(vowelDyn4(),input$replyLine4)
        vT$indexPlot  <- fuseCols(vowelDyn4(),input$replyPlot4)

        if (input$selError4=="0%")
          z <- 0
        if (input$selError4=="90%")
          z <- 1.645
        if (input$selError4=="95%")
          z <- 1.96
        if (input$selError4=="99%")
          z <- 4.575

        vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis4))

        if (nrow(vT)==0)
          return(NULL)

        if (((length(input$catLine4)==0) | (length(input$catLine4)>14)) && (length(input$catPlot4)==0))
        {
          if (is.element("average",input$selGeon4))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, speaker=vT$speaker, vowel=vT$vowel, dynamics=vT$dynamics)
            vT <- aggregate(dynamics ~ indexXaxis + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(dynamics ~ indexXaxis +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$dynamics ~ vT$indexXaxis, FUN=mean)
          ag$sd <- aggregate(vT$dynamics ~ vT$indexXaxis, FUN=sd)[,2]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$dynamics ~ vT$indexXaxis, FUN=length)[,2]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure4=="SD")
          {
            ag$ll <- ag[,2] - z * ag$sd
            ag$ul <- ag[,2] + z * ag$sd
          }
          if (input$selMeasure4=="SE")
          {
            ag$ll <- ag[,2] - z * ag$se
            ag$ul <- ag[,2] + z * ag$se
          }

          ag <- ag[order(ag[,2]),]
          ag[,1] <- factor(ag[,1], levels=ag[,1])

          colnames(ag)[1] <- paste(input$replyXaxis4, collapse = " ")
          colnames(ag)[2] <- input$replyMethod4

          return(ag)
        }
        else

        if (((length(input$catLine4)==0) | (length(input$catLine4)>14)) && (length(input$catPlot4)>0))
        {
          vT <- subset(vT, is.element(vT$indexPlot,input$catPlot4))

          if (nrow(vT)==0)
            return(data.frame())

          if (is.element("average",input$selGeon4))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, indexPlot=vT$indexPlot, speaker=vT$speaker, vowel=vT$vowel, dynamics=vT$dynamics)
            vT <- aggregate(dynamics ~ indexXaxis + indexPlot + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(dynamics ~ indexXaxis + indexPlot +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexPlot, FUN=mean)
          ag$sd <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexPlot, FUN=sd)[,3]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexPlot, FUN=length)[,3]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure4=="SD")
          {
            ag$ll <- ag[,3] - z * ag$sd
            ag$ul <- ag[,3] + z * ag$sd
          }
          if (input$selMeasure4=="SE")
          {
            ag$ll <- ag[,3] - z * ag$se
            ag$ul <- ag[,3] + z * ag$se
          }

          ag <- ag[order(ag[,3]),]
          xx <- unique(ag[,1])

          ag0 <- data.frame()

          for (q in (1:length(xx)))
          {
            ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
          }

          ag <- ag0
          ag[,1] <- factor(ag[,1], levels=xx)
          ag[,2] <- as.character(ag[,2])

          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- paste(input$replyXaxis4, collapse = " ")
          colnames(ag)[2] <- paste(input$replyPlot4 , collapse = " ")
          colnames(ag)[3] <- input$replyMethod4

          colnames(ag) <- make.unique(names(ag))

          return(ag)
        }
        else

        if (((length(input$catLine4)>0) & (length(input$catLine4)<=14)) && (length(input$catPlot4)==0))
        {
          vT <- subset(vT, is.element(vT$indexLine,input$catLine4))

          if (nrow(vT)==0)
            return(data.frame())

          if (is.element("average",input$selGeon4))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, indexLine=vT$indexLine, speaker=vT$speaker, vowel=vT$vowel, dynamics=vT$dynamics)
            vT <- aggregate(dynamics ~ indexXaxis + indexLine + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(dynamics ~ indexXaxis + indexLine +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexLine, FUN=mean)
          ag$sd <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexLine, FUN=sd)[,3]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexLine, FUN=length)[,3]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure4=="SD")
          {
            ag$ll <- ag[,3] - z * ag$sd
            ag$ul <- ag[,3] + z * ag$sd
          }
          if (input$selMeasure4=="SE")
          {
            ag$ll <- ag[,3] - z * ag$se
            ag$ul <- ag[,3] + z * ag$se
          }

          ag <- ag[order(ag[,3]),]
          xx <- unique(ag[,1])

          ag0 <- data.frame()

          for (q in (1:length(xx)))
          {
            ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
          }

          ag <- ag0
          ag[,1] <- factor(ag[,1], levels=xx)
          ag[,2] <- as.character(ag[,2])

          colnames(ag)[1] <- paste(input$replyXaxis4, collapse = " ")
          colnames(ag)[2] <- paste(input$replyLine4 , collapse = " ")
          colnames(ag)[3] <- input$replyMethod4

          colnames(ag) <- make.unique(names(ag))

          return(ag)
        }
        else

        if (((length(input$catLine4)>0) & (length(input$catLine4)<=14))  && (length(input$catPlot4)>0))
        {
          vT <- subset(vT, is.element(vT$indexLine,input$catLine4) & is.element(vT$indexPlot,input$catPlot4))

          if (nrow(vT)==0)
            return(data.frame())

          if (is.element("average",input$selGeon4))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, indexPlot=vT$indexPlot, indexLine=vT$indexLine, speaker=vT$speaker, vowel=vT$vowel, dynamics=vT$dynamics)
            vT <- aggregate(dynamics ~ indexXaxis + indexPlot + indexLine + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(dynamics ~ indexXaxis + indexPlot + indexLine +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=mean)
          ag$sd <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=sd)[,4]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$dynamics ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=length)[,4]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure4=="SD")
          {
            ag$ll <- ag[,4] - z * ag$sd
            ag$ul <- ag[,4] + z * ag$sd
          }
          if (input$selMeasure4=="SE")
          {
            ag$ll <- ag[,4] - z * ag$se
            ag$ul <- ag[,4] + z * ag$se
          }

          ag <- ag[order(ag[,4]),]
          xx <- unique(ag[,1])

          ag0 <- data.frame()

          for (q in (1:length(xx)))
          {
            ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
          }

          ag <- ag0
          ag[,1] <- factor(ag[,1], levels=xx)
          ag[,2] <- as.character(ag[,2])
          ag[,3] <- as.character(ag[,3])

          ag <- ag[order(ag[,3]),]

          colnames(ag)[1] <- paste(input$replyXaxis4, collapse = " ")
          colnames(ag)[2] <- paste(input$replyLine4 , collapse = " ")
          colnames(ag)[3] <- paste(input$replyPlot4 , collapse = " ")
          colnames(ag)[4] <- input$replyMethod4

          colnames(ag) <- make.unique(names(ag))

          return(ag)
        }
        else
          return(data.frame())
      })

      output$selScale4 <- renderUI(
      {
        selectInput('replyScale4', 'Scale:', optionsScale()[1:(length(optionsScale())-1)], selected = optionsScale()[1], selectize=FALSE, multiple=FALSE)
      })

      output$selMethod4 <- renderUI(
      {
        options <- c("Fox & Jacewicz (2009) TL"     = "TL",
                     "Fox & Jacewicz (2009) TL_roc" = "TL_roc")

        selectInput('replyMethod4', 'Method:', options, selected = options[1], selectize=FALSE, multiple=FALSE)
      })

      output$selGraph4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        options <- c("Dot plot","Bar chart")
        selectInput('replyGraph4', 'Select graph type:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      output$selVar4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        options <- c("f0","F1","F2","F3")
        selectInput('replyVar4', 'Variable:', options, selected = character(0), multiple=TRUE, selectize=FALSE, width="100%", size=4)
      })

      output$selTimes4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        timeCode   <- getTimeCode()
        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        nColumns   <- ncol(vowelTab())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        selectInput('replyTimes4', 'Points:', timeCode, multiple=TRUE, selectize=FALSE, selected = character(0), width="100%")
      })

      output$selXaxis4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyXaxis4', 'Var. x-axis:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catXaxis4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyXaxis4)>0)
          options <- unique(fuseCols(vowelTab(),input$replyXaxis4))
        else
          options <- NULL

        selectInput('catXaxis4', 'Sel. categ.:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      output$selLine4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        indexVowel <- grep("^vowel$",options)
        selectInput('replyLine4', 'Color var.:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catLine4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyLine4)>0)
          options <- unique(fuseCols(vowelTab(),input$replyLine4))
        else
          options <- NULL

        selectInput('catLine4', 'Sel. colors:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      output$selPlot4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyPlot4', 'Panel var.:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catPlot4 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyPlot4)>0)
          options <- unique(fuseCols(vowelTab(),input$replyPlot4))
        else
          options <- NULL

        selectInput('catPlot4', 'Sel. panels:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      scaleLab4 <- function()
      {
        return(scaleLab(input$replyScale4))
      }

      plotGraph4 <- function()
      {
        if (is.null(vowelSub4()) || (nrow(vowelSub4())==0))
          return(NULL)

        if (input$selError4=="0%")
          w <- 0
        if (input$selError4=="90%")
          w <- 0.4
        if (input$selError4=="95%")
          w <- 0.4
        if (input$selError4=="99%")
          w <- 0.4

        if (is.element("rotate x-axis labels",input$selGeon4))
          Angle = 90
        else
          Angle = 0

        if (((length(input$catLine4)==0) | (length(input$catLine4)>14)) && (length(input$catPlot4)==0))
        {
          if (input$replyGraph4=="Dot plot")
          {
            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,2], group=1)) +
                  geom_point(colour="indianred2", size=3) +
                  geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }
          else

          if (input$replyGraph4=="Bar chart")
          {
            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,2])) +
                  geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }
        }
        else

        if (((length(input$catLine4)==0) | (length(input$catLine4)>14)) && (length(input$catPlot4)>0))
        {
          if (input$replyGraph4=="Dot plot")
          {
            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,3], group=1)) +
                  geom_point(colour="indianred2", size=3) +
                  geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  facet_wrap(~vowelSub4()[,2]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }

          else

          if (input$replyGraph4=="Bar chart")
          {
            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,3])) +
                  geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  facet_wrap(~vowelSub4()[,2]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }
          else {}
        }
        else

        if (((length(input$catLine4)>0) & (length(input$catLine4)<=14)) && (length(input$catPlot4)==0))
        {
          if (input$replyGraph4=="Dot plot")
          {
            pd <- position_dodge(0.7)

            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,3], group=vowelSub4()[,2], color=vowelSub4()[,2])) +
                  geom_point(size=3, position=pd) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  scale_colour_discrete(name=paste0(paste(input$replyLine4, collapse = " "),"\n")) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'points'),
                        aspect.ratio   =0.67)
          }
          else

          if (input$replyGraph4=="Bar chart")
          {
            pd <- position_dodge(0.9)

            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,3], fill=vowelSub4()[,2])) +
                  geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  scale_fill_hue(name=paste0(paste(input$replyLine4, collapse = " "),"\n")) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'lines'),
                        aspect.ratio   =0.67)
          }
          else {}

          if (input$replyXaxis4==input$replyLine4)
            gp <- gp + theme(axis.title.x=element_blank(),
                             axis.text.x =element_blank(),
                             axis.ticks.x=element_blank())
          else {}
        }
        else

        if (((length(input$catLine4)>0) & (length(input$catLine4)<=14))  && (length(input$catPlot4)>0))
        {
          if (input$replyGraph4=="Dot plot")
          {
            pd <- position_dodge(0.5)

            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,4], group=vowelSub4()[,2], color=vowelSub4()[,2])) +
                  geom_point(size=3, position=pd) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  scale_colour_discrete(name=paste0(paste(input$replyLine4, collapse = " "),"\n")) +
                  facet_wrap(~vowelSub4()[,3]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'points'),
                        aspect.ratio   =0.67)
          }
          else

          if (input$replyGraph4=="Bar chart")
          {
            pd <- position_dodge(0.9)

            gp <- ggplot(data=vowelSub4(), aes(x=vowelSub4()[,1], y=vowelSub4()[,4], fill=vowelSub4()[,2])) +
                  geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title4) +
                  xlab(paste(input$replyXaxis4, collapse = " ")) + ylab(paste0(input$replyMethod4," ", paste(input$replyVar4,collapse = ' ')," (",scaleLab4(),")")) +
                  scale_fill_hue(name=paste0(paste(input$replyLine4, collapse = " "),"\n")) +
                  facet_wrap(~vowelSub4()[,3]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint4b), family=input$replyFont4b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'lines'),
                        aspect.ratio   =0.67)
          }
          else {}

          if (input$replyXaxis4==input$replyLine4)
            gp <- gp + theme(axis.title.x=element_blank(),
                             axis.text.x =element_blank(),
                             axis.ticks.x=element_blank())
          else {}
        }
        else {}

        return(graphics::plot(gp))
      }

      res4 <- function()
      {
        if (length(input$replySize4b)==0)
          return(72)

        if (input$replySize4b=="small")
          return(36)
        if (input$replySize4b=="medium")
          return(72)
        if (input$replySize4b=="large")
          return(90)
      }

      observeEvent(input$replySize4b,
      {
        output$graph4 <- renderPlot(height = 550, width = 700, res = res4(),
        {
          if ((length(input$replyVar4)>0) & (length(input$replyTimes4)>1) & (length(input$catXaxis4)>0))
          {
            plotGraph4()
          }
        })
      })

      output$Graph4 <- renderUI(
      {
        plotOutput("graph4", height="627px")
      })

      output$selFormat4a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat4a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName4a <- function()
      {
        return(paste0("dynamicsTable.",input$replyFormat4a))
      }

      output$download4a <- downloadHandler(filename = fileName4a, content = function(file)
      {
        if ((length(input$replyVar4)>0) & (length(input$replyTimes4)>1) & (length(input$catXaxis4)>0))
        {
          vT <- vowelSub4()

          colnames(vT)[which(colnames(vT)=="sd")] <- "standard deviation"
          colnames(vT)[which(colnames(vT)=="se")] <- "standard error"
          colnames(vT)[which(colnames(vT)=="n" )] <- "number of observations"
          colnames(vT)[which(colnames(vT)=="ll")] <- "lower limit"
          colnames(vT)[which(colnames(vT)=="ul")] <- "upper limit"
        }
        else
          vT <- data.frame()

        if (input$replyFormat4a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

        if (input$replyFormat4a=="xlsx")
        {
          WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
        }
        else {}
      })

      output$selSize4b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize4b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selFont4b <- renderUI(
      {
        options <- c("Courier" = "Courier", "Helvetica" = "Helvetica", "Times" = "Times")
        selectInput('replyFont4b', label=NULL, options, selected = "Helvetica", selectize=FALSE, multiple=FALSE)
      })

      output$selPoint4b <- renderUI(
      {
        options <- c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
        selectInput('replyPoint4b', label=NULL, options, selected = 22, selectize=FALSE, multiple=FALSE)
      })

      output$selFormat4b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
        selectInput('replyFormat4b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
      })

      fileName4b <- function()
      {
        return(paste0("dynamicsPlot.",input$replyFormat4b))
      }

      output$download4b <- downloadHandler(filename = fileName4b, content = function(file)
      {
        grDevices::pdf(NULL)

        scale  <- 72/res4()
        width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
        height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
        
        if ((length(input$replyVar4)>0) & (length(input$replyTimes4)>1) & (length(input$catXaxis4)>0) && (nrow(vowelSub4())>0))
          plot <- plotGraph4()
        else
          plot <- ggplot()+theme_bw()
          
        show_modal_spinner()
        
        if (input$replyFormat4b=="JPG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
        else
        if (input$replyFormat4b=="PNG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
        else
        if (input$replyFormat4b=="SVG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
        else
        if (input$replyFormat4b=="EPS")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
        else
        if (input$replyFormat4b=="PDF")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
        else
        if (input$replyFormat4b=="TEX")
        {
          tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
          print(plot)
        }
        else {}

        grDevices::graphics.off()
        
        remove_modal_spinner()
      })

      ##########################################################################

      vowelNorm2 <- reactive(
      {
        return(vowelNormD(vowelTab(),input$replyNormal2))
      })

      vowelSub2 <- reactive(
      {
        if (is.null(vowelNorm2()) || (nrow(vowelNorm2())==0) || (length(input$catXaxis2)==0))
          return(NULL)

        vT <- vowelNorm2()

        indexVowel <- grep("^vowel$", colnames(vT))

        if (any(is.na(vT[,indexVowel+1])))
          vT[,indexVowel+1] <- 0

        vT$indexXaxis <- fuseCols(vowelNorm2(),input$replyXaxis2)
        vT$indexLine  <- fuseCols(vowelNorm2(),input$replyLine2)
        vT$indexPlot  <- fuseCols(vowelNorm2(),input$replyPlot2)

        if (input$selError2=="0%")
          z <- 0
        if (input$selError2=="90%")
          z <- 1.645
        if (input$selError2=="95%")
          z <- 1.96
        if (input$selError2=="99%")
          z <- 2.575

        vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis2))

        if (nrow(vT)==0)
          return(NULL)

        if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)==0))
        {
          if (is.element("average",input$selGeon2))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, speaker=vT$speaker, vowel=vT$vowel, duration=vT$duration)
            vT <- aggregate(duration ~ indexXaxis + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(duration ~ indexXaxis +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$duration ~ vT$indexXaxis, FUN=mean)
          ag$sd <- aggregate(vT$duration ~ vT$indexXaxis, FUN=sd)[,2]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$duration ~ vT$indexXaxis, FUN=length)[,2]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure2=="SD")
          {
            ag$ll <- ag[,2] - z * ag$sd
            ag$ul <- ag[,2] + z * ag$sd
          }
          if (input$selMeasure2=="SE")
          {
            ag$ll <- ag[,2] - z * ag$se
            ag$ul <- ag[,2] + z * ag$se
          }

          ag <- ag[order(ag[,2]),]
          ag[,1] <- factor(ag[,1], levels=ag[,1])

          colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
          colnames(ag)[2] <- "duration"

          return(ag)
        }
        else

        if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)>0))
        {
          vT <- subset(vT, is.element(vT$indexPlot,input$catPlot2))

          if (nrow(vT)==0)
            return(data.frame())

          if (is.element("average",input$selGeon2))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, indexPlot=vT$indexPlot, speaker=vT$speaker, vowel=vT$vowel, duration=vT$duration)
            vT <- aggregate(duration ~ indexXaxis + indexPlot + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(duration ~ indexXaxis + indexPlot +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexPlot, FUN=mean)
          ag$sd <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexPlot, FUN=sd)[,3]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexPlot, FUN=length)[,3]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure2=="SD")
          {
            ag$ll <- ag[,3] - z * ag$sd
            ag$ul <- ag[,3] + z * ag$sd
          }
          if (input$selMeasure2=="SE")
          {
            ag$ll <- ag[,3] - z * ag$se
            ag$ul <- ag[,3] + z * ag$se
          }

          ag <- ag[order(ag[,3]),]
          xx <- unique(ag[,1])

          ag0 <- data.frame()

          for (q in (1:length(xx)))
          {
            ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
          }

          ag <- ag0
          ag[,1] <- factor(ag[,1], levels=xx)
          ag[,2] <- as.character(ag[,2])

          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
          colnames(ag)[2] <- paste(input$replyPlot2 , collapse = " ")
          colnames(ag)[3] <- "duration"

          colnames(ag) <- make.unique(names(ag))

          return(ag)
        }
        else

        if (((length(input$catLine2)>0) & (length(input$catLine2)<=14)) && (length(input$catPlot2)==0))
        {
          vT <- subset(vT, is.element(vT$indexLine,input$catLine2))

          if (nrow(vT)==0)
            return(data.frame())

          if (is.element("average",input$selGeon2))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, indexLine=vT$indexLine, speaker=vT$speaker, vowel=vT$vowel, duration=vT$duration)
            vT <- aggregate(duration ~ indexXaxis + indexLine + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(duration ~ indexXaxis + indexLine +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexLine, FUN=mean)
          ag$sd <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexLine, FUN=sd)[,3]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexLine, FUN=length)[,3]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure2=="SD")
          {
            ag$ll <- ag[,3] - z * ag$sd
            ag$ul <- ag[,3] + z * ag$sd
          }
          if (input$selMeasure2=="SE")
          {
            ag$ll <- ag[,3] - z * ag$se
            ag$ul <- ag[,3] + z * ag$se
          }

          ag <- ag[order(ag[,3]),]
          xx <- unique(ag[,1])

          ag0 <- data.frame()

          for (q in (1:length(xx)))
          {
            ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
          }

          ag <- ag0
          ag[,1] <- factor(ag[,1], levels=xx)
          ag[,2] <- as.character(ag[,2])

          colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
          colnames(ag)[2] <- paste(input$replyLine2 , collapse = " ")
          colnames(ag)[3] <- "duration"

          colnames(ag) <- make.unique(names(ag))

          return(ag)
        }
        else

        if (((length(input$catLine2)>0) & (length(input$catLine2)<=14))  && (length(input$catPlot2)>0))
        {
          vT <- subset(vT, is.element(vT$indexLine,input$catLine2) & is.element(vT$indexPlot,input$catPlot2))

          if (nrow(vT)==0)
            return(data.frame())

          if (is.element("average",input$selGeon2))
          {
            vT <- data.frame(indexXaxis=vT$indexXaxis, indexPlot=vT$indexPlot, indexLine=vT$indexLine, speaker=vT$speaker, vowel=vT$vowel, duration=vT$duration)
            vT <- aggregate(duration ~ indexXaxis + indexPlot + indexLine + speaker + vowel, data=vT, FUN=mean)
            vT <- aggregate(duration ~ indexXaxis + indexPlot + indexLine +           vowel, data=vT, FUN=mean)
          }

          ag    <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=mean)
          ag$sd <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=sd)[,4]
          ag$sd[is.na(ag$sd)] <- 0
          ag$n  <- aggregate(vT$duration ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=length)[,4]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure2=="SD")
          {
            ag$ll <- ag[,4] - z * ag$sd
            ag$ul <- ag[,4] + z * ag$sd
          }
          if (input$selMeasure2=="SE")
          {
            ag$ll <- ag[,4] - z * ag$se
            ag$ul <- ag[,4] + z * ag$se
          }

          ag <- ag[order(ag[,4]),]
          xx <- unique(ag[,1])

          ag0 <- data.frame()

          for (q in (1:length(xx)))
          {
            ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
          }

          ag <- ag0
          ag[,1] <- factor(ag[,1], levels=xx)
          ag[,2] <- as.character(ag[,2])
          ag[,3] <- as.character(ag[,3])

          ag <- ag[order(ag[,3]),]

          colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
          colnames(ag)[2] <- paste(input$replyLine2 , collapse = " ")
          colnames(ag)[3] <- paste(input$replyPlot2 , collapse = " ")
          colnames(ag)[4] <- "duration"

          colnames(ag) <- make.unique(names(ag))

          return(ag)
        }
        else
          return(data.frame())
      })

      output$selNormal2 <- renderUI(
      {
        options <- c("None" = "",
                     "Lobanov (1971)" = " Lobanov")

        selectInput('replyNormal2', 'Normalization:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      output$selGraph2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        options <- c("Dot plot","Bar chart")
        selectInput('replyGraph2', 'Select graph type:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      output$selXaxis2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyXaxis2', 'Variable x-axis:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catXaxis2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyXaxis2)>0)
          options <- unique(fuseCols(vowelTab(),input$replyXaxis2))
        else
          options <- NULL

        selectInput('catXaxis2', 'Sel. categories:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      output$selLine2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        indexVowel <- grep("^vowel$",options)
        selectInput('replyLine2', 'Color variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catLine2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyLine2)>0)
          options <- unique(fuseCols(vowelTab(),input$replyLine2))
        else
          options <- NULL

        selectInput('catLine2', 'Select colors:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      output$selPlot2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyPlot2', 'Panel variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catPlot2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyPlot2)>0)
          options <- unique(fuseCols(vowelTab(),input$replyPlot2))
        else
          options <- NULL

        selectInput('catPlot2', 'Select panels:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      plotGraph2 <- function()
      {
        if (is.null(vowelSub2()) || (nrow(vowelSub2())==0))
          return(NULL)

        if (input$selError2=="0%")
          w <- 0
        if (input$selError2=="90%")
          w <- 0.4
        if (input$selError2=="95%")
          w <- 0.4
        if (input$selError2=="99%")
          w <- 0.4

        if (is.element("rotate x-axis labels",input$selGeon2))
          Angle = 90
        else
          Angle = 0

        if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)==0))
        {
          if (input$replyGraph2=="Dot plot")
          {
            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,2], group=1)) +
                  geom_point(colour="indianred2", size=3) +
                  geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }
          else

          if (input$replyGraph2=="Bar chart")
          {
            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,2])) +
                  geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }
        }
        else

        if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)>0))
        {
          if (input$replyGraph2=="Dot plot")
          {
            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3], group=1)) +
                  geom_point(colour="indianred2", size=3) +
                  geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  facet_wrap(~vowelSub2()[,2]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }

          else

          if (input$replyGraph2=="Bar chart")
          {
            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3])) +
                  geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  facet_wrap(~vowelSub2()[,2]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        aspect.ratio   =0.67)
          }
          else {}
        }
        else

        if (((length(input$catLine2)>0) & (length(input$catLine2)<=14)) && (length(input$catPlot2)==0))
        {
          if (input$replyGraph2=="Dot plot")
          {
            pd <- position_dodge(0.7)

            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3], group=vowelSub2()[,2], color=vowelSub2()[,2])) +
                  geom_point(size=3, position=pd) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  scale_colour_discrete(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'points'),
                        aspect.ratio   =0.67)
          }
          else

          if (input$replyGraph2=="Bar chart")
          {
            pd <- position_dodge(0.9)

            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3], fill=vowelSub2()[,2])) +
                  geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  scale_fill_hue(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'lines'),
                        aspect.ratio   =0.67)
          }
          else {}

          if (input$replyXaxis2==input$replyLine2)
            gp <- gp + theme(axis.title.x=element_blank(),
                             axis.text.x =element_blank(),
                             axis.ticks.x=element_blank())
          else {}
        }
        else

        if (((length(input$catLine2)>0) & (length(input$catLine2)<=14))  && (length(input$catPlot2)>0))
        {
          if (input$replyGraph2=="Dot plot")
          {
            pd <- position_dodge(0.5)

            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,4], group=vowelSub2()[,2], color=vowelSub2()[,2])) +
                  geom_point(size=3, position=pd) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  scale_colour_discrete(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                  facet_wrap(~vowelSub2()[,3]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'points'),
                        aspect.ratio   =0.67)
          }
          else

          if (input$replyGraph2=="Bar chart")
          {
            pd <- position_dodge(0.9)

            gp <- ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,4], fill=vowelSub2()[,2])) +
                  geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
                  geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                  ggtitle(input$title2) +
                  xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                  scale_fill_hue(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                  facet_wrap(~vowelSub2()[,3]) +
                  theme_bw() +
                  theme(text           =element_text(size=as.numeric(input$replyPoint2b), family=input$replyFont2b),
                        axis.text.x    =element_text(angle=Angle),
                        plot.title     =element_text(face="bold", hjust = 0.5),
                        legend.key.size=unit(1.5, 'lines'),
                        aspect.ratio   =0.67)
          }
          else {}

          if (input$replyXaxis2==input$replyLine2)
            gp <- gp + theme(axis.title.x=element_blank(),
                             axis.text.x =element_blank(),
                             axis.ticks.x=element_blank())
          else {}
        }
        else {}

        return(graphics::plot(gp))
      }

      res2 <- function()
      {
        if (length(input$replySize2b)==0)
          return(72)

        if (input$replySize2b=="small")
          return(36)
        if (input$replySize2b=="medium")
          return(72)
        if (input$replySize2b=="large")
          return(90)
      }

      observeEvent(input$replySize2b,
      {
        output$graph2 <- renderPlot(height = 550, width = 700, res = res2(),
        {
          if (length(input$catXaxis2)>0)
          {
            plotGraph2()
          }
        })
      })

      output$Graph2 <- renderUI(
      {
        plotOutput("graph2", height="627px")
      })

      output$selFormat2a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat2a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName2a <- function()
      {
        return(paste0("durationTable.",input$replyFormat2a))
      }

      output$download2a <- downloadHandler(filename = fileName2a, content = function(file)
      {
        if (length(input$catXaxis2)>0)
        {
          vT <- vowelSub2()

          colnames(vT)[which(colnames(vT)=="sd")] <- "standard deviation"
          colnames(vT)[which(colnames(vT)=="se")] <- "standard error"
          colnames(vT)[which(colnames(vT)=="n" )] <- "number of observations"
          colnames(vT)[which(colnames(vT)=="ll")] <- "lower limit"
          colnames(vT)[which(colnames(vT)=="ul")] <- "upper limit"
        }
        else
          vT <- data.frame()

        if (input$replyFormat2a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

        if (input$replyFormat2a=="xlsx")
        {
          WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
        }
        else {}
      })

      output$selSize2b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize2b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selFont2b <- renderUI(
      {
        options <- c("Courier" = "Courier", "Helvetica" = "Helvetica", "Times" = "Times")
        selectInput('replyFont2b', label=NULL, options, selected = "Helvetica", selectize=FALSE, multiple=FALSE)
      })

      output$selPoint2b <- renderUI(
      {
        options <- c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
        selectInput('replyPoint2b', label=NULL, options, selected = 22, selectize=FALSE, multiple=FALSE)
      })

      output$selFormat2b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
        selectInput('replyFormat2b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
      })

      fileName2b <- function()
      {
        return(paste0("durationPlot.",input$replyFormat2b))
      }

      output$download2b <- downloadHandler(filename = fileName2b, content = function(file)
      {
        grDevices::pdf(NULL)

        scale  <- 72/res2()
        width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
        height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
        
        if ((length(input$catXaxis2)>0) && (nrow(vowelSub2())>0))
          plot <- plotGraph2()
        else
          plot <- ggplot()+theme_bw()
        
        show_modal_spinner()
        
        if (input$replyFormat2b=="JPG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
        else
        if (input$replyFormat2b=="PNG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
        else
        if (input$replyFormat2b=="SVG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
        else
        if (input$replyFormat2b=="EPS")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
        else
        if (input$replyFormat2b=="PDF")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
        else    
        if (input$replyFormat2b=="TEX")
        {
          tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
          print(plot)
        }
        else {}

        grDevices::graphics.off()
        
        remove_modal_spinner()
      })

      ##########################################################################

      replyTimes30  <- reactive(input$replyTimes3)
      replyTimes3   <- debounce(replyTimes30 , 2000)

      replyTimesN30 <- reactive(input$replyTimesN3)
      replyTimesN3  <- debounce(replyTimesN30, 2000)

      selFormant30  <- reactive(input$selFormant3)
      selFormant3   <- debounce(selFormant30 , 2000)

      vowelScale3 <- reactive(
      {
        return(vowelScale(vowelSame(),input$replyScale3,0))
      })

      vowelNorm3 <- reactive(
      {
        if (length(input$replyNormal3)==0)
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelScale3()))
        nColumns   <- ncol(vowelScale3())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        if (!is.null(replyTimesN3()))
          replyTimesN <- replyTimesN3()
        else
          return(NULL)

        if (max(replyTimesN) > nPoints)
          replyTimesN <- Round(nPoints/2)
        else {}
        
        vL1 <- vowelLong1(vowelScale3(),replyTimesN)
        vL2 <- vowelLong2(vL1)
        vL3 <- vowelLong3(vL1)
        vL4 <- vowelLong4(vL1)

        return(vowelNormF(vowelScale3(),vL1,vL2,vL3,vL4,input$replyNormal3))
      })

      vowelSubS3 <- reactive(
      {
        if (is.null(vowelTab()) || (nrow(vowelTab())==0)  || (((input$selMetric3=="Euclidean") & (length(input$replyVowel3)<1)) | ((input$selMetric3=="Accdist") & (length(input$replyVowel3)<3))) || (length(replyTimes3())==0) || (length(selFormant3())==0))
          return(NULL)

        vT <- vowelNorm3()
        vT <- subset(vT, is.element(vT$vowel, input$replyVowel3))

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        nPoints <- (ncol(vowelTab()) - (indexVowel + 1))/5

        if ((nrow(vT)>0) && (max(as.numeric(replyTimes3()))<=nPoints))
        {
          vT0 <- data.frame()

          for (i in (1:length(replyTimes3())))
          {
            Code <- strtoi(replyTimes3()[i])

            indexF1 <- indexVowel + 4 + ((Code-1) * 5)
            indexF2 <- indexVowel + 5 + ((Code-1) * 5)
            indexF3 <- indexVowel + 6 + ((Code-1) * 5)

            vT0 <- rbind(vT0, data.frame(vowel   = vT$vowel    ,
                                         speaker = vT$speaker  ,
                                         time    = i           ,
                                         F1      = vT[,indexF1],
                                         F2      = vT[,indexF2],
                                         F3      = vT[,indexF3]))
          }

          return(aggregate(cbind(F1,F2,F3)~vowel+speaker+time, data=vT0, FUN=mean))
        }
        else
          return(data.frame())
      })

      vowelSubG3 <- reactive(
      {
        if (is.null(vowelSubS3()) || (nrow(vowelSubS3())==0) || (length(input$replyGrouping3)==0))
          return(NULL)

        vT0 <- unique(data.frame(speaker=vowelNorm3()$speaker,grouping=fuseCols(vowelNorm3(),input$replyGrouping3)))

        if (max(as.data.frame(table(vT0$speaker))$Freq)==1)
        {
          rownames(vT0) <- vT0$speaker
          vT0$speaker <- NULL
          vT <- vT0[as.character(vowelSubS3()$speaker),]
        }
        else
          vT <- rep("none",nrow(vowelSubS3()))

        return(data.frame(grouping=vT))
      })

      # Distances among speakers

      vowelCorS1 <- reactive(
      {
        if (is.null(vowelSubS3()) || (nrow(vowelSubS3())==0))
          return(NULL)

        vT <- vowelSubS3()
        vT$speaker <- as.character(vT$speaker)

        labs <- unique(vT$speaker)
        nl <- length(labs)

        corr <- matrix(0, nrow = nl, ncol = nl)

        rownames(corr) <- labs
        colnames(corr) <- labs

        withProgress(value = 0, style = "old",
        {
          for (i in (2:nl))
          {
            incProgress(1/nl, message = paste("Calculating ...", format((i/nl)*100, digits=0), "%"))

            iSub <- subset(vT, speaker==labs[i])

            for (j in (1:(i-1)))
            {
              jSub <- subset(vT, speaker==labs[j])

              if (is.element("F1",selFormant3()))
                difF1 <- (iSub$F1-jSub$F1)^2
              if (is.element("F2",selFormant3()))
                difF2 <- (iSub$F2-jSub$F2)^2
              if (is.element("F3",selFormant3()))
                difF3 <- (iSub$F3-jSub$F3)^2

              sumF <- rep(0, nrow(iSub))

              if (is.element("F1",selFormant3()))
                sumF <- sumF + difF1
              if (is.element("F2",selFormant3()))
                sumF <- sumF + difF2
              if (is.element("F3",selFormant3()))
                sumF <- sumF + difF3

              sumF <- sqrt(sumF)

              corr[i,j] <- mean(sumF)
              corr[j,i] <- corr[i,j]
            }
          }
        })

        for (i in (1:nl))
        {
          corr[i,i] <- 0
        }

        return(corr)
      })

      # Measure distances among vowels per speaker

      vDist <- function(vTsub)
      {
        vTsub$voweltime <- paste(vTsub$vowel,vTsub$time)

        labs <- unique(vTsub$voweltime)
        nl <- length(labs)

        vec <- c()
        k <- 0

        for (i in (2:nl))
        {
          iSub <- subset(vTsub, voweltime==labs[i])

          for (j in (1:(i-1)))
          {
            jSub <- subset(vTsub, voweltime==labs[j])

            k <- k + 1

            sqsum <- 0
            if (is.element("F1",selFormant3()))
              sqsum <- sqsum + (iSub$F1-jSub$F1)^2
            if (is.element("F2",selFormant3()))
              sqsum <- sqsum + (iSub$F2-jSub$F2)^2
            if (is.element("F3",selFormant3()))
              sqsum <- sqsum + (iSub$F3-jSub$F3)^2

            vec[k] <- sqrt(sqsum)
          }
        }

        return(vec)
      }

      # Measure within distances for all speakers

      wDist <- function(vT)
      {
        labs <- unique(vT$speaker)
        nl <- length(labs)

        spk <- c()
        vec <- c()

        withProgress(value = 0, style = "old",
        {
          for (i in (1:nl))
          {
            incProgress(1/nl, message = paste("Calculating part 1/2 ...", format((i/nl)*100, digits=0), "%"))

            iSub <- subset(vT, speaker==labs[i])
            iVec <- vDist(iSub)

            spk <- c(spk,rep(labs[i],length(iVec)))
            vec <- c(vec,iVec)
          }
        })

        return(data.frame(speaker=spk,dist=vec))
      }

      # Correlations among speakers

      vowelCorS2 <- reactive(
      {
        if (is.null(vowelSubS3()) || (nrow(vowelSubS3())==0))
          return(NULL)

        vT <- vowelSubS3()
        vT$speaker <- as.character(vT$speaker)

        vec <- wDist(vT)

        labs <- unique(vec$speaker)
        nl <- length(labs)

        corr <- matrix(0, nrow = nl, ncol = nl)

        rownames(corr) <- labs
        colnames(corr) <- labs

        withProgress(value = 0, style = "old",
        {
          for (i in (2:nl))
          {
            incProgress(1/nl, message = paste("Calculating part 2/2 ...", format((i/nl)*100, digits=0), "%"))

            iVec <- subset(vec, speaker==labs[i])$dist

            for (j in (1:(i-1)))
            {
              jVec <- subset(vec, speaker==labs[j])$dist

              if ((sd(iVec)>0) && (sd(jVec)>0))
              {
                corr[i,j] <- cor(iVec,jVec)
              }
              else
                corr[i,j] <- 0

              corr[j,i] <- corr[i,j]
            }
          }
        })

        for (i in (1:nl))
        {
          corr[i,i] <- 1
        }

        return(corr)
      })

      # Compare speakers

      vowelCorS <- reactive(
      {
        if (input$selMetric3=="Euclidean")
          return(vowelCorS1())

        if (input$selMetric3=="Accdist")
          return(vowelCorS2())
      })

      # Correlations among speakers of selected groupings

      vowelCorC <- reactive(
      {
        if (is.null(vowelSubG3()) || (nrow(vowelSubG3())==0))
          return(NULL)

        vT <- data.frame(speaker=vowelSubS3()$speaker,grouping=vowelSubG3())
        vT <- subset(vT, is.element(vT$grouping,input$catGrouping3))
        vT$speaker <- as.character(vT$speaker)

        labs <- unique(vT$speaker)

        if (length(labs)<5)
          return(NULL)

        return(vowelCorS()[labs,labs])
      })

      # Correlations among groupings

      vowelCorG <- reactive(
      {
        if (is.null(vowelSubG3()) || (nrow(vowelSubG3())==0))
          return(NULL)

        vT <- data.frame(speaker=vowelSubS3()$speaker,grouping=vowelSubG3())
        vT <- subset(vT, is.element(vT$grouping,input$catGrouping3))
        vT$speaker <- as.character(vT$speaker)

        labs <- unique(vT$grouping)
        nl <- length(labs)

        if (length(labs)<5)
          return(NULL)

        corr <- matrix(0, nrow = nl, ncol = nl)

        rownames(corr) <- labs
        colnames(corr) <- labs

        for (i in (2:nl))
        {
          iSub  <- subset(vT, grouping==labs[i])
          iLabs <- unique(iSub$speaker)

          for (j in (1:(i-1)))
          {
            jSub  <- subset(vT, grouping==labs[j])
            jLabs <- unique(jSub$speaker)

            subVowelCorC <- vowelCorC()[iLabs,jLabs]

            corr[i,j] <- mean(subVowelCorC)
            corr[j,i] <- corr[i,j]
          }
        }

        for (i in (1:nl))
          corr[i,i] <- 1

        return(corr)
      })

      # Measure correlations

      vowelCor3 <- reactive(
      {
        if (is.null(vowelSubS3()) || (nrow(vowelSubS3())==0) || is.null(vowelSubG3()) || (nrow(vowelSubG3())==0))
          return(NULL)

        if (!input$summarize3)
          vowelCor <- vowelCorC()
        else
          vowelCor <- vowelCorG()

        if (!is.null(vowelCor) && (nrow(vowelCor)>0) && (sd(vowelCor)>0))
          return(vowelCor)
        else
          return(NULL)
      })

      # Measure distances

      vowelDiff3 <- reactive(
      {
        if (is.null(vowelCor3()) || (nrow(vowelCor3())==0))
          return(NULL)

        if (input$selMetric3=="Euclidean")
          return(  vowelCor3())

        if (input$selMetric3=="Accdist"  )
          return(1-vowelCor3())
      })

      vowelDist3 <- reactive(
      {
        if (is.null(vowelDiff3()) || (nrow(vowelDiff3())==0))
          return(NULL)
        else
          return(as.dist(vowelDiff3(), diag=FALSE, upper=FALSE))
      })

      clusObj <- reactive(
      {
        if (input$replyMethod31=="S-L")
          clus <- hclust(vowelDist3(), method="single")

        if (input$replyMethod31=="C-L")
          clus <- hclust(vowelDist3(), method="complete")

        if (input$replyMethod31=="UPGMA")
          clus <- hclust(vowelDist3(), method="average")

        if (input$replyMethod31=="WPGMA")
          clus <- hclust(vowelDist3(), method="mcquitty")

        if (input$replyMethod31=="Ward")
          clus <- hclust(vowelDist3(), method="ward.D2")

        return(clus)
      })

      getPerplexity <- function()
      {
        if (nrow(vowelCor3()) < 91)
          return((nrow(vowelCor3())-1) %/% 3)
        else
          return(30)
      }

      multObj <- reactive(
      {
        if (input$replyMethod32=="Classical")
        {
          fit <- cmdscale(vowelDist3(), eig=TRUE, k=2)
          coords <- as.data.frame(fit$points)
        }

        if (input$replyMethod32=="Kruskal's")
        {
          fit <- isoMDS(vowelDist3(), k=2)
          coords <- as.data.frame(fit$points)
        }

        if (input$replyMethod32=="Sammon's")
        {
          fit <- sammon(vowelDist3(), k=2)
          coords <- as.data.frame(fit$points)
        }

        if (input$replyMethod32=="t-SNE")
        {
          fit <- Rtsne(vowelDist3(), check_duplicates=FALSE, pca=TRUE, perplexity=getPerplexity(), theta=0.5, dims=2)
          coords <- as.data.frame(fit$Y)
        }

        return(coords)
      })

      output$selTimes3 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimes3', 'Time points to be included:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$selScale3 <- renderUI(
      {
        selectInput('replyScale3', 'Scale:', optionsScale()[1:(length(optionsScale())-1)], selected = optionsScale()[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      output$selNormal3 <- renderUI(
      {
        if (is.null(vowelTab()) || length(input$replyScale3)==0)
          return(NULL)

        onlyF1F2 <- !is.element("F3", input$selFormant3)
        selectInput('replyNormal3', 'Normalization:', optionsNormal(vowelTab(), input$replyScale3, TRUE, onlyF1F2), selected = optionsNormal(vowelTab(), input$replyScale3, TRUE, onlyF1F2)[1], selectize=FALSE, multiple=FALSE)
      })

      output$selTimesN3 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyNormal3)>0) && ((input$replyNormal3=="") |
                                               (input$replyNormal3==" Peterson") |
                                               (input$replyNormal3==" Sussman") |
                                               (input$replyNormal3==" Syrdal & Gopal") |
                                               (input$replyNormal3==" Thomas & Kendall")))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimesN3', 'Normalization based on:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$selVowel3 <- renderUI(
      {
        if (is.null(vowelSame()))
          return(NULL)

        options <- unique(vowelSame()$vowel)
        selectInput('replyVowel3', 'Sel. vowels:', options, multiple=TRUE, selectize = FALSE, size=4, width="100%")
      })

      output$selGrouping3 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyGrouping3', 'Sel. variable:', options, selected = character(0), multiple=TRUE, selectize=FALSE, size=4, width="100%")
      })

      output$exclVow3 <- renderUI(
      {
        if (is.null(vowelTab()) || (nrow(vowelTab())==0))
          return(NULL)

        if ((length(input$replyVowel3)==0) && (length(vowelExcl())>0))
        {
          vowels <- ""

          for (i in 1:length(vowelExcl()))
            vowels <- paste(vowels, vowelExcl()[i])

          return(tags$div(HTML(paste0("<font color='black'>","Vowels excluded: ",vowels,"</font><br><br>"))))
        }
        else
          return(NULL)
      })

      output$catGrouping3 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyGrouping3)>0)
          options <- unique(fuseCols(vowelTab(),input$replyGrouping3))
        else
          options <- NULL

        selectInput('catGrouping3', 'Sel. categories:', options, multiple=TRUE, selectize = FALSE, size=4, width="100%")
      })

      output$selMethod3 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$selClass3)>0)
        {
          if (input$selClass3=="Cluster analysis")
            radioButtons('replyMethod31', NULL, c("S-L","C-L","UPGMA","WPGMA","Ward"), selected="UPGMA"    , TRUE)
          else
          if (input$selClass3=="Multidimensional scaling")
            radioButtons('replyMethod32', NULL, c("Classical","Kruskal's","Sammon's","t-SNE"), selected="Classical", TRUE)
          else {}
        }
        else
          return(NULL)
      })

      output$selGeon3 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$selClass3)>0) && (input$selClass3=="Multidimensional scaling"))
          checkboxGroupInput("mdsGeon3", "Options: ", c("points","labels","X\u21C4Y","inv. X","inv. Y"), selected=c("points","labels"), inline=TRUE)
        else
          return(NULL)
      })

      colPalette3 <- function(n)
      {
        return(colPalette(n,input$grayscale3))
      }

      plotClus <- function()
      {
        dendro <- dendro_data(as.dendrogram(clusObj()), type = "rectangle")

        gp <- ggplot(dendro$segments) +
              geom_segment(aes(x = x, y = y, xend = xend, yend = yend))

        speakers         <- as.character(label(dendro)$label)
        lookup           <- unique(data.frame(speaker=vowelNorm3()$speaker,grouping=fuseCols(vowelNorm3(),input$replyGrouping3)))
        rownames(lookup) <- lookup$speaker
        lookup$speaker   <- NULL
        groupings        <- lookup[speakers,]

        if (length(speakers)>90) fs <- 0.7 else
        if (length(speakers)>75) fs <- 1   else
        if (length(speakers)>60) fs <- 2   else
        if (length(speakers)>45) fs <- 3   else
        if (length(speakers)>30) fs <- 4   else
        if (length(speakers)>15) fs <- 5   else
        if (length(speakers)> 1) fs <- 6   else {}

        fs <- min(fs, convertUnit(unit(as.numeric(input$replyPoint3b), "pt"), "mm", valueOnly=TRUE))

        dendro$labels$label <- paste0("  ",dendro$labels$label)

        if ((input$replyGrouping3=="speaker") || (length(unique(groupings))==1) || (input$summarize3))
          gp <- gp + geom_text (data = dendro$labels, aes(x, y, label = label                  ), hjust = 0, angle = 0, family=input$replyFont3b, size = fs)
        else
          gp <- gp + geom_text (data = dendro$labels, aes(x, y, label = label, colour=groupings), hjust = 0, angle = 0, family=input$replyFont3b, size = fs)

        gp <- gp +
              scale_y_reverse(expand = c(0.5, 0)) +
              scale_color_manual(values=colPalette3(length(unique(groupings)))) +
              labs(colour=paste0(" ",paste(input$replyGrouping3, collapse = " "),"\n")) +
              coord_flip() +
              ggtitle(input$title3) +
              xlab(NULL) + ylab(NULL) +
              theme_bw() +
              theme(text            =element_text(size=as.numeric(input$replyPoint3b), family=input$replyFont3b),
                    plot.title      =element_text(face="bold", hjust = 0.5),
                    axis.text       =element_blank(),
                    axis.ticks      =element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    legend.key.size =unit(1.5,'lines')) +
              guides(color = guide_legend(override.aes = list(linetype = 0, shape=3)))

        print(gp)
      }

      plotMult <- function()
      {
        coords <- multObj()

        if (!is.element("X\u21C4Y", input$mdsGeon3))
        {
          Xlab <- "dimension 1"
          Ylab <- "dimension 2"
        }
        else
        {
          colnames(coords)[1] <- "V0"
          colnames(coords)[2] <- "V1"
          colnames(coords)[1] <- "V2"

          Xlab <- "dimension 2"
          Ylab <- "dimension 1"
        }

        if (is.element("inv. X", input$mdsGeon3))
          coords$V1 <- -1 * coords$V1

        if (is.element("inv. Y", input$mdsGeon3))
          coords$V2 <- -1 * coords$V2

        speakers         <- as.character(rownames(as.matrix(vowelDist3())))
        lookup           <- unique(data.frame(speaker=vowelNorm3()$speaker,grouping=fuseCols(vowelNorm3(),input$replyGrouping3)))
        rownames(lookup) <- lookup$speaker
        lookup$speaker   <- NULL
        groupings        <- lookup[speakers,]

        if (length(speakers)>45) fs <- 3 else
        if (length(speakers)>30) fs <- 4 else
        if (length(speakers)>10) fs <- 5 else
        if (length(speakers)> 1) fs <- 6 else {}

        fs <- min(fs, convertUnit(unit(as.numeric(input$replyPoint3b), "pt"), "mm", valueOnly=TRUE))

        if ((input$replyGrouping3=="speaker") || (length(unique(groupings))==1) || (input$summarize3))
          gp <- ggplot(coords, aes(V1, V2, label = rownames(as.matrix(vowelDist3()))                 ))
        else
          gp <- ggplot(coords, aes(V1, V2, label = rownames(as.matrix(vowelDist3())), color=groupings))

        if (is.element("points", input$mdsGeon3) &
            is.element("labels", input$mdsGeon3))
          gp <- gp + geom_point(size = 2.0) + geom_text_repel(family=input$replyFont3b, size = fs, show.legend=FALSE)
        else

        if (is.element("points", input$mdsGeon3))
          gp <- gp + geom_point(size = 2.5)
        else

        if (is.element("labels", input$mdsGeon3))
          gp <- gp + geom_text (family=input$replyFont3b, size = fs, family=input$replyFont3b)
        else {}

        gp <- gp +
              scale_x_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
              scale_y_continuous(breaks = 0, limits = c(min(coords$V1-0.01,coords$V2-0.01), max(coords$V1+0.01,coords$V2+0.01))) +
              scale_color_manual(values=colPalette3(length(unique(groupings)))) +
              labs(colour=paste0(" ",paste(input$replyGrouping3, collapse = " "),"\n")) +
              geom_vline(xintercept = 0, color="darkgrey") + geom_hline(yintercept = 0, color="darkgrey") +
              ggtitle(input$title3) +
              xlab(Xlab) + ylab(Ylab) +
              theme_bw() +
              theme(text           =element_text(size=as.numeric(input$replyPoint3b), family=input$replyFont3b),
                    plot.title     =element_text(face="bold", hjust = 0.5),
                    legend.key.size=unit(1.5,'lines'),
                    aspect.ratio   =1)

        print(gp)
      }

      plotGraph3 <- function()
      {
        if (is.null(vowelDist3()))
          return(NULL)

        if (length(input$selClass3)>0)
        {
          if ((length(input$replyMethod31)>0) && (input$selClass3=="Cluster analysis"        ))
            plotClus()
          else

          if ((length(input$replyMethod32)>0) && (input$selClass3=="Multidimensional scaling"))
            plotMult()
          else {}
        }
        else
          return(NULL)
      }

      res3 <- function()
      {
        if (length(input$replySize3b)==0)
          return(72)

        if (input$replySize3b=="small")
          return(36)
        if (input$replySize3b=="medium")
          return(72)
        if (input$replySize3b=="large")
          return(90)
      }

      observeEvent(input$replySize3b,
      {
        output$graph3 <- renderPlot(height = 550, width = 700, res = res3(),
        {
          if ((((input$selMetric3=="Euclidean") & (length(input$replyVowel3)>=1)) | ((input$selMetric3=="Accdist") & (length(input$replyVowel3)>=3))) && (length(input$replyGrouping3)>0) && (length(input$catGrouping3)>0) && (length(replyTimes3())>0) && (length(selFormant3())>0) && (!is.null(vowelCor3())))
          {
            plotGraph3()
          }
        })
      })

      output$Graph3 <- renderUI(
      {
        plotOutput("graph3", height="627px")
      })

      mdsDist <- function(coords)
      {
        nf <- nrow(coords)
        dist <- matrix(0, nrow = nf, ncol = nf)

        nd <- ncol(coords)

        for (i in 2:nf)
        {
          for (j in 1:(i-1))
          {
            sum <- 0

            for (k in 1:nd)
            {
              sum <- sum + (coords[i,k] - coords[j,k])^2
            }

            dist[i,j] <- sqrt(sum)
            dist[j,i] <- sqrt(sum)
          }
        }

        for (i in 1:nf)
          dist[i,i] <- 0

        return(as.dist(dist, diag=FALSE, upper=FALSE))
      }

      output$explVar3 <- renderUI(
      {
        if (is.null(vowelDist3()))
          return(NULL)

        if (length(input$selClass3)>0)
        {
          if ((length(input$replyMethod31)>0) && (input$selClass3=="Cluster analysis"))
          {
            explVar <- formatC(x=cor(vowelDist3(), cophenetic(clusObj()))^2, digits = 4, format = "f")
            return(tags$div(HTML(paste0("<font color='black'>","Explained variance: ",explVar,"</font><br><br>"))))
          }
          else

          if ((length(input$replyMethod32)>0) && (input$selClass3=="Multidimensional scaling"))
          {
            explVar <- formatC(x=cor(vowelDist3(), mdsDist(multObj()))^2, digits = 4, format = "f")
            return(tags$div(HTML(paste0("<font color='black'>","Explained variance: ",explVar,"</font><br><br>"))))
          }
          else {}
        }
        else
          return(NULL)
      })

      output$selFormat3a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat3a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName3a <- function()
      {
        return(paste0("exploreTable.",input$replyFormat3a))
      }

      output$download3a <- downloadHandler(filename = fileName3a, content = function(file)
      {
        if ((((input$selMetric3=="Euclidean") & (length(input$replyVowel3)>=1)) | ((input$selMetric3=="Accdist") & (length(input$replyVowel3)>=3))) && (length(input$replyGrouping3)>0) && (length(input$catGrouping3)>0) && (length(replyTimes3())>0) && (length(selFormant3())>0)  && (!is.null(vowelDiff3())))
        {
          vT <- data.frame(rownames(vowelDiff3()), vowelDiff3())
          colnames(vT) <- c("element", colnames(vowelDiff3()))
        }
        else
          vT <- data.frame()

        if (input$replyFormat3a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

        if (input$replyFormat3a=="xlsx")
        {
          WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, na = "NA", FreezeRow = 1, FreezeCol = 1, AdjWidth = TRUE)
        }
        else {}
      })

      output$selSize3b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize3b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selFont3b <- renderUI(
      {
        options <- c("Courier" = "Courier", "Helvetica" = "Helvetica", "Times" = "Times")
        selectInput('replyFont3b', label=NULL, options, selected = "Helvetica", selectize=FALSE, multiple=FALSE)
      })

      output$selPoint3b <- renderUI(
      {
        options <- c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,40,44,48,54,60)
        selectInput('replyPoint3b', label=NULL, options, selected = 22, selectize=FALSE, multiple=FALSE)
      })

      output$selFormat3b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF","TEX")
        selectInput('replyFormat3b', label=NULL, options, selected = "PNG", selectize=FALSE, multiple=FALSE)
      })

      fileName3b <- function()
      {
        return(paste0("explorePlot.",input$replyFormat3b))
      }

      output$download3b <- downloadHandler(filename = fileName3b, content = function(file)
      {
        grDevices::pdf(NULL)

        scale  <- 72/res3()
        width  <- convertUnit(x=unit(700, "pt"), unitTo="in", valueOnly=TRUE)
        height <- convertUnit(x=unit(550, "pt"), unitTo="in", valueOnly=TRUE)
        
        if ((length(input$replyVowel3)>=3) && (length(input$replyGrouping3)>0) && (length(input$catGrouping3)>0) && (length(replyTimes3())>0) && (length(selFormant3())>0) && (!is.null(vowelDiff3())))
          plot <- plotGraph3()
        else
          plot <- ggplot()+theme_bw()
        
        show_modal_spinner()
        
        if (input$replyFormat3b=="JPG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="jpeg")
        else
        if (input$replyFormat3b=="PNG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="png" )
        else
        if (input$replyFormat3b=="SVG")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device="svg" )
        else
        if (input$replyFormat3b=="EPS")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_ps )
        else
        if (input$replyFormat3b=="PDF")
          ggsave(filename=file, plot=plot, scale=scale, width=width, height=height, units="in", dpi=300, device=grDevices::cairo_pdf)
        else    
        if (input$replyFormat3b=="TEX")
        {
          tikzDevice::tikz(file=file, width=width, height=height, engine='xetex')
          print(plot)
        }
        else {}

        grDevices::graphics.off()
        
        remove_modal_spinner()
      })
      
      ##########################################################################
      
      global <- reactiveValues(replyScale5=NULL, replyNormal5=NULL)

      observeEvent(input$buttonHelp5, {
        showModal(modalDialog(easyClose = TRUE, fade = FALSE,
          title =
          HTML(paste0("<span style='font-weight: bold; font-size: 17px;'>Evaluation of scale conversion and speaker normalization methods</span>")),

          HTML(paste0("<span style='font-size: 15px;'>

                       This tab is meant to be used in order to find the most suitable combination of a scale conversion method and a speaker normalization method for your data set.
                       Choose the settings and press the Go! button. Be prepared that running the evaluation procedures <b>may take some time</b>, depending on the size of your data set.

                       <br><br>

                       In case the speakers have pronounced different sets of vowels, the procedures are run on the basis of the set of vowels that are found across all speakers.
                       The vowels thus excluded are printed.

                       <br><br>

                       <span style='font-weight: bold;'>Evaluate</span><br>

                       The results are presented as a table where the columns represent the scale conversion methods and the rows the normalization procedures.
                       Each score is shown on a background with a color somewhere in between turquoise and yellow.
                       The more yellow the background is, the better the result.
                       Note that for some tests larger scores represent better results, and for other tests smaller scores represent better results.

                       <br><br>

                       <span style='font-weight: bold;'>Compare</span><br>

                       After having selected the option 'Compare' one can choose to compare either scale conversion methods or speaker normalization methods.
                       When comparing the scale conversion methods, the unnormalized formant measurements are used.
                       When comparing the speaker normalization methods, the raw Hz formant measurements are used.

                       <br><br>

                       For more details about the implementation of the methods that are used in this tab type <span style='font-family: monospace; font-size: 90%;'>vignette('visvow')</span> in the R console and read section 6.

                       <br><br>

                       <span style='font-weight: bold;'>References</span><br>

                       Fabricius, A., Watt, D., & Johnson, D. E. (2009). A comparison of three speaker-intrinsic vowel formant frequency normalization algorithms for sociophonetics. <i>Language Variation and Change</i>, 21(3), 413-435.
                       <br>                   
                       Adank, P., Smits, R., & Van Hout, R. (2004). A comparison of vowel normalization procedures for language variation research. <i>The Journal of the Acoustical Society of America</i>, 116(5), 3099-3107.
                       <br>

                       </span>")),

          footer = modalButton("OK")
        ))
      })

      output$selTimes5 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimes5', 'Time points to be included:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$selTimesN5 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimesN5', 'Normalization based on:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$selVars51 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))

        if (indexVowel > 2)
          options <- c(colnames(vowelTab()[2:(indexVowel-1)]))
        else
          options <- NULL

        selectInput('replyVars51', 'Anatomic var(s):', options, selected=character(0), multiple=TRUE, selectize=FALSE, size=5, width="100%")
      })

      output$selVars52 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))

        if (indexVowel > 2)
          options <- c(colnames(vowelTab()[2:(indexVowel-1)]))
        else
          options <- NULL

        selectInput('replyVars52', 'Socioling. var(s):', options, selected=character(0), multiple=TRUE, selectize=FALSE, size=5, width="100%")
      })

      emptyF0 <- reactive(
      {
        req(vowelTab())
        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        return(sum(vowelTab()[,indexVowel+3]==0)==nrow(vowelTab()))
      })
      
      emptyF3 <- reactive(
      {
        req(vowelTab())
        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        return(sum(vowelTab()[,indexVowel+6]==0)==nrow(vowelTab()))
      })
      
      output$selF035 <- renderUI(
      {
        if (!emptyF0() | !emptyF3())
        {
          tL <- tagList(tags$b("Include:"))
            
          if (!emptyF0() & !emptyF3())
            return(tagList(tL, splitLayout(cellWidths = 38, 
                                           checkboxInput("replyF05", "f0", value = FALSE, width = NULL),
                                           checkboxInput("replyF35", "F3", value = FALSE, width = NULL))))
          else 
              
          if (!emptyF0())
            return(tagList(tL, splitLayout(cellWidths = 38, 
                                           checkboxInput("replyF05", "f0", value = FALSE, width = NULL))))
          else
              
          if (!emptyF3())
            return(tagList(tL, splitLayout(cellWidths = 38, 
                                             checkboxInput("replyF35", "F3", value = FALSE, width = NULL))))
          else
            return(NULL)
        }
      })

      output$exclVow5 <- renderUI(
      {
        if (is.null(vowelTab()) || (nrow(vowelTab())==0))
          return(NULL)

        if (length(vowelExcl())>0)
        {
          vowels <- ""

          for (i in 1:length(vowelExcl()))
            vowels <- paste(vowels, vowelExcl()[i])

          return(tags$div(HTML(paste0("<font color='black'>","Vowels excluded: ",vowels,"</font><br>"))))
        }
        else
          return(NULL)
      })

      output$getOpts5 <- renderUI(
      {
        req(input$selMeth5)

        if (input$selMeth5=="Evaluate")
        {
          return(radioButtons(inputId  = 'selAuth5',
                              label    = 'Author:',
                              choices  = c("Adank et al. (2004)",
                                           "Fabricius et al. (2009)"),
                              selected =   "Fabricius et al. (2009)",
                              inline   = FALSE))
        }

        if (input$selMeth5=="Compare")
        {
          return(radioButtons(inputId  = 'selConv5',
                              label    = 'Conversion:',
                              choices  = c("Scaling",
                                           "Normalization"),
                              selected =   "Scaling",
                              inline   = FALSE))
        }
      })

      output$getEval5 <- renderUI(
      {
        req(input$selAuth5)

        if ((input$selMeth5=="Evaluate") & (input$selAuth5=="Adank et al. (2004)"))
        {
          return(radioButtons(inputId  = 'selEval52',
                              label    = 'Method:',
                              choices  = c("preserve phonemic variation",
                                           "minimize anatomic variation",
                                           "preserve sociolinguistic variation"),
                              selected =   "preserve phonemic variation",
                              inline   = FALSE))
        }
        
        if ((input$selMeth5=="Evaluate") & (input$selAuth5=="Fabricius et al. (2009)"))
        {
          return(radioButtons(inputId  = 'selEval51',
                              label    = 'Method:',
                              choices  = c("equalize vowel space areas",
                                           "improve vowel space overlap"),
                              selected =   "equalize vowel space areas",
                              inline   = FALSE))
        }
      })

      output$goButton <- renderUI(
      {
        if (length(unique(vowelTab()$speaker)) > 1)
          return(div(style="text-align: center;", actionButton('getEval', 'Go!')))
        else
          return(div(style="text-align: center;", p("You need multiple speakers for this function.")))
      })
      
      vowelScale5 <- reactive(
      {
        return(vowelScale(vowelSame(), global$replyScale5, 50))
      })

      vowelNorm5 <- reactive(
      {
        indexVowel <- grep("^vowel$", colnames(vowelScale5()))
        nColumns   <- ncol(vowelScale5())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        if (!is.null(input$replyTimesN5))
          replyTimesN <- input$replyTimesN5
        else
          return(NULL)

        if (max(replyTimesN) > nPoints)
          replyTimesN <- Round(nPoints/2)
        else {}
        
        vL1 <- vowelLong1(vowelScale5(),replyTimesN)
        vL2 <- vowelLong2(vL1)
        vL3 <- vowelLong3(vL1)
        vL4 <- vowelLong4(vL1)

        return(vowelNormF(vowelScale5(), vL1, vL2, vL3, vL4, global$replyNormal5))
      })

      vowelSubS5 <- reactive(
      {
        if (is.null(vowelNorm5()) || (nrow(vowelNorm5())==0)  || (length(input$replyTimes5)==0))
          return(NULL)

        vT <- vowelNorm5()

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        nPoints <- (ncol(vowelTab()) - (indexVowel + 1))/5

        if ((nrow(vT)>0) && (max(as.numeric(input$replyTimes5))<=nPoints))
        {
          if ((!is.null(input$replyVars51)) && (length(input$replyVars51) > 0))
          {
            indices <- which(colnames(vT) %in% input$replyVars51)
            vT$vars1 <- unite(data=vT[,1:(indexVowel-1)], col="all", indices, sep = "_", remove = FALSE)$all
          }
          else
            vT$vars1 <- "none"

          if ((!is.null(input$replyVars52)) && (length(input$replyVars52) > 0))
          {
            indices <- which(colnames(vT) %in% input$replyVars52)
            vT$vars2 <- unite(data=vT[,1:(indexVowel-1)], col="all", indices, sep = "_", remove = FALSE)$all
          }
          else
            vT$vars2 <- "none"

          vT0 <- data.frame()

          for (i in (1:length(input$replyTimes5)))
          {
            Code <- strtoi(input$replyTimes5[i])

            indexF0 <- indexVowel + 3 + ((Code-1) * 5)
            indexF1 <- indexVowel + 4 + ((Code-1) * 5)
            indexF2 <- indexVowel + 5 + ((Code-1) * 5)
            indexF3 <- indexVowel + 6 + ((Code-1) * 5)

            if (any(is.na(vT[,indexF0])))
              vT[,indexF0] <- 0
            if (any(is.na(vT[,indexF1])))
              vT[,indexF1] <- 0
            if (any(is.na(vT[,indexF2])))
              vT[,indexF2] <- 0
            if (any(is.na(vT[,indexF3])))
              vT[,indexF3] <- 0

            vT0 <- rbind(vT0, data.frame(vowel   = vT$vowel    ,
                                         speaker = vT$speaker  ,
                                         time    = i           ,
                                         vars1   = vT$vars1    ,
                                         vars2   = vT$vars2    ,
                                         f0      = vT[,indexF0],
                                         F1      = vT[,indexF1],
                                         F2      = vT[,indexF2],
                                         F3      = vT[,indexF3]))
          }

          return(aggregate(cbind(f0,F1,F2,F3)~vowel+speaker+time+vars1+vars2, data=vT0, FUN=mean))
        }
        else
          return(data.frame())
      })

      asPolySet <- function(df, PID)
      {
        df$PID <- PID
        df$POS <- 1:nrow(df)
        return(df)
      }

      perc <- function(yp)
      {
        eql <- nrow(subset(yp, yp[,1]==yp[,2]))
        all <- nrow(yp)

        return((eql/all)*100)
      }

      round2 <- function(x, n=0)
      {
        scale<-10^n
        return(trunc(x*scale+sign(x)*0.5)/scale)
      }

      evalResults <- eventReactive(input$getEval,
      {
        req(vowelTab())

        Scale  <- unlist(optionsScale())
        Normal <- unlist(optionsNormal(vowelTab(), " Hz", !input$replyF05, !input$replyF35))

        allScalesAllowed <- c("",
                              " Peterson",
                              " Syrdal & Gopal",
                              " Thomas & Kendall",
                              " Gerstman",
                              " Lobanov",
                              " Watt & Fabricius",
                              " Fabricius et al.",
                              " Bigham",
                              " Heeringa & Van de Velde I" ,
                              " Heeringa & Van de Velde II")

        matrix1  <- matrix(NA, nrow = length(Normal), ncol = (length(Scale)-1))
        matrix2  <- matrix(NA, nrow = length(Normal), ncol = (length(Scale)-1))
        matrix3  <- matrix(NA, nrow = length(Normal), ncol = (length(Scale)-1))
        matrix4  <- matrix(NA, nrow = length(Normal), ncol = (length(Scale)-1))
        matrix5  <- matrix(NA, nrow = length(Normal), ncol = (length(Scale)-1))

         loop <- 0
        nLoop <- (length(Scale)-1) * length(Normal)

        withProgress(value = 0, style = "old",
        {
          for (i in 1:(length(Scale)-1))
          {
            global$replyScale5 <- Scale[i]

            for (j in 1:length(Normal))
            {
              loop <- loop + 1
              incProgress((1/nLoop), message = paste("Calculating ...", format((loop/(nLoop))*100, digits=0), "%"))

              if ((Scale[i]==" Hz") | is.element(Normal[j], allScalesAllowed))
              {
                global$replyNormal5 <- Normal[j]
                
                vT <- vowelSubS5(); if (is.null(vT)) next()
                
                # Adank et al. (2004)
                
                preds <- c()
                
                if (!emptyF0() && input$replyF05 && 
                    (sd(vT$f0) > 0.000001))
                  preds <- cbind(preds, vT$f0)
                
                if (sd(vT$F1) > 0.000001) 
                  preds <- cbind(preds, vT$F1)
                
                if (sd(vT$F2) > 0.000001) 
                  preds <- cbind(preds, vT$F2)
                
                if (!emptyF3() && input$replyF35 &&
                    (sd(vT$F3) > 0.000001))
                  preds <- cbind(preds, vT$F3)
                
                model <- lda(factor(vT$vowel)~preds)
                p <- predict(model)
                yp <- cbind(as.character(vT$vowel), as.character(p$class))
                matrix1[j,i] <- perc(yp)
                
                ##
                
                if (length(unique(vT$vars1)) > 1)
                {
                  Segments <- unique(data.frame(vowel=vT$vowel, time=vT$time))
                  
                  preds <- c()
                  
                  for (k in 1:nrow(Segments))
                  {
                    vTsub <- subset(vT, (vowel==Segments$vowel[k]) & (time==Segments$time[k]))
                    
                    if (!emptyF0() && input$replyF05 && 
                        (sd(vTsub$f0) > 0.000001))
                      preds <- cbind(preds, vTsub$f0)
                    
                    if (sd(vTsub$F1) > 0.000001) 
                      preds <- cbind(preds, vTsub$F1)
                    
                    if (sd(vTsub$F2) > 0.000001) 
                      preds <- cbind(preds, vTsub$F2)
                    
                    if (!emptyF3() && input$replyF35 &&
                        (sd(vTsub$F3) > 0.000001))
                      preds <- cbind(preds, vTsub$F3)
                  }
                  
                  model <- lda(x=preds[, 1:ncol(preds)], grouping=vTsub$vars1)
                  p <- predict(model)
                  yp <- cbind(as.character(vT$vars1), as.character(p$class))
                  matrix2[j,i] <- perc(yp)
                }
                else
                  matrix2[j,i] <- NA
                
                ##
                
                if (length(unique(vT$vars2)) > 1)
                {
                  Segments <- unique(data.frame(vowel=vT$vowel, time=vT$time))
                  
                  Perc <- c()
                  
                  for (k in 1:nrow(Segments))
                  {
                    vTsub <- subset(vT, (vowel==Segments$vowel[k]) & (time==Segments$time[k]))
                    
                    preds <- c()
                    
                    if (!emptyF0() && input$replyF05 && 
                        (sd(vTsub$f0) > 0.000001))
                      preds <- cbind(preds, vTsub$f0)
                    
                    if (sd(vTsub$F1) > 0.000001) 
                      preds <- cbind(preds, vTsub$F1)
                    
                    if (sd(vTsub$F2) > 0.000001) 
                      preds <- cbind(preds, vTsub$F2)
                    
                    if (!emptyF3() && input$replyF35 &&
                        (sd(vTsub$F3) > 0.000001))
                      preds <- cbind(preds, vTsub$F3)
                    
                    model <- lda(factor(vTsub$vars2)~preds)
                    p <- predict(model)
                    yp <- cbind(as.character(vTsub$vars2), as.character(p$class))
                    Perc <- c(Perc, perc(yp))
                  }
                  
                  matrix3[j,i] <- mean(Perc)
                }
                else
                  matrix3[j,i] <- NA
    
                # Fabricius et al. (2009)

                if (!input$replyF05 & !input$replyF35)
                {
                  fab1 <- rep(NA, length(input$replyTimes5))
                  fab2 <- rep(NA, length(input$replyTimes5))
              
                  for (t in (1:length(input$replyTimes5)))
                  {
                    speakers <- unique(vT$speaker)
              
                    area <- c()
                    polySet <- data.frame()

                    for (k in 1:length(speakers))
                    {
                      vTsub <- subset(vT, (speaker==speakers[k]) & (time==t))
                  
                      indices <- grDevices::chull(vTsub$F1, vTsub$F2)
                      area <- c(area, abs(polyarea(vTsub$F1[indices], vTsub$F2[indices])))
                  
                      polySet <- rbind(polySet, asPolySet(data.frame(X=vTsub$F1[indices], Y=vTsub$F2[indices]), k))
                    }
                  
                    fab1[t] <- (sd(area)/mean(area))^2

                    ##
                  
                    inter <- joinPolys(polySet, operation="INT"  )
                    union <- joinPolys(polySet, operation="UNION")
                  
                    areaI <- abs(polyarea(inter$X, inter$Y))
                    areaU <- abs(polyarea(union$X, union$Y))
                  
                    fab2[t] <- areaI / areaU
                  }

                  if ((i==1) & (j==1))
                    fab10 <- fab1

                  fab1 <- 1 - (fab1/fab10)

                  matrix4[j,i] <- mean(fab1)
                  matrix5[j,i] <- mean(fab2)
                }
                else
                {
                  matrix4[j,i] <- NA
                  matrix5[j,i] <- NA
                }
              }
            }
          }
        })

        matrix1 <- round2(matrix1, n=3)
        matrix2 <- round2(matrix2, n=3)
        matrix3 <- round2(matrix3, n=3)
        matrix4 <- round2(matrix4, n=3)
        matrix5 <- round2(matrix5, n=3)

        matrix1  <- as.data.frame(matrix1)
        matrix2  <- as.data.frame(matrix2)
        matrix3  <- as.data.frame(matrix3)
        matrix4  <- as.data.frame(matrix4)
        matrix5  <- as.data.frame(matrix5)

        matrix1[is.na(matrix1)] <- "-"
        matrix2[is.na(matrix2)] <- "-"
        matrix3[is.na(matrix3)] <- "-"
        matrix4[is.na(matrix4)] <- "-"
        matrix5[is.na(matrix5)] <- "-"

        colnames(matrix1) <- Scale[1:(length(Scale)-1)]
        colnames(matrix2) <- Scale[1:(length(Scale)-1)]
        colnames(matrix3) <- Scale[1:(length(Scale)-1)]
        colnames(matrix4) <- Scale[1:(length(Scale)-1)]
        colnames(matrix5) <- Scale[1:(length(Scale)-1)]

        matrix1 <- cbind(c("None", as.character(Normal[2:length(Normal)])), matrix1)
        matrix2 <- cbind(c("None", as.character(Normal[2:length(Normal)])), matrix2)
        matrix3 <- cbind(c("None", as.character(Normal[2:length(Normal)])), matrix3)
        matrix4 <- cbind(c("None", as.character(Normal[2:length(Normal)])), matrix4)
        matrix5 <- cbind(c("None", as.character(Normal[2:length(Normal)])), matrix5)

        colnames(matrix1)[1] <- " "
        colnames(matrix2)[1] <- " "
        colnames(matrix3)[1] <- " "
        colnames(matrix4)[1] <- " "
        colnames(matrix5)[1] <- " "

        return(list(matrix1, matrix2, matrix3, matrix4, matrix5))
      })

      showResults1 <- eventReactive(input$getEval,
      {
        req(vowelTab())

        Scale  <- unlist(optionsScale())
        Normal <- unlist(optionsNormal(vowelTab(), " Hz", !input$replyF05, !input$replyF35))

        matrix6  <- matrix(NA, nrow = (length(Scale)-1), ncol = (length(Scale)-1))

        rownames(matrix6) <- Scale[1:(length(Scale)-1)]
        colnames(matrix6) <- Scale[1:(length(Scale)-1)]

         loop <- 0
        nLoop <- ((length(Scale)-1) * ((length(Scale)-1)-1))/2

        global$replyNormal5 <- ""

        withProgress(value = 0, style = "old",
        {
          for (i in 2:(length(Scale)-1))
          {
            global$replyScale5  <- Scale[i]
            vT1 <- vowelSubS5()

            for (j in 1:(i-1))
            {
              loop <- loop + 1
              incProgress((1/nLoop), message = paste("Calculating ...", format((loop/(nLoop))*100, digits=0), "%"))

              global$replyScale5  <- Scale[j]
              vT2 <- vowelSubS5()

              if (emptyF3() || !input$replyF35)
                Cor <- 1-((cor(vT1$F1, vT2$F1) + cor(vT1$F2, vT2$F2))/2)
              else
                Cor <- 1-((cor(vT1$F1, vT2$F1) + cor(vT1$F2, vT2$F2) + cor(vT1$F3, vT2$F3))/3)

              matrix6[i,j] <- Cor
              matrix6[j,i] <- Cor
            }
          }
        })

        for (i in 1:(length(Scale)-1))
        {
          matrix6[i,i] <- 0
        }

        return(matrix6)
      })

      showResults2 <- eventReactive(input$getEval,
      {
        req(vowelTab())

        Scale  <- unlist(optionsScale())
        Normal <- unlist(optionsNormal(vowelTab(), " Hz", !input$replyF05, !input$replyF35))

        matrix6  <- matrix(NA, nrow = length(Normal), ncol = length(Normal))

        rownames(matrix6) <- c(" None", Normal[2:length(Normal)])
        colnames(matrix6) <- c(" None", Normal[2:length(Normal)])

        global$replyScale5 <- " Hz"

         loop <- 0
        nLoop <- (length(Normal) * (length(Normal)-1))/2

        withProgress(value = 0, style = "old",
        {
          for (i in 2:length(Normal))
          {
            global$replyNormal5 <- Normal[i]
            vT1 <- vowelSubS5()

            for (j in 1:(i-1))
            {
              loop <- loop + 1
              incProgress((1/nLoop), message = paste("Calculating ...", format((loop/(nLoop))*100, digits=0), "%"))

              global$replyNormal5 <- Normal[j]
              vT2 <- vowelSubS5()

              if (emptyF3() || !input$replyF35)
                Cor <- 1-((cor(vT1$F1, vT2$F1) + cor(vT1$F2, vT2$F2))/2)
              else
                Cor <- 1-((cor(vT1$F1, vT2$F1) + cor(vT1$F2, vT2$F2) + cor(vT1$F3, vT2$F3))/3)

              matrix6[i,j] <- Cor
              matrix6[j,i] <- Cor
            }
          }
        })

        for (i in 1:length(Normal))
        {
          matrix6[i,i] <- 0
        }

        return(matrix6)
      })

      output$table5 <- renderFormattable(
      {
        if (length(unique(vowelTab()$speaker)) < 2)
          return(NULL)
        
        if (input$selAuth5=="Fabricius et al. (2009)")
          req(input$selEval51)

        if (input$selAuth5=="Adank et al. (2004)")
          req(input$selEval52)

        df <- data.frame()

        if ((input$selAuth5=="Adank et al. (2004)") && (input$selEval52 == "preserve phonemic variation"))
        {
          df <- evalResults()[[1]]
          col1 <- "turquoise"
          col2 <- "yellow"
        }

        if ((input$selAuth5=="Adank et al. (2004)") && (input$selEval52 == "minimize anatomic variation"))
        {
          df <- evalResults()[[2]]
          col1 <- "yellow"
          col2 <- "turquoise"
        }

        if ((input$selAuth5=="Adank et al. (2004)") && (input$selEval52 == "preserve sociolinguistic variation"))
        {
          df <- evalResults()[[3]]
          col1 <- "turquoise"
          col2 <- "yellow"
        }

        if ((input$selAuth5=="Fabricius et al. (2009)") && (input$selEval51 == "equalize vowel space areas"))
        {
          df <- evalResults()[[4]]
          col1 <- "turquoise"
          col2 <- "yellow"
        }

        if ((input$selAuth5=="Fabricius et al. (2009)") && (input$selEval51 == "improve vowel space overlap"))
        {
          df <- evalResults()[[5]]
          col1 <- "turquoise"
          col2 <- "yellow"
        }

        formattable(df, align = rep("l", 11), list(formattable::area() ~ color_tile(col1, col2)))
      })

      output$graph5 <- renderPlot(
      {
        req(input$selConv5)

        if (input$selConv5=="Scaling")
          clus <- hclust(as.dist(showResults1()), method="average")

        if (input$selConv5=="Normalization")
          clus <- hclust(as.dist(showResults2()), method="average")

        dendro <- dendro_data(as.dendrogram(clus), type = "rectangle")
        dendro$labels$label <- paste0("  ", dendro$labels$label)

        rownames(dendro$labels) <- 1:nrow(dendro$labels)

        gp <- ggplot(dendro$segments) +
              geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
              geom_text (data = dendro$labels, aes(x, y, label = label), hjust = 0, angle = 0, size = 4, family="Helvetica") +
              scale_y_reverse(expand = c(0.5, 0)) +
              coord_flip() +
              ggtitle('') +
              xlab(NULL) + ylab(NULL) +
              theme_bw() +
              theme(text            =element_text(size=22, family="Helvetica"),
                    plot.title      =element_text(face="bold", hjust = 0.5),
                    axis.text       =element_blank(),
                    axis.ticks      =element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank()) +
             guides(color = guide_legend(override.aes = list(linetype = 0, shape=3)))

        print(gp)
      })

      output$Graph5 <- renderUI(
      {
        if (input$selMeth5=="Evaluate")
        {
          fluidPage(
            style = "padding:0; margin:0; font-size: 90%",
            formattableOutput("table5", height="513px")
          )
        }
        else
        {
          fluidPage(
            style = "padding:0; margin:0;",
            plotOutput("graph5", height="513px")
          )
        }
      })
    }
  )
}

################################################################################
