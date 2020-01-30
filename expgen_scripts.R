

library(Hmisc)
library(tidyr)
library(data.table)
library(text2vec)
library(stringdist)
library(dplyr)
library(data.table)

# function to sample suitable sets from simlex
wordsampler = function(nwords, nstims, nsimilar, simthreshold_high, simthreshold_low, assocmax, editmin, simlex, simmat, allowduplicates, allowsameinitial, poslist, wordlen_min, wordlen_max){
  if(nsimilar*2 > nwords){stop("Requested number of similar words exceeds number of total words")}
  if(wordlen_min > wordlen_max){stop("Min word length cannot be > max word length!")}
  
  simlex = simlex %>% 
    filter(nchar(word1) %in% (wordlen_min:wordlen_max)  & 
             nchar(word2) %in% (wordlen_min:wordlen_max) ) %>% 
    filter(POS %in% poslist) #%>% 
  #filter(Assoc.USF. < assocmax) %>% 
  #filter({stringdist(word1,word2)>=editmin })
  vocab = unique(unlist(simlex[,1:2]))
  # nrow(simlex) # 481 pairs left
  
  s = simlex[
    which(
      simlex$SimLex999 >= simthreshold_high &  # similar
        simlex$Assoc.USF. < assocmax &           # but not associated
        stringdist(simlex$word1, simlex$word2) >= editmin # and not form-similar
    ) , ,drop=F
    ]
  # leaves 17 suitable candidate pairs only
  if(!allowsameinitial){
    s = s[which(
      substr(s$word1,1,1) != substr(s$word2,1,1)
    ),,drop=F]
  } # then 15
  
  stims = vector("list", nstims)
  vocabdist = stringdistmatrix(vocab, useNames = T) %>% as.matrix()
  globalx = c() # storage for all targets from all stims; used for allowduplicates check
  for(i in 1:nstims){
    # print(" ", quote=F); print(i)
    
    if(nsimilar>0){
      # Basic case, just one pair:
      # if required try first if n duplicates in stims already
      if(allowduplicates<nstims & i>1){
        x = s[sample(1:nrow(s), 1), c("word1", "word2"), drop=F]
        it=0
        while( sum(globalx %in% paste(x, collapse=" ")) >= allowduplicates ){
          it=it+1
          #cat(".") # debug
          if(it>1000){
            stop(paste("Found", i, "stims, but now stuck at finding more suitable target pairs, so stopping; adjust allowduplicates to higher value and restart."))
          }
          x = s[sample(1:nrow(s), 1), c("word1", "word2"), drop=F]
        }
      } else {
        x = s[sample(1:nrow(s), 1), c("word1", "word2"), drop=F]
      }
      
      
      # More than one pair within single stim:
      if(nsimilar>1){
        npairs = 1
        it=0
        while(npairs<nsimilar){ # skips if only 1 nsimilar pair requested  
          # look for similar pairs that would not be inter-similar, keep at it until found:
          it=it+1
          if(it>100){
            # to avoid local minima:
            print("Looking for 1+ pairs per stim, stuck, tried 100 times, seeding with a new one to get out of local minimum")
            it=0
            x = s[sample(1:nrow(s), 1), c("word1", "word2"), drop=F]
          }
          xnew = s[sample(1:nrow(s), 1), c("word1", "word2"), drop=F]
          # avoid duplicates, skip if found:
          xnewtmp = unlist(xnew, F,F)
          if( any(xnewtmp %in% unlist(x, F,F) ) ) next()
          # if stims must have unique pairs, also this check:
          if(allowduplicates<nstims){
            if( sum(globalx %in% paste(xnewtmp, collapse=" ") )>=allowduplicates ) next() 
            # (checking for first member is enough, given simlex has no duplicate pairs
            # so words are currently allowed to recur with allowduplicates, but pairs not
          }
          
          x2 = rbind(x, xnew)
          xm = max(
            c(simmat[x2[,1],x2[,1] ] %>% {diag(.)=0; return(.)}, 
              simmat[x2[,2],x2[,2] ] %>% {diag(.)=0; return(.)}
            ))
          xe = min(c(stringdistmatrix(unlist(x2, use.names = F)))) # dist object, diagonal excluded
          # if new pairs are ok, add (and keep looking for more if necessary):
          if(xm <= simthreshold_low & xe >= editmin){
            x = x2
            npairs=npairs+1
          }
        }
      }
      globalx = c(globalx, unname(apply(x,1, paste, collapse=" ")) ) 
      x = c(t(x), use.names = F) # dataframe to vector
    } else {
      x = c() # if no pairs should be similar
    } # done finding target pairs, now:
    
    
    ### keep adding y distractors until limit reached:
    nneeded = nwords-length(x)
    y = rep(NA, nneeded)  # distractor array
    # see if actually possible:
    if(nneeded>length(vocab)){
      stop("Requested word set is larger than Simlex, adjust or use larger pool")
    } else {
      if(nneeded>(length(vocab)/2)){
        print("Requested word set is half the size of Simlex, solution might be hard to find")
      }
    }
    # start:
    it=0
    while(any(is.na(y))){ # if the loop below fails, restart; brute force search basically
      if(it>1000){ 
        stop("Restarted distractor set search 1000 times already, couldn't find solution so stopping, maybe adjust something? (probably up simthreshold_low to 0.3 if size~20 word sets requested")
      }
      it=it+1
      candidates = sample(setdiff(vocab, x) ) # shuffle the vector so no need to keep sampling
      cl=length(candidates)
      i2=1
      for(ii in 1:nneeded){
        ok = c(F,F,F,F)
        while(!all(ok) & i2 <= cl ){
          y2 = candidates[i2]
          # test that new one has low similarity and form similarity to rest of y set:
          if(ii>1){
            ok[1] = all(simmat[y2, na.omit(y), drop=T] < simthreshold_low  )
            ok[3] = all(vocabdist[y2, na.omit(y), drop=T] >= editmin )
          } else {ok[c(1,3)]=T} # skip on first word onbviously
          # test that new one in y set is not similar to any in the x set:
          ok[2] = all(simmat[y2, x, drop=T] < simthreshold_low )
          ok[4] = all(vocabdist[y2, x, drop=T] >= editmin )
          i2 = i2+1
          #print(i2) # debug
        }
        if(!all(ok) | i2>cl ){
          break  # if couldn't find next word or ran out of candidates, the restart, try until works basically
        } else { # if candidate matches conditions then add and move on
          y[ii] = y2
        }
      }
      # print(it, quote=F) # debug
    } # this ends if required number of words found
    stims[[i]] = c(x,y)
    
  } # end stims for-loop
  attr(stims, "globalx") = globalx
  #stims = lapply(stims, unlist, use.names=F) # concatenate targets and distractors
  return(stims)
}



# function to generate an artificial lexicon
langgen = function(langlen, engdict, maxinitial, vowels, cons, transl){
  # homogenize kpt-gbd, w-v, z-s -- so filters also out similar sounding ones
  if(!is.null(transl)) engdict = translate(engdict, transl[1], transl[2])
  vowels = strsplit(vowels, "")[[1]]
  cons   = strsplit(cons, "")[[1]]
  
  syllables = expand.grid(cons, vowels, stringsAsFactors = F) %>%  
    apply(1,paste0, collapse="")
  # 60 combos
  partlist = replicate(langlen, syllables) %>% 
    as.data.frame(stringsAsFactors=F) %>% 
    as.list()
  lang = do.call(expand.grid, partlist) %>% 
    apply(1,paste0, collapse="") %>%
    setdiff(engdict)  # exclude english and english-sounding words
  # 2998 left
  print(paste("Generated a language of", length(lang), "unique words"))
  return(lang)
}

# function to go through stim word sample sets, and find a suitable artificial language lexicon
stimgen = function(stims_words, lang, nlang, editmin, cons, maxinitial){
  stimlist = vector("list", length(stims_words))
  vocabdist = stringdistmatrix(lang, unique(unlist(stims_words, use.names = F)), useNames = T )
  # langdist  = stringdistmatrix(lang, useNames = T) %>% as.matrix()
  cons=strsplit(cons, "")[[1]]
  ncons = length(cons)
  for(s in seq_along(stims_words)){
    candidates = sample(lang) # reshuffle language
    cl=length(candidates)
    i2=1
    y = rep(NA, nlang)
    for(ii in 1:nlang){
      ok = c(F,F,F)  # not super efficient, could speed up by next'ing loop at any fail
      while(!all(ok) & i2 <= cl ){
        y2 = candidates[i2]
        if(ii>1){
          # test that new one has low form similarity to rest of y set:
          ok[1] = all(as.vector(stringdistmatrix(y2, na.omit(y))) >= editmin )
          # and that initial consonant is not repeated too many times:
          ok[3] = all( table(substr(c(y,y2),1,1)) <= maxinitial)
        } else {ok[c(1,3)]=T} # skip on first word obviously
        # test that new one in y set is not form-similar to any in the x set:
        ok[2] = all(vocabdist[y2, stims_words[[s]], drop=T] >= editmin )
        i2 = i2+1
        #print(i2) # debug
      }
      if(!all(ok) | i2>cl ){
        stop(paste("Could not find artificial lang solution for stim no", s,
                   "(which is weird, the list should be huge, unless the input letter strings are tiny)"))
      } else { # if candidate matches conditions then add and move on
        y[ii] = y2
      }
    }
    stimlist[[s]] = y
  }
  return(stimlist)
}
