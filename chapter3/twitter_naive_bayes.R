library(hash)

twitter_app_file_path = "/home/vitidn/mydata/repo_git/DataSmart/chapter3/twitter_app.txt"
twitter_other_file_path = "/home/vitidn/mydata/repo_git/DataSmart/chapter3/twitter_other.txt"
twitter_test_file_path = "/home/vitidn/mydata/repo_git/DataSmart/chapter3/twitter_test.txt"

countFrequency <- function(term,store_dict){
  if(! has.key(term,store_dict)[[term]]){
    store_dict[term] = 1
  }else{
    store_dict[term] = store_dict[[term]] + 1
  }
}

toTerms <- function(sentence){
  s = sentence
  s = paste(s,"",sep= " ")
  s = tolower(s)
  #don't wanna mess with URL
  s = gsub("\\. "," ",s)
  s = gsub(": "," ",s)
  #split by punctuations
  terms = strsplit(s,split="[ ?!;,]")
  terms = terms[[1]]
  #filter with length > 3
  terms = terms[nchar(terms) > 3]
  return(terms)
}

countWordFrequency <- function(file_path,store_dict){  
  conn <- file(file_path,open="r")
  line_reads <-readLines(conn)
  for (i in 1:length(line_reads)){
    terms = toTerms(line_reads[i])
    #fill term's frequency in store_dict
    sapply(terms,countFrequency,store_dict)  
    }
  
}

calculateNaiveBayesProb <- function(terms,term_dict){
  term_freqs = values(term_dict)
  #strip names out of the vector
  names(term_freqs) = c()
  sum_freq = sum(term_freqs) + 1 #add smoothing factor
  #for each term, count occurence
  term_probs = sapply(terms,
      f <- function(term,term_dict){ 
        if(has.key(term,term_dict)[[term]]){
          return(term_dict[[term]]+1)
        }else{
          return(1)
        }
    },
    term_dict
    )
  #turn each cout occurence into log-prob
  term_probs = sapply(term_probs,
    f <- function(x,sum_freq){
      log(x/sum_freq)
  },
  sum_freq
  )
  
  return(sum(term_probs))
}

predictAppOrOther <- function(app_freq_dict,other_freq_dict,file_path){
  conn <- file(file_path,open="r")
  line_reads <-readLines(conn)
  for (i in 1:length(line_reads)){
    terms = toTerms(line_reads[i])
    terms = unique(terms)
    
    app_prob = calculateNaiveBayesProb(terms,app_freq_dict)
    other_prob = calculateNaiveBayesProb(terms,other_freq_dict)
    
    predicted = ""
    if(app_prob >= other_prob){
      predicted = "App"
    }else{
      predicted = "Other"  
    }
    
    print(sprintf("line %d prediction:%s [App prob:%f Other prob:%f]",i,predicted,app_prob,other_prob))
  }
}

#count term-frequency in train's data file
app_freq_dict = hash()
countWordFrequency(twitter_app_file_path,app_freq_dict)
other_freq_dict = hash()
countWordFrequency(twitter_other_file_path,other_freq_dict)

#do the prediction
predictAppOrOther(app_freq_dict,other_freq_dict,twitter_test_file_path)
