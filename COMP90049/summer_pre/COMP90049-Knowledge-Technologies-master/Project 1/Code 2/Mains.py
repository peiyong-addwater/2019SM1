from Methods import formatedLocation
from Methods import formatedTweets
from Methods import normal_global
from Methods import normal_gram


'''
format Tweets and Location name files
'''
formatTweetFileName = formatedTweets("hongyaow_tweets.txt")
formatLocationFileName = formatedLocation("US-loc-names.txt")


'''
read formated locations and tweets file into lists
'''
tweet_list = []
t = open(formatTweetFileName, "r")
for tweet in t:    
    tweet_list.append(tweet.strip("\n"))
print "====== load formated tweets file done! ======"
t.close()

location_list = []
l = open(formatLocationFileName, "r")
for loc in l:
    location_list.append(loc.strip("\n"))
print "====== load formated location name file done! ======"
l.close()



''' normal_gram:
    threshold
        100: totally match
        75: almost match, has some misspelling
'''
threshold = 75 # threshold for similarity percentage
gramOut = open("gramDistanceResult.txt", "w")
for locationLine in location_list:
    locWord = locationLine.lower().split()
    
    for tweetLine in tweet_list:
        
        tweetWord = tweetLine.lower().split()
        similaritys = []
        
        for gram1 in locWord:
            for gram2 in tweetWord:
                if len(gram1) > 2 and len(gram2) > 2:
                    result = normal_gram(gram1, gram2)
                    if result != None:
                        similaritys.append(result)
            
            
            if len(similaritys) == 0:
                break      
                        
        if len(similaritys) >= len(locWord):
            #for val in similaritys:
                #print val
                
            totalSimilarity =  sum(similaritys)/len(similaritys)
            if totalSimilarity >= threshold:
                gramOut.write(locationLine + " :   " + tweetLine + "\n")
                #print totalSimilarity
                print locationLine + " :   " + tweetLine
        

print "====== all comparison done! ======"
gramOut.close()


''' normal_global:
    threshold:
            0: totally match
            45: 45% dissimilar
'''
threshold = 45 # threshold for dissimilarity percentage
levenOut = open("globalDistanceResult.txt", "w")
for locationLine in location_list:
    locWord = locationLine.lower().split()
    
    for tweetLine in tweet_list:
        
        tweetWord = tweetLine.lower().split()
        dissimilaritys = []
        
        for gram1 in locWord:
            for gram2 in tweetWord:
                if len(gram1) >2 and len(gram2) >2:
                    result = normal_global(gram1, gram2)
                    if result <= 1:
                        dissimilaritys.append(result)
            
            
            if len(dissimilaritys) == 0:
                break      
                        
        if len(dissimilaritys) >= len(locWord):
            #for val in similaritys:
                #print val
            if sum(dissimilaritys) == 0:
                totalDisSimilarity = 0
            else:
                totalDisSimilarity =  100 * (float(sum(dissimilaritys))/float(len(dissimilaritys)))   
            
            if totalDisSimilarity <= threshold:
                gramOut.write(locationLine + " :   " + tweetLine + "\n")
                #print totalSimilarity
                print locationLine + " :   " + tweetLine
        

print "====== all comparison done! ======"
levenOut.close()