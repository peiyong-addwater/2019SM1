import re
'''==== read and format location name file ===='''
def formatedLocation(locationFileName):
    reader = open(locationFileName)
    line = reader.readline()
    Locations = []
    
    while line != '' and line != None: 
        line = line.strip("\n")
        newLine = re.sub("[^A-Za-z]", " ", line) # only match alphabet
        newLine = re.sub(' +', ' ', newLine) # replace multiple space with one
        newLine = newLine.strip() # delete front and back space
        Locations.append(newLine) 
        
        line = reader.readline()  
#        locationNum += 1 
    reader.close()

    #delete duplicate lines
    formatedLocations=[]
    for line in Locations:
        if line not in formatedLocations:
            formatedLocations.append(line)
            #print line
    
#    save into a new file
    fo = open("formatedLocation.txt", "w")
    for locas in formatedLocations:
        fo.write(locas + "\n");

    fo.close()
    print "====== format Location file done! ======"

#    return formatLocations 
    return "formatedLocation.txt"


'''==== read and format tweets file ===='''
def formatedTweets(tweetFileName):
    reader = open(tweetFileName)
    line = reader.readline()
    formatTweets = []
    lineNum = 0
    while line != '' and line != None: 
        tempTweet = line.split('\t')
    
        if len(tempTweet) == 4:  #    need delete bad format tweets lines
            newTweet = re.sub("[^A-Za-z]", " ", tempTweet[2]) # only match alphabet
            newTweet = re.sub(' +', ' ', newTweet) # replace multiple space with one
            newTweet = newTweet.strip() # delete front and back space
            formatTweets.append(newTweet) 
            lineNum += 1
        
        line = reader.readline()   

    reader.close()

#    save into a new file
    fo = open("formatedTweet.txt", "w")
    for tweet in formatTweets:
        fo.write(tweet + "\n");

    fo.close()
    print "====== format tweet file done! ====="
    
#    return formatTweets
    return "formatedTweet.txt"


def normal_gram(words1, words2):       
        word1gram = []
        for i in range(len(words1) - 1):
            word1gram.append(words1[i] + words1[i + 1])
#        print word1gram   
        
            
        
        word2gram = []             
        for i in range(len(words2) - 1):
            word2gram.append(words2[i] + words2[i + 1])           
#       print word2gram
            
        match = 0
        if len(word1gram) > len(word2gram):
            for gram in word2gram:
                if gram in word1gram:
                    match = match + 1
        else:
            for gram in word1gram:
                if gram in word2gram:
                    match = match + 1
            
#            print match            
        gramDistance = len(word1gram) + len(word2gram) - 2 * match        
#           print gramDistance
            
        if gramDistance > 2 or match == 0:
#           print "jump"
            return None
            
        similarity = (float(match) / float(max(len(word1gram), len(word2gram)))) * 100
        return similarity
    

def normal_global(str1, str2):
    if len(str1) <= 2 or len(str2) <= 2:
        return 100
    
    len_str1 = len(str1) + 1
    len_str2 = len(str2) + 1
    #create matrix
    matrix = [0 for n in range(len_str1 * len_str2)]
    #init x axis
    for i in range(len_str1):
        matrix[i] = i
    
    #init y axis
    for j in range(0, len(matrix), len_str1):
        if j % len_str1 == 0:
            matrix[j] = j // len_str1
 
    for i in range(1, len_str1):
        for j in range(1, len_str2):
            if str1[i-1] == str2[j-1]:
                cost = 0
            else:
                cost = 1
            matrix[j*len_str1+i] = min(matrix[(j-1)*len_str1+i]+1,
                    matrix[j*len_str1+(i-1)]+1,
                    matrix[(j-1)*len_str1+(i-1)] + cost)
 
    return matrix[-1]
