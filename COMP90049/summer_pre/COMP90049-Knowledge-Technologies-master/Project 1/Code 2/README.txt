This program contains 2 Python files:
	Methods.py
	Mains.py
The Python version is: 
	Python 2.7.10

‘Methods.py’ contains all methods need in this program, for example:
formatedLocation: format location name files into better version
formatedTweets: format tweets files into better version
normal_gram: n-gram distance algorithm method
normal_global: global edit distance algorithm method

‘Mains.py’ is main function, which takes two datasets (namely ‘US-loc-names.txt’ and ‘hongyaow_tweets.txt’) in and return n-gram and global edit distance searching results, respectively.




Run the program:
Put all python files in the same directory, e.g. c:\program\
Put two datasets files (namely ‘US-loc-names.txt’ and ‘hongyaow_tweets.txt’) in the same directory as python files, e.g. c:\program

In terminal:
First load to the directory (cd c:\program)
Then run the program by typing: python Mains.py

Two result files namely ‘globalDistanceResult.txt’ and ‘gramDistanceResult.txt’ will appear in the same directory after a very long time.
