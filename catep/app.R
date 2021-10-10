library("mirtCAT")
library("shinythemes")
library("RMariaDB")
library("RMySQL")
library("DBI")
library(shiny)
#source("C:/Users/GTUSR0313/Desktop/New_folder/catep/app.R", encoding="UTF-8")
#set.seed(1234)
nitems <- 24 #Toplam madde sayisi
ritems=15    #Reading madde sayisi
lnitems=2   
litems=nitems-ritems-lnitems # Puanlanan listening madde sayisi
itemnames <- paste0('Item.', 1:nitems)
a <- matrix(c(rlnorm(nitems/2, 0.2, 0.3), rnorm(nitems/4, 0, 0.3), numeric(nitems/2),
              rnorm(nitems/4, 0, 0.3), rlnorm(nitems/2, 0.2, 0.3)), nitems)
a[1:ritems,2]=0
a[(nitems-lnitems+1):(nitems),2]=0
a[(ritems+1):(nitems),2]=0
d <- matrix(nitems)
for (i in 1:nitems) d[i]=-2+(i-1)*.2
pars <- data.frame(a, d)
colnames(pars) <- c("a1", "a2", "d")
trait_cov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

mod <- generate.mirt_object(pars, itemtype = "2PL", latent_covariance = trait_cov)

coef(mod, simplify=TRUE)
questions <- answers <- character(nitems)
choices <- matrix(NA, nitems, 4)
stems=list(div(strong("Two items will be responded on this text"),br(),br(),HTML("Edinburgh is the capital city of Scotland. It is very historic and has many old buildings to visit. But perhaps it is most famous for its main street  Princes Street. Here can you find all types of shops and a lovely park to have a picnic in. So, what makes it different and special? Well in this street you can also find a castle. Edinburgh Castle is over 1000 years old and is still used by soldiers. It has a very famous gun called Mons Meg. At one oclock everyday you can hear this gun all over the city. The main railway station is also on the main street so many tourists come to visit Edinburgh by train and in a very short time they can visit the castle go shopping and have lunch. Edinburgh is my home city and I think it is a great place for people to visit."),br(),br(), strong("The main street in Edinburgh is unusual because")),
           div(HTML("Edinburgh is the capital city of Scotland. It is very historic and has many old buildings to visit. But perhaps it is most famous for its main street  Princes Street. Here can you find all types of shops and a lovely park to have a picnic in. So, what makes it different and special? Well in this street you can also find a castle. Edinburgh Castle is over 1000 years old and is still used by soldiers. It has a very famous gun called Mons Meg. At one oclock everyday you can hear this gun all over the city. The main railway station is also on the main street so many tourists come to visit Edinburgh by train and in a very short time they can visit the castle go shopping and have lunch. Edinburgh is my home city and I think it is a great place for people to visit."), br(),br(),strong ("What happens at one oclock every day?")),
           div(HTML("When it comes to thinking of the jobs which robots could never be able to do, you would probably come up with doctors and teachers first. Maybe, you think that robots can take over the jobs of cleaners and factory workers, but there are some jobs that require human interaction. However, we need to ask ourselves this question: Are we underestimating what robots are capable of? In fact, it may sound interesting, but in some situations, they do better at diagnosing sicknesses. In addition to this information, some patients may even feel more comfortable when they have to share personal information with a machine rather than an"),strong(" actual"),HTML(" human being."), br(),br(), HTML("According to the passage, actual has the closest meaning to")),
           div(HTML("Fairies take a great deal of place in todays childrens stories, magical creatures with wings. Almost everyone knows that they are pretty and they are considered to be friendly to people. Also, they do small magic to impress humans. Shakespeare is thought to be the pioneer when it comes to fairies and we can see many modern ideas about fairies thanks to him in the 18th and 19th centuries. Even though the origins of fairies go back to the Ancient Greeks, similar creatures can be seen in many other cultures, as well. There are some who believe that fairies were the gods of the previous, pagan religions which worshipped the nature. Later on, as they lost their significance and became less strong figures in the stories, the Greek and Roman Gods took over."), br(),br(), strong("According to the passage, it is clear that")),
           div(HTML("An influencer is a person who can have an effect on the decisions of other people who follow them because of their role in a specific area such as technology, fashion, or travel. Influencers generally have a vast number of followers who pay a great deal of attention to what they think when it comes to buying things. They have the ability to convince other people to buy things and thats why many companies consider influencers as an easy and direct way to reach potential customers. There are a lot of brands which are asking famous influencers to promote their products and make people buy what they are trying to sell. As influencers make a lot of money even when it comes to a single post on a social media website, it is no wonder to think that many people would like to be influencers, as well."),br(),br(),strong("What is the best title for the text?")),
           div(HTML("Do you use any of the social networking websites which are so popular these days, the places where you can connect up with friends and relatives and meet people who share the same interests as you? If youre younger you may use MySpace, young adults are more likely to be found on Facebook and busy professionals may prefer something like LinkedIn. But at least two of these sites have one thing in common: apart from being social spaces where you can meet and chat to people, share photos and other things, they have all added new verbs and nouns to our lives in the past couple of years. Some examples are that you can facebook your holiday photos (upload them to your Facebook page), facebook someone to see who they are (look him up in Facebook), facebook someone about a party (contact someone through their Facebook page) and ask permission to facebook someone (add them as a Facebook friend)."),br(),br(),strong("The text mainly discusses how___.")),
           div(strong("Three items will be responded on this text"),br(),br(),HTML("According to a United Nations FAO report published in 2011, almost half of the fruit and vegetables produced in the world go to waste, which occurs during production, transformation, transport, and even  notably in developed economies  after being bought by the final customer. In an ideal world, and as in the past, much of the food consumed in cities would be produced locally, not shipped thousands of miles as happens today. This wastage has a huge impact in terms of natural resources, particularly space and water, which in turn have huge implications for the global environment. In order to find a solution to this serious problem, Chicago's O'Hare airport has been turned into a home to an aeroponic garden, where people can see how vegetables can be grown in an environment that is not only without soil but even without any permanent medium in which to grow."),br(),br(), 
               HTML("In this experimental garden, plants are grown, as the name suggests, in the air, their roots hanging down in nothing. Not just air, obviously; these roots are regularly sprayed with a nutrient-rich solution that gives them just what they need for optimum growth and the only water used is what is actually taken up by the roots of plants when they are sprayed. Any water not used can be collected and reused. Therefore aeroponics reduces water waste to zero. The same goes for nutrients as chemical fertilizer are not needed any further. Reduced use of natural resources will be vital for ensuring a"),strong(" sustainable "), HTML("future for generations to come, even when the global population goes above 10 billion. If our world is to survive for much longer, we have no choice but to reduce the use of natural resources and cut out wastage.  Compared to traditional agriculture, aeroponics does both."),br(),br(),strong("The word 'sustainable' in paragraph 2 is closest in meaning to _.")),
           div(HTML("According to a United Nations FAO report published in 2011, almost half of the fruit and vegetables produced in the world go to waste, which occurs during production, transformation, transport, and even  notably in developed economies  after being bought by the final customer. In an ideal world, and as in the past, much of the food consumed in cities would be produced locally, not shipped thousands of miles as happens today. This wastage has a huge impact in terms of natural resources, particularly space and water, which in turn have huge implications for the global environment. In order to find a solution to this serious problem, Chicago's O'Hare airport has been turned into a home to an aeroponic garden, where people can see how vegetables can be grown in an environment that is not only without soil but even without any permanent medium in which to grow."),br(),br(), 
               HTML("In this experimental garden, plants are grown, as the name suggests, in the air, their roots hanging down in nothing. Not just air, obviously; these roots are regularly sprayed with a nutrient-rich solution that gives them just what they need for optimum growth and the only water used is what is actually taken up by the roots of plants when they are sprayed. Any water not used can be collected and reused. Therefore aeroponics reduces water waste to zero. The same goes for nutrients as chemical fertilizer are not needed any further. Reduced use of natural resources will be vital for ensuring a sustainable future for generations to come, even when the global population goes above 10 billion. If our world is to survive for much longer, we have no choice but to reduce the use of natural resources and cut out wastage.  Compared to traditional agriculture, aeroponics does both."), br(),br(),strong("Which of the following is an opinion based on the text?")),
           div(HTML("According to a United Nations FAO report published in 2011, almost half of the fruit and vegetables produced in the world go to waste, which occurs during production, transformation, transport, and even  notably in developed economies  after being bought by the final customer. In an ideal world, and as in the past, much of the food consumed in cities would be produced locally, not shipped thousands of miles as happens today. This wastage has a huge impact in terms of natural resources, particularly space and water, which in turn have huge implications for the global environment. In order to find a solution to this serious problem, Chicago's O'Hare airport has been turned into a home to an aeroponic garden, where people can see how vegetables can be grown in an environment that is not only without soil but even without any permanent medium in which to grow."),br(),br(), 
               HTML("In this experimental garden, plants are grown, as the name suggests, in the air, their roots hanging down in nothing. Not just air, obviously; these roots are regularly sprayed with a nutrient-rich solution that gives them just what they need for optimum growth and the only water used is what is actually taken up by the roots of plants when they are sprayed. Any water not used can be collected and reused. Therefore aeroponics reduces water waste to zero. The same goes for nutrients as chemical fertilizer are not needed any further. Reduced use of natural resources will be vital for ensuring a sustainable future for generations to come, even when the global population goes above 10 billion. If our world is to survive for much longer, we have no choice but to reduce the use of natural resources and cut out wastage.  Compared to traditional agriculture, aeroponics does both."),br(),br(),strong("The authors attitude towards aeroponics can best be described as ")),
           div(strong("Three items will be responded on this text"),br(),br(),HTML("The first image many people have of early education is a circle of small children, sitting cross-legged, paying attention (or sometimes not) to a teacher reading a book aloud and showing pictures to the class. And early education teachers across the world repeated this practice daily, that is, until a few months ago. As schools, teachers and families face the need for shifting to online teaching, one  specific question has arisen over how to perform these read alouds on Zoom, and Youtube, two of the platforms where many young students continue to learn. As this rapid shift to online education has become widespread, another important question has come further under the spotlight: Is reading a book to students online an infringement of copyright law?"),br(),br(), 
               HTML("To answer this question, Its important to understand why youre reading aloud to know if you can rely on fair use. Most suggest that developing a love of books in the early years is essential to support a lifelong joy of reading, as well as contributing to later success at school. For this reason, more and more early education teachers want to reach their students after hours by recording themselves reading books and posting them online for pupils to watch from home. However, technology presents a challenge when it comes to understanding and interpreting what is fair use of copyright law, currently a concern for many teachers. That is, they are unsure about whether its legal to read books to students or post them online. This situation highlights the uncertainties some have about how copyright law applies to common educational practices when teaching shifts quickly from classrooms to an online environment."),br(),br(), 
               HTML("The short answer seems story time is generally covered by fair use of the law. Although many well-intentioned practitioners have warned teachers against this practice, the fact is that copyright law permits many read-aloud activities online. As teachers and learners are still adapting to new educational environments, copyright concerns about reading aloud should not be among the challenges they face."),br(),br(),
               strong("The writer mentions the online platforms to ")),
           div(HTML("The first image many people have of early education is a circle of small children, sitting cross-legged, paying attention (or sometimes not) to a teacher reading a book aloud and showing pictures to the class. And early education teachers across the world repeated this practice daily, that is, until a few months ago. As schools, teachers and families face the need for shifting to online teaching, one  specific question has arisen over how to perform these read alouds on Zoom, and Youtube, two of the platforms where many young students continue to learn. As this rapid shift to online education has become widespread, another important question has come further under the spotlight: Is reading a book to students online an infringement of copyright law?"),br(),br(), 
               HTML("To answer this question, Its important to understand why youre reading aloud to know if you can rely on fair use. Most suggest that developing a love of books in the early years is essential to support a lifelong joy of reading, as well as contributing to later success at school. For this reason, more and more early education teachers want to reach their students after hours by recording themselves reading books and posting them online for pupils to watch from home. However, technology presents a challenge when it comes to understanding and interpreting what is fair use of copyright law, currently a concern for many teachers. That is, they are unsure about whether its legal to read books to students or post them online. This situation highlights the uncertainties some have about how copyright law applies to common educational practices when teaching shifts quickly from classrooms to an online environment."),br(),br(), 
               HTML("The short answer seems story time is generally covered by fair use of the law. Although many well-intentioned practitioners have warned teachers against this practice, the fact is that copyright law permits many read-aloud activities online. As teachers and learners are still adapting to new educational environments, copyright concerns about reading aloud should not be among the challenges they face."),br(),br(),
               strong("The second paragraph is mainly concerned with")),
           div(HTML("The first image many people have of early education is a circle of small children, sitting cross-legged, paying attention (or sometimes not) to a teacher reading a book aloud and showing pictures to the class. And early education teachers across the world repeated this practice daily, that is, until a few months ago. As schools, teachers and families face the need for shifting to online teaching, one  specific question has arisen over how to perform these read alouds on Zoom, and Youtube, two of the platforms where many young students continue to learn. As this rapid shift to online education has become widespread, another important question has come further under the spotlight: Is reading a book to students online an infringement of copyright law?"),br(),br(), 
               HTML("To answer this question, Its important to understand why you are reading aloud to know if you can rely on fair use. Most suggest that developing a love of books in the early years is essential to support a lifelong joy of reading, as well as contributing to later success at school. For this reason, more and more early education teachers want to reach their students after hours by recording themselves reading books and posting them online for pupils to watch from home. However, technology presents a challenge when it comes to understanding and interpreting what is fair use of copyright law, currently a concern for many teachers. That is, they are unsure about whether its legal to read books to students or post them online. This situation highlights the uncertainties some have about how copyright law applies to common educational practices when teaching shifts quickly from classrooms to an online environment."),br(),br(), 
               HTML("The short answer seems story time is generally covered by fair use of the law. Although many well-intentioned practitioners have warned teachers against this practice, the fact is that copyright law permits many read-aloud activities online. As teachers and learners are still adapting to new educational environments, copyright concerns about reading aloud should not be among the challenges they face."),br(),br(),
               strong("In the last paragraph, the authors attitude toward using digital storytelling is")),
           div(strong("Three items will be responded on this text"),br(),br(),HTML("Shooting for the stars has become an international competition. When we think about space exploration, the first thing that usually comes to mind is NASA, the American Space Agency. Perhaps ESA comes to mind  the European Space Agency. Some may even think of the Chinese Space Agency or the Russian Space Agency. Afterall, these are the most notable countries when it comes to space exploration. However, they are not the only countries who have their sights set on the stars. In recent years, many countries have entered the ring and are now fighting for a place in space. One such country is Turkey."),br(),br(), 
               HTML("Headquartered in Ankara, Turkey, the TUA, or Turkish Space Agency, was founded on December 13, 2018. Its stated, official, mission is to reduce dependence on foreign aerospace science and technology. Their mission also is to increase competitiveness in the international arena and to create a scientific and technological infrastructure while expanding research and development with an aim to develop new technologies. Some of the things they are working on to meet their goals include projects to develop new systems, facilities, tools and equipment related to space and aviation, including satellites, launch vehicles and systems, air vehicles, simulators, and space platforms."),br(),br(),
               HTML("One of the biggest projects the agency is currently working on is a trip to the moon. On February 9th, 2021, President Recep Tayyip Erdogan announced Turkeys National Space Program to the country and the rest of the world and proclaimed that Turkey would reach the moon with an unmanned vehicle in 2023, only 2 short years away. The goal is to reach the moon using only Turkish technology and components. One such component is the original hybrid rocket engine developed by DeltaV Uzay Teknolojileri A.S. The rocket, known as SORS, or Hybrid Probe Rocket in English, has passed its first stage of development by successfully igniting during a test in March in the city of Sile, the home of the ignition testing facility. On hand for the event was Prime Minister Varank, who gave the command to start the countdown to ignition. The rocket remained ignited for the full targeted time limit of 50 seconds and was regarded as a complete success. This time limit is"),strong(" substantial "),HTML("because it is the amount of time a rocket needs to remain ignited in order to break through Earths atmosphere and reach the inner limits of space."),br(),br(),
               HTML("In addition to the Moon Mission, President Erdogan announced plans that map out the next 10 years of the space program. While reaching the moon is a notable goal, the most significant component of the space program is Turkeys desire to make a profit off its endeavors. The country fully intends on commercializing its capabilities and technological applications. To meet these lofty goals, the plan also includes education and training as well as recruiting young people. The plan is for the space program to be a sustainable investment in the future of the country. For Turkeys Space program, the future is certainly looking stellar."),br(),br(),
               strong("According to the first paragraph, how many institutions lead the world in space exploration?")),
           div(HTML("Shooting for the stars has become an international competition. When we think about space exploration, the first thing that usually comes to mind is NASA, the American Space Agency. Perhaps ESA comes to mind  the European Space Agency. Some may even think of the Chinese Space Agency or the Russian Space Agency. Afterall, these are the most notable countries when it comes to space exploration. However, they are not the only countries who have their sights set on the stars. In recent years, many countries have entered the ring and are now fighting for a place in space. One such country is Turkey."),br(),br(), 
               HTML("Headquartered in Ankara, Turkey, the TUA, or Turkish Space Agency, was founded on December 13, 2018. Its stated, official, mission is to reduce dependence on foreign aerospace science and technology. Their mission also is to increase competitiveness in the international arena and to create a scientific and technological infrastructure while expanding research and development with an aim to develop new technologies. Some of the things they are working on to meet their goals include projects to develop new systems, facilities, tools and equipment related to space and aviation, including satellites, launch vehicles and systems, air vehicles, simulators, and space platforms."),br(),br(),
               HTML("One of the biggest projects the agency is currently working on is a trip to the moon. On February 9th, 2021, President Recep Tayyip Erdogan announced Turkeys National Space Program to the country and the rest of the world and proclaimed that Turkey would reach the moon with an unmanned vehicle in 2023, only 2 short years away. The goal is to reach the moon using only Turkish technology and components. One such component is the original hybrid rocket engine developed by DeltaV Uzay Teknolojileri A.S. The rocket, known as SORS, or Hybrid Probe Rocket in English, has passed its first stage of development by successfully igniting during a test in March in the city of Sile, the home of the ignition testing facility. On hand for the event was Prime Minister Varank, who gave the command to start the countdown to ignition. The rocket remained ignited for the full targeted time limit of 50 seconds and was regarded as a complete success. This time limit is"),strong(" substantial "),HTML("because it is the amount of time a rocket needs to remain ignited in order to break through Earths atmosphere and reach the inner limits of space."),br(),br(),
               strong("What is the main idea of this text?")),
           div(HTML("Shooting for the stars has become an international competition. When we think about space exploration, the first thing that usually comes to mind is NASA, the American Space Agency. Perhaps ESA comes to mind  the European Space Agency. Some may even think of the Chinese Space Agency or the Russian Space Agency. Afterall, these are the most notable countries when it comes to space exploration. However, they are not the only countries who have their sights set on the stars. In recent years, many countries have entered the ring and are now fighting for a place in space. One such country is Turkey."),br(),br(), 
               HTML("Headquartered in Ankara, Turkey, the TUA, or Turkish Space Agency, was founded on December 13, 2018. Its stated, official, mission is to reduce dependence on foreign aerospace science and technology. Their mission also is to increase competitiveness in the international arena and to create a scientific and technological infrastructure while expanding research and development with an aim to develop new technologies. Some of the things they are working on to meet their goals include projects to develop new systems, facilities, tools and equipment related to space and aviation, including satellites, launch vehicles and systems, air vehicles, simulators, and space platforms."),br(),br(),
               HTML("One of the biggest projects the agency is currently working on is a trip to the moon. On February 9th, 2021, President Recep Tayyip Erdogan announced Turkey's National Space Program to the country and the rest of the world and proclaimed that Turkey would reach the moon with an unmanned vehicle in 2023, only 2 short years away. The goal is to reach the moon using only Turkish technology and components. One such component is the original hybrid rocket engine developed by DeltaV Uzay Teknolojileri A.S. The rocket, known as SORS, or Hybrid Probe Rocket in English, has passed its first stage of development by successfully igniting during a test in March in the city of Sile, the home of the ignition testing facility. On hand for the event was Prime Minister Varank, who gave the command to start the countdown to ignition. The rocket remained ignited for the full targeted time limit of 50 seconds and was regarded as a complete success. This time limit is"),strong(" substantial "),HTML("because it is the amount of time a rocket needs to remain ignited in order to break through Earths atmosphere and reach the inner limits of space."),br(),br(),
               strong("In the third paragraph, the word substantial is closest in meaning to")),
           div(strong("Whats Gary mainly talking about?")),
           div(strong("What inspired Gary to work on his area of study?")),
           div(strong("Regarding the future of transportation Gary predicts that ______.")),
           div(strong("The findings of a 2015 study reveals that.....")),
           div(strong("The part of the talk regarding gratitude journaling mainly argues that ...")),
           div(strong("The main purpose of the talk is to -----.")),
           div(strong('Please listen carefully the audio, you can not listen it again',br(),'Length=1 mins'),br(),br(),
               strong("How many rabbits does Fiona have?"), tags$audio(src = "www/1007-1008-1009.m4a",autoplay=TRUE, type = "audio/mp3")),
           div(strong('Following three items will be responded on this audio.',br(),'Please listen carefully, you can not listen it again',br(),'Length=3 mins'),br(),br(),
               strong("Whats Gary mainly talking about?"),br(),"The science behind futurology as a profession",br(),"Differences between futurology and history",br(),"Environmental issues he observed in his hometown",br(),"His experience as a college student in Houston",br(),br(),
               strong("What inspired Gary to work on his area of study?"),br(),"people will have more cars in New York",br(),"cities will use Houston as a model for transportation",br(),"alterations in transportation will take place gradually",br(),"new laws will restrict the way people use public transport",br(),br(),
               strong("Regarding the future of transportation Gary predicts that ______."),br(),"higher levels of gratitude correlate with a stronger sense of fairness",br(),"people feel more grateful to strangers than acquaintances",br(),"gifts of food and clothing generates a deeper sense of gratitude",br(),"gratitude means much more than just a moral emotion", br(),br(),
               tags$audio(src = "www/1001-1002-1003.mpeg",autoplay=TRUE, type = "audio/mp3")),
           div(strong('Following three items will be responded on this audio.',br(),'Please listen carefully, you can not listen it again',br(),'Length=2 mins'),br(),br(),
               strong("The findings of a 2015 study reveals that....."),br(),"higher levels of gratitude correlate with a stronger sense of fairness",br(),"people feel more grateful to strangers than acquaintances",br(),"gifts of food and clothing generates a deeper sense of gratitude ",br(),"noting blessings is a key part of preventive treatment for heart failure ",br(),br(),
               strong("The part of the talk regarding gratitude journaling mainly argues that ..."),br(),"writing down happiness scores significantly increases levels of oxytocin ",br(),"keeping a gratitude journal boosts emotional and physical well-being",br(),"counting your blessings promotes closer social ties within the community",br(),"noting blessings is a key part of preventive treatment for heart failure ",br(),br(),
               strong("The main purpose of the talk is to -----."),br(),"provide a broader definition of gratitude",br(),"categorize gratitude as a basic emotion",br(),"illustrate the effects of feeling grateful",br(),"compare findings of research on gratitude",
               tags$audio(src = "www/1004-1005-1006.mp3",autoplay=TRUE, type = "audio/mp3")))

questions <- sapply(stems, as.character)
df <- data.frame(Question=questions, Option=choices, Type = 'radio', stringsAsFactors = FALSE)

df$Option.1[1]="it has many different shops."
df$Option.2[1]="it is near the main train station. "
df$Option.3[1]="there is a castle in the street."
df$Option.4[1]="there is a place to eat outside."
answers[1]=df$Option.3[1]
df$Option.1[2]="Edinburgh Castle opens for tourists"
df$Option.2[2]="The first train arrives at the station. "
df$Option.3[2]="The soldiers at the castle start work."
df$Option.4[2]="They fire the gun at Edinburgh castle."
answers[2]=df$Option.4[2]

df$Option.1[3]="ideal"
df$Option.2[3]="potential"
df$Option.3[3]="possible"
df$Option.4[3]="real"
answers[3]=df$Option.4[3]

df$Option.1[4]="fairies are the only creatures in kids stories"
df$Option.2[4]="Shakespeare has a lot of effects on modern ideas about fairies."
df$Option.3[4]="today we can still see the importance of fairies in many cultures."
df$Option.4[4]="we can only see the origins of fairies during 18th and 19th centuries."
answers[4]=df$Option.2[4]

df$Option.1[5]="How to Become an Influencer"
df$Option.2[5]="How Influencers Make Money "
df$Option.3[5]="The Impact of Influencers on People"
df$Option.4[5]="Why Companies Need Influencers"
answers[5]=df$Option.3[5]

df$Option.1[6]="Facebook affects our relationships"
df$Option.2[6]="social media connects people worldwide"
df$Option.3[6]="social networking sites influence language"
df$Option.4[6]="networking sites prevents communication"
answers[6]=df$Option.3[6]

df$Option.1[7]="renewable"
df$Option.2[7]="affordable"
df$Option.3[7]="excusable"
df$Option.4[7]="illegible"
answers[7]=df$Option.1[7]
df$Option.1[8]="Growing most of the food locally, not importing, is desirable."
df$Option.2[8]="Aeroponics proves how crops require soil to be produced."
df$Option.3[8]="Food waste in developed countries happens before purchase."
df$Option.4[8]="Consuming more and more resources leads to a better world."
answers[8]=df$Option.1[8]
df$Option.1[9]="cautionary"
df$Option.2[9]="accusatory"
df$Option.3[9]="outraged"
df$Option.4[9]="concerned"
answers[9]=df$Option.1[9]


df$Option.1[10]="raise awareness of how read alouds contributes to early education."
df$Option.2[10]="describe what read alouds include in early education."
df$Option.3[10]="give a reason why read alouds are common early education."
df$Option.4[10]="bring attention to how read alouds can be adapted."
answers[10]=df$Option.4[10]

df$Option.1[11]="benefits of storytelling."
df$Option.2[11]="early years of book love."
df$Option.3[11]="difficulties with digital storytelling."
df$Option.4[11]="shifts in copyright laws."
answers[11]=df$Option.3[11]

df$Option.1[12]="enthusiastic."
df$Option.2[12]="changing"
df$Option.3[12]="neutral"
df$Option.4[12]="practical"
answers[12]=df$Option.4[12]


df$Option.1[13]="one"
df$Option.2[13]="four"
df$Option.3[13]="five"
df$Option.4[13]="seven"
answers[13]=df$Option.2[13]

df$Option.1[14]="The founding of the Turkish Space Agency"
df$Option.2[14]="The competitiveness of the Turkish Space Agency"
df$Option.3[14]="The purpose of the Turkish Space Agency"
df$Option.4[14]="The launching of a Turkish Space Agency rocket"
answers[14]=df$Option.3[14]

df$Option.1[15]="important"
df$Option.2[15]="original"
df$Option.3[15]="excessive"
df$Option.4[15]="understanding"
answers[15]=df$Option.1[15]

df$Option.1[16]="His job and its scientific differences from another job"
df$Option.2[16]="Areas most people work on in his profession around the world"
df$Option.3[16]="The unique characteristics of his job and his own area of interest"
df$Option.4[16]="Garys reasons for choosing his profession and his future plans "
answers[16]=df$Option.3[16]

df$Option.1[17]="The science behind futurology as a profession"
df$Option.2[17]="Differences between futurology and history"
df$Option.3[17]="Environmental issues he observed in his hometown"
df$Option.4[17]="His experience as a college student in Houston  "
answers[17]=df$Option.4[17]

df$Option.1[18]="people will have more cars in New York"
df$Option.2[18]="cities will use Houston as a model for transportation"
df$Option.3[18]="alterations in transportation will take place gradually"
df$Option.4[18]="new laws will restrict the way people use public transport"
answers[18]=df$Option.3[18]

df$Option.1[19]="higher levels of gratitude correlate with a stronger sense of fairness"
df$Option.2[19]="people feel more grateful to strangers than acquaintances"
df$Option.3[19]="gifts of food and clothing generates a deeper sense of gratitude "
df$Option.4[19]="gratitude means much more than just a moral emotion "
answers[19]=df$Option.1[19]

df$Option.1[20]="writing down happiness scores significantly increases levels of oxytocin "
df$Option.2[20]="keeping a gratitude journal boosts emotional and physical well-being"
df$Option.3[20]="counting your blessings promotes closer social ties within the community"
df$Option.4[20]="noting blessings is a key part of preventive treatment for heart failure "
answers[20]=df$Option.2[20]

df$Option.1[21]="provide a broader definition of gratitude"
df$Option.2[21]="categorize gratitude as a basic emotion"
df$Option.3[21]="illustrate the effects of feeling grateful"
df$Option.4[21]="compare findings of research on gratitude"
answers[21]=df$Option.3[21]

df$Option.1[22]="1"
df$Option.2[22]="2"
df$Option.3[22]="3"
df$Option.4[22]="6"
answers[22]=df$Option.3[22]

df$Type[24]="none"
df$Type[23]="none"
storiesDb <- dbConnect(MariaDB(), user='catepne1_yalcinyelpaze', password='ketep.638', dbname='catepne1_CATEP', host='89.252.138.67',port='3306')
df$Answer <- answers
df$Question <- sapply(df$Question, as.character)
shiney=list(title='CATEP',authors="Catep Project Team",itemtimer="Item Time:",timemsg=c("saat","dakika","saniye","ve"),
            firstpage=list(h1('Welcome to the CATEP English Proficiency Testing'),sprintf('The following interface was created by CATEP Project Team')),
            instructions=c("To answer next items please click on the action button below.","Next"),
            begin_message="You are seem to ready answer the items",
            demographics= list(selectInput(inputId = 'University',
                                           label = 'Please select your universtiy',
                                           choices = c('', 'Gaziantep', 'Kahramanmaras Sutcu imam', 'Bilkent', 'other'),
                                           selected = NULL),
                               selectInput(inputId = 'Unit',
                                           label = 'Please select your unit, you study',
                                           choices = c('', 'Foreign Languages School', 'Faculty of Education', 'Faculty of Science&Letters', 'Other'),
                                           selected = NULL),
                               selectInput(inputId = 'Grade',
                                           label = 'Please select your grade',
                                           choices = c('', 'Prep', '1', '2', '3','4'),
                                           selected = NULL),
                               textInput(inputId = 'Name',
                                         label = 'Please write your name&surname')),
            
            demographics_inputIDs=c('University','Unit','Grade','Name'),
            lastpage=function(person){
                return(strong('Dear ', person$demographics$Name,' Congratulations you have finished the test session.',
                              br(),br(),"Your Reading Ability Theta is ",h2(person$thetas[1]),
                              br(),br(),"Your CEFR Reading Level ",h2("B2"),
                              br(),br(),"Your Listening Ability Theta is ",h2(person$thetas[2]),
                              br(),br(),"Your CEFR Listening Level ",h2("B1"),
                              br(),br(),"You answered ", sum(person$responses[1:ritems],na.rm =TRUE)," of ",ritems, "reading items correctly ",
                              br(),br(),"You answered ", sum(person$responses[ritems+1:nitems-lnitems+1],na.rm =TRUE)," of ",litems, "listening items correctly "))
                
                # br(), sum(person$item_time,na.rm = T)," seconds"))
            })
# theme=shinytheme('united'))

server<- function(person){
    storiesDb <- dbConnect(MariaDB(), user='catepne1_yalcinyelpaze', password='ketep.638', dbname='catepne1_CATEP', host='89.252.138.67',port='3306')
    #dbListTables(storiesDb)
    dbSendQuery(storiesDb,paste(sprintf("insert into USERS(UNIVERSTIY, STUDY_UNIT, GRADE, NAME_SURNAME) values('%s', '%s', '%s', '%s','%s','%s')",person$demographics$University,person$demographics$Unit,person$demographics$Grade,person$demographics$Name,sum(person$responses[1:ritems],na.rm =TRUE),sum(person$responses[ritems+1:nitems-lnitems+1],na.rm =TRUE))))
    dbDisconnect(storiesDb)
}


customNextItem <- function(design, person, test){
    #browser()
    
}


customNextItem <- function(design, person, test){
    #storiesDb <- dbConnect(MariaDB(), user='catepne1_yalcinyelpaze', password='ketep.638', dbname='catepne1_CATEP', host='89.252.138.67',port='3306')
    #dbListTables(storiesDb)
    dbSendQuery(storiesDb,paste(sprintf("insert into USERS(UNIVERSTIY, STUDY_UNIT, GRADE, NAME_SURNAME, READABILITY, LISTENABILITY,READTRUE,LISTENTRUE) values('%s', '%s', '%s', '%s','%s','%s','%s','%s')",person$demographics$University,person$demographics$Unit,person$demographics$Grade,person$demographics$Name,person$thetas[1],person$thetas[2],sum(person$responses[1:ritems],na.rm =TRUE),sum(person$responses[ritems+1:nitems-lnitems+1],na.rm =TRUE))))
    #dbDisconnect(storiesDb)
    
    
    block1 <- 1:ritems
    block2 <- (ritems+1):(nitems)
    total_answered <- sum(!is.na(extract.mirtCAT(person, 'items_answered')))
    
    
    if(total_answered < ritems){ #first 15 items block1
        block <- block1
    } else     block <- block2
    ret <- findNextItem(person=person, design=design, test=test, 
                        subset=block, criteria = 'Arule')
    ret
}
catep_MI <- mirtCAT(df, mod, criteria = 'Arule', 
                    design = list(customNextItem=customNextItem, min_SEM=0,
                                  constraints=list(not_scored=c(24,23),ordered=c(1,2),ordered=c(7,8,9), ordered=c(10,11,12), ordered=c(13:15), ordered=c(23,16,17,18), ordered=c(24,19,20,21))),shinyGUI = shiney)



dbDisconnect(storiesDb)

rsconnect::deployApp('C:/Users/GTUSR0313/Desktop/New_folder/catep')

plot(catep_MI)
summary(catep_MI)