# Passer_analysis
Lonzo Ball Effect project intends to identify players that are effective at facilitating scoring opportunities for his teammates.

This project was completed as part of a job application. The data is private so instead I am just providing the data dictionary.

-------------------------------------------------------------------------------

‘Lonzo Ball’ Effect: Making teammates better by putting them in scoring positions

Since we’ve been hearing a bunch about Lonzo Ball and his effectiveness as a passer, his ability to find open teammates, and his ability to make everyone around him better I’ve decided to focus on just that. The code investigates individual players primarily as passers.

Question: Can we quantify effectiveness as a passer?
•	Passing to players that can shoot, where they can shoot 
o	Measure shooter’s FG percentage in area
•	Passing to players where they don’t need to create shot (take < 3 dribbles)
•	Passing to open players (nearest defender distance)

Visualizations:
•	Plot
o	Where passes are from
o	If shot is made or missed (Open circle/X)
o	Less than 3 dribbles (green), 3 or more dribbles (orange)
•	Data Table
o	FG% of all shooters when passed to from specified player from all areas on the court
•	Histograms
o	NDD
o	Dribbles

Why is this important?
Coaching & Strategy
•	Identifying inefficiencies in offense
o	Ball handlers can understand that passing to i.e Blake Griffin on the right wing results in a made basket only 30% of the time and be coached to look for other options
o	‘Blake Griffin’ can be coached on more effective offense from right wing
o	Ex. Passer consistently passes to players that have to dribble >5 times for shot
o	Ex. Passer is passing into traffic/puts shooter in bad position: NDD < 5ft, no dribble on shot, late in shotclock
•	Game planning for opponents
o	Deny Chris Paul from finding Harden on wing as that is more effective offense than CP as a pull up shooter
Personnel Adjustments
•	Quantify and identify if player can score based on where they catch the ball 
o	Seek players out that enable us to have a scoring presence from different spots on the floor
o	Ex. Passer consistently finds players on wing where NDD > 10ft, except percentage of shots made is low


Method
Data Cleanup 
I cleaned up the data a bit, merged data so that the shot data included player and team name so that I could make some sense of it. 

Functions
1.	getPlayer() creates a dataframe of all records of the specified player as a passer
2.	createAreas() adds columns to the dataframe of the area where the pass is made from and to
a.	Areas: Top of the key, high post, low post, right/left wings, right/left corners
b.	Allows me to see where passes are from and to in basketball terms
3.	getShooterFGinArea() creates a dataframe of all shooters for the area in which the received the pass from
4.	getFG_pass_from() computes the FG% of all shooters that the player has passed to in each of the 7 offensive areas

Visualization
I initially wanted to graph all of the passes, plotting where they passed from, where they passed to, if the shot was made, and color code based on NDD of the resulting shot. This proved pretty difficult in R, and I briefly explored creating the visualization in D3 before deciding that the most valuable information would be the actual analysis and that for the time being I will just present the information via tables. 

Analysis

Visual Analysis

Let’s look at Ben McLemore in this dataset:
 
 [See Lonzo_Ball_Effect.docx for missing image]

The plot above shows everywhere Ben passed from (not where shots are), whether the resulting shot was a make or miss (O or X) and if the ‘shooter’ made a basketball move or more or less caught and shot (<3 dribbles). This plot points out the obvious: Ben operates mainly on the wings, occasionally from the high post. One thing it points out is that usually when Ben passes from the baseline the shooter takes 3 or more dribbles for the shot. This tells me this is not necessarily an open shot for the shooter and that they have to create a shot for themselves. 

  [See Lonzo_Ball_Effect.docx for missing image]


A simple histogram of the distance of the nearest defender doesn’t show any conclusive evidence other than what we already know almost all shots are contested or wide open.

 
Again this histogram doesn’t show much other than most shots (in this dataset) are taken off of < 2 dribbles. 

Numerical Analysis
Once we had our player and the areas I was able to quantify for each pass a player makes, what is the FG% of the shooter when he receives the ball in that area. For example, if we look at Ben McLemore in the data we can then get a new dataset that shows when he passes to Demarcus in the high post 63% of the times that results in a bucket versus 33% on the left wing. This gets created for all shooters in all areas of the court in which they received a pass from the player we are investigating.

Additionally, we have data on the FG% of all shots that result when Ben McLemore passes from each area:

Top of the key	54.7%
High Post	54.4%
Low Post	63.8%
Right Wing	49.4%
Left Wing	54.5%
Right Corner	57.1%
Left Corner	53.3%


