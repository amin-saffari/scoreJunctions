I should have a replacement for anne''s perl script as I don''t understand the language! Also it''s
really good to not have perl wraper in your program.

These and also some other advantages like:
1) Using folding feature of protein coding and none coding (Christian library [ViennaRNA])



I should have at least 3 parts:
1) Calculating the conservation score for Splice site 
2) Run RNAcode to see if the transcript is conserved

Calculating splice site conservation is only using haarz bed format which only
provides you the coordinate of splice site and nothing more. This means I do not 
have the coordinate of my transcript to see the conservation of its structure (RNAcode).

These are mostly format handling than anything else so in the near future I have to come up
with a plan to reduce this load!

What do we have:
1) lib-BiobaseMAF
2) biohazard
3) scoreJunction

Given the position in certain chromosome, lib-Biobsae must provided me the list of all sequences 
there. I definitly need to use it in SScore for seeing the other species sequences and 
also in TScore because when I extracted the coordinate of the particular read I need to know 
what other species have on those positions!

Also I need to extract the coordinate of the splited read which I think this by itself should
be already solved by running biohazard library
