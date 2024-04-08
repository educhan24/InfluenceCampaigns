select * from socialtraffic;
select * from campaignnewsletters;
select * from geographytraffic; 
select * from alltraffic;
select * from targetcampaign;
select * from contentsummary;
select * from donations where campaignID = 'COP28';
select * from campaignposts;
select * from targetcampaign where campaignID = 'COP28'; and EngagementType = 'Event' and Audience1 = 'N/A';

select *  from sfcampaigns where Status = 'Attended';
select * FROM alltraffic where campaignID ='COP28';

ALTER TABLE targetcampaign ADD COLUMN campaignID varchar(100) AFTER uid;

delete from alltraffic where campaignID = 'COP28';
SET SQL_SAFE_UPDATES=0;
UPDATE alltraffic SET campaignID = '2023_CoalvGas';
Update sfcampaigns SET CampaignName = 'GCF_CCAF_Event_WMBC Transition Finance' where CampaignName = '_GCF_CCAF_Event_WMBC Transition Finance';

