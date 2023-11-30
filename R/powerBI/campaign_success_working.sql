select * from alltraffic;
select * from campaignnewsletters;
select * from targetcampaign;

ALTER TABLE targetcampaign ADD COLUMN campaignID varchar(100) AFTER uid;

SET SQL_SAFE_UPDATES=1;
UPDATE alltraffic SET campaignID = '2023_CoalvGas';

