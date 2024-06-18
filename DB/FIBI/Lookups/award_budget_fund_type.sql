INSERT INTO AWARD_BUDGET_FUND_TYPE (FUND_TYPE_CODE,FUND_TYPE,DESCRIPTION,IS_DEFAULT,IS_COST_SHARE_ENABLE,REFERENCE_COLUMN,UPDATE_USER,UPDATE_TIMESTAMP) VALUES ('A','Total Project Cost','Anticipated Distributable + Cost Sharing amount which can be included in the Total Budget','N','Y','ANT_DISTRIBUTABLE_AMOUNT','fibi_admin',NOW());
INSERT INTO AWARD_BUDGET_FUND_TYPE (FUND_TYPE_CODE,FUND_TYPE,DESCRIPTION,IS_DEFAULT,IS_COST_SHARE_ENABLE,REFERENCE_COLUMN,UPDATE_USER,UPDATE_TIMESTAMP) VALUES ('O','Total Obligation Amount','Obligated Distributable','Y','N','OBLI_DISTRIBUTABLE_AMOUNT','fibi_admin',NOW());
