package com.polus.fibicomp.vo;

public final class EndPoints extends EndPointDetails{

	public EndPoints(String tableName, String primaryColumn, String descriptionColumn) {
		super(tableName, primaryColumn, descriptionColumn);
	}

	public static final EndPoints GRANT_CALL = new  EndPoints("GRANT_CALL_HEADER", "GRANT_HEADER_ID", "NAME");

	public static final EndPoints SPONSOR = new  EndPoints("SPONSOR", "SPONSOR_CODE", "SPONSOR_NAME");

	public static final EndPoints PERSON = new EndPoints("PERSON", "PERSON_ID", "FULL_NAME");

	public static final EndPoints AWARD = new EndPoints("AWARD", "AWARD_ID", "TITLE");

	public static final EndPoints DEV_PROPOSAL = new EndPoints("EPS_PROPOSAL", "PROPOSAL_ID", "TITLE");

	public static final EndPoints PROPOSAL = new EndPoints("PROPOSAL", "PROPOSAL_ID", "TITLE");

	public static final EndPoints UNIT = new EndPoints("UNIT", "UNIT_NUMBER", "UNIT_NAME");

	public static final EndPoints ORGANIZATION = new EndPoints("ORGANIZATION", "ORGANIZATION_ID", "ORGANIZATION_NAME");

	public static final EndPoints COUNTRY = new EndPoints("COUNTRY", "COUNTRY_CODE", "COUNTRY_NAME");

	public static final EndPoints PROFIT_CENTER = new EndPoints("SAP_PROFIT_CENTER", "PROFIT_CENTER_CODE", "PROFIT_CENTER_NAME");

	public static final EndPoints GRANT_CODE = new EndPoints("SAP_GRANT_CODE", "GRANT_CODE", "GRANT_CODE_NAME");

	public static final EndPoints FUND_CENTER = new EndPoints("SAP_FUND_CENTER", "FUND_CENTER_CODE", "FUND_CENTER_NAME");

	public static final EndPoints COST_CENTER = new EndPoints("SAP_COST_CENTER", "COST_CENTER_CODE", "COST_CENTER_NAME");

	public static final EndPoints KEYWORD = new EndPoints("science_keyword", "SCIENCE_KEYWORD_CODE", "DESCRIPTION");

	public static final EndPoints FIBIROLE = new EndPoints("role", "ROLE_ID", "ROLE_NAME");

	public static EndPoints getEndPointDetails(String type) {
		switch (type) {
		case "grantcall_elastic": return GRANT_CALL;
		case "sponsorName" : return SPONSOR;
		case "fibiPerson":  return PERSON;
		case "awardfibi":  return AWARD;
		case "fibiproposal":  return DEV_PROPOSAL;
		case "instituteproposal":  return PROPOSAL;
		case "unitName":  return UNIT;
		case "fibiOrganization":  return ORGANIZATION;
		case "fibiCountry":  return COUNTRY;
		case "fibiDepartment":  return UNIT;
		case "grantCodeName":  return GRANT_CODE;
		case "costCenterName":  return COST_CENTER;
		case "fundCenterName":  return FUND_CENTER;
		case "profitCenterName":  return PROFIT_CENTER;
		case "keyWords": return KEYWORD;
		case "fibiRole": return FIBIROLE;
		default:
			return null;
		}	
	}

}
