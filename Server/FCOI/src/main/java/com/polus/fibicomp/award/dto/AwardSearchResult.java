package com.polus.fibicomp.award.dto;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;

public class AwardSearchResult {

	private String awardNumber;

	private Integer awardId;

	private String title;

	private String sponsorAwardNumber;

	private String sponsorName;

	private String unitName;

	private String accountNumber;

	private String principalInvestigator;

	public AwardSearchResult(Award award, String sponsorName, String unitName) {
		this.awardNumber = award.getAwardNumber();
		this.awardId = award.getAwardId();
		this.title = award.getTitle();
		this.sponsorAwardNumber = award.getSponsorAwardNumber();
		this.sponsorName = sponsorName;
		this.unitName = unitName;
		this.accountNumber = award.getAccountNumber();
		if (award.getAwardPersons() != null && !award.getAwardPersons().isEmpty()) {
			for (AwardPerson person : award.getAwardPersons()) {
				if (person != null && person.getIsPi().equals(Boolean.TRUE)) {
					this.principalInvestigator = person.getFullName();
					break;
				}
			}
		}
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getPrincipalInvestigator() {
		return principalInvestigator;
	}

	public void setPrincipalInvestigator(String principalInvestigator) {
		this.principalInvestigator = principalInvestigator;
	}

}
