package com.polus.fibicomp.award.dto;

public class SponsorTermCodeData {
	
	private String code;
	
	private String description;
	
	private Integer awardSponsorTermId;

	private String acType;
	
	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
	
	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getAwardSponsorTermId() {
		return awardSponsorTermId;
	}

	public void setAwardSponsorTermId(Integer awardSponsorTermId) {
		this.awardSponsorTermId = awardSponsorTermId;
	}

}
