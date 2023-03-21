package com.polus.fibicomp.award.vo;

import java.util.ArrayList;
import java.util.List;

public class AwardSummaryVO {

	private String awardNumber;

	private AwardSummaryDetailsVO activeAward;

	private List<AwardSummaryDetailsVO> pendingAwards;

	public AwardSummaryVO() {
		pendingAwards = new ArrayList<>();
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public AwardSummaryDetailsVO getActiveAward() {
		return activeAward;
	}

	public void setActiveAward(AwardSummaryDetailsVO activeAward) {
		this.activeAward = activeAward;
	}

	public List<AwardSummaryDetailsVO> getPendingAwards() {
		return pendingAwards;
	}

	public void setPendingAwards(List<AwardSummaryDetailsVO> pendingAwards) {
		this.pendingAwards = pendingAwards;
	}

}
