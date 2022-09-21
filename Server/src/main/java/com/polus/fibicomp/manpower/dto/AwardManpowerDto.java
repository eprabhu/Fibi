package com.polus.fibicomp.manpower.dto;

import java.util.List;
import java.util.Map;

import com.polus.fibicomp.manpower.pojo.AwardManpower;

public class AwardManpowerDto {

	private Map<String, List<AwardManpower>> awardManpowerDetails;

	public Map<String, List<AwardManpower>> getAwardManpowerDetails() {
		return awardManpowerDetails;
	}

	public void setAwardManpowerDetails(Map<String, List<AwardManpower>> awardManpowerDetails) {
		this.awardManpowerDetails = awardManpowerDetails;
	}


}
