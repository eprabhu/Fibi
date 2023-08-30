package com.polus.fibicomp.coi.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TravelDisclosureActionLogDto {

	private String actionTypeCode;
	private Integer travelDisclosureId;
	private Integer travelNumber;
	private String oldAdmin;
	private String newAdmin;
	private String comment;
	private String message;
	private String riskCategoryCode;
	private String riskCategory;
	private String newRiskCategoryCode;
	private String newRiskCategory;
	private String oldDisclosureStatus;
	private String newDisclosureStatus;

}
